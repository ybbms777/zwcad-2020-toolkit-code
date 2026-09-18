using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;
using ZwSoft.ZwCAD.DatabaseServices;
using ZwSoft.ZwCAD.Runtime;
using CadApp = ZwSoft.ZwCAD.ApplicationServices.Application;

// .NET Framework 4, compiled against the installed ZWCAD 2020 assemblies.
// No background hook: input capture exists only during the Lisp call.
public class ZWKitMouse
{
    // A real Windows cursor follows the mouse even while CAD mouse messages are filtered.
    // Its hotspot is exactly the white center pixel, not the edge of the cursor bitmap.
    sealed class PrecisionCursor : IDisposable
    {
        [StructLayout(LayoutKind.Sequential)] struct ICONINFO
        { public bool Icon; public uint X, Y; public IntPtr Mask, Color; }
        [DllImport("user32.dll")] static extern bool GetIconInfo(IntPtr icon, out ICONINFO info);
        [DllImport("user32.dll")] static extern IntPtr CreateIconIndirect(ref ICONINFO info);
        [DllImport("user32.dll")] static extern bool DestroyIcon(IntPtr icon);
        [DllImport("user32.dll")] static extern bool DestroyCursor(IntPtr cursor);
        [DllImport("user32.dll")] static extern IntPtr SetCursor(IntPtr cursor);
        [DllImport("user32.dll")] static extern int ShowCursor(bool show);
        [StructLayout(LayoutKind.Sequential)] struct CURSORINFO
        { public int Size, Flags; public IntPtr Handle; public Point Position; }
        [DllImport("user32.dll")] static extern bool GetCursorInfo(ref CURSORINFO info);
        [DllImport("gdi32.dll")] static extern bool DeleteObject(IntPtr obj);
        IntPtr handle, previous;
        int shows;
        public PrecisionCursor()
        {
            using (Bitmap bitmap = MakeBitmap())
            {
                IntPtr icon = bitmap.GetHicon();
                ICONINFO info;
                try
                {
                    if (!GetIconInfo(icon, out info)) throw new InvalidOperationException("无法创建准星。");
                    try
                    {
                        info.Icon = false; info.X = 16; info.Y = 16;
                        handle = CreateIconIndirect(ref info);
                    }
                    finally { DeleteObject(info.Mask); DeleteObject(info.Color); }
                }
                finally { DestroyIcon(icon); }
            }
            if (handle == IntPtr.Zero) throw new InvalidOperationException("无法创建鼠标准星。");
            previous = SetCursor(handle);
            // Balance only increments made by this command; do not alter persistent CAD settings.
            for (int i = 0; i < 64; i++) { shows++; if (ShowCursor(true) >= 0) break; }
        }
        internal static Bitmap MakeBitmap()
        {
            Bitmap b = new Bitmap(33, 33, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            using (Graphics g = Graphics.FromImage(b))
            using (Pen black = new Pen(Color.Black, 3))
            using (Pen white = new Pen(Color.White, 1))
            {
                g.Clear(Color.Transparent);
                g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.None;
                foreach (Pen p in new Pen[] { black, white })
                {
                    g.DrawLine(p, 16, 1, 16, 10); g.DrawLine(p, 16, 22, 16, 31);
                    g.DrawLine(p, 1, 16, 10, 16); g.DrawLine(p, 22, 16, 31, 16);
                    g.DrawRectangle(p, 13, 13, 6, 6);
                }
                g.FillRectangle(Brushes.Black, 15, 15, 3, 3);
                g.FillRectangle(Brushes.White, 16, 16, 1, 1);
            }
            return b;
        }
        public void Refresh() { SetCursor(handle); }
        internal bool Check()
        {
            ICONINFO icon;
            if (!GetIconInfo(handle, out icon)) return false;
            bool hotspot = !icon.Icon && icon.X == 16 && icon.Y == 16;
            DeleteObject(icon.Mask); DeleteObject(icon.Color);
            CURSORINFO state = new CURSORINFO(); state.Size = Marshal.SizeOf(typeof(CURSORINFO));
            return hotspot && GetCursorInfo(ref state) && (state.Flags & 1) != 0;
        }
        internal static int VisibleFlags()
        {
            CURSORINFO state = new CURSORINFO(); state.Size = Marshal.SizeOf(typeof(CURSORINFO));
            return GetCursorInfo(ref state) ? state.Flags : -1;
        }
        public void Dispose()
        {
            SetCursor(previous);
            for (int i = 0; i < shows; i++) ShowCursor(false);
            if (handle != IntPtr.Zero) { DestroyCursor(handle); handle = IntPtr.Zero; }
        }
    }
    [StructLayout(LayoutKind.Sequential)] struct RECT { public int L,T,R,B; }
    [DllImport("user32.dll")] static extern short GetAsyncKeyState(int key);
    [DllImport("user32.dll")] static extern IntPtr GetForegroundWindow();
    [DllImport("user32.dll")] static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
    [DllImport("user32.dll")] static extern IntPtr WindowFromPoint(Point p);
    [DllImport("user32.dll")] static extern IntPtr GetParent(IntPtr h);
    [DllImport("user32.dll")] static extern bool GetClientRect(IntPtr h, out RECT r);
    [DllImport("user32.dll")] static extern bool ClientToScreen(IntPtr h, ref Point p);
    [DllImport("user32.dll")] static extern bool InvalidateRect(IntPtr h, IntPtr r, bool erase);
    delegate bool EnumWindow(IntPtr h, IntPtr data);
    [DllImport("user32.dll")] static extern bool EnumChildWindows(IntPtr h, EnumWindow callback, IntPtr data);
    [DllImport("user32.dll")] static extern IntPtr SendMessage(IntPtr h, uint msg, IntPtr w, IntPtr l);
    [DllImport("user32.dll")] static extern bool RedrawWindow(IntPtr h, IntPtr r, IntPtr region, uint flags);
    static IntPtr FindCanvas(int width, int height)
    {
        IntPtr found = IntPtr.Zero;
        EnumChildWindows(CadApp.DocumentManager.MdiActiveDocument.Window.Handle, delegate(IntPtr h, IntPtr data)
        {
            RECT r;
            if (GetClientRect(h, out r) && Math.Abs(r.R-r.L-width)<=2 && Math.Abs(r.B-r.T-height)<=2)
                found = h;
            return true;
        }, IntPtr.Zero);
        return found;
    }
    static IntPtr PackedPoint(int x, int y)
    { return new IntPtr(unchecked((int)(((uint)y & 65535)<<16 | ((uint)x & 65535)))); }
    static void ClearNativeCrosshair(IntPtr h)
    {
        // Clear CAD's software crosshair before showing our hardware cursor.
        // No physical pointer movement or button events are generated.
        SendMessage(h, 0x200, IntPtr.Zero, PackedPoint(-32000,-32000));
        SendMessage(h, 0x2A3, IntPtr.Zero, IntPtr.Zero); // WM_MOUSELEAVE
        RedrawWindow(h, IntPtr.Zero, IntPtr.Zero, 0x1 | 0x20 | 0x100);
    }
    static void RestoreNativeCrosshair(IntPtr h)
    {
        Point origin = new Point(); ClientToScreen(h, ref origin);
        Point p = Cursor.Position; RECT r;
        if (GetClientRect(h,out r) && p.X>=origin.X && p.Y>=origin.Y &&
            p.X<origin.X+r.R && p.Y<origin.Y+r.B)
            SendMessage(h, 0x200, IntPtr.Zero, PackedPoint(p.X-origin.X,p.Y-origin.Y));
        InvalidateRect(h,IntPtr.Zero,false);
    }
    static bool Down(int k) { return (GetAsyncKeyState(k) & 0x8000) != 0; }
    static bool OurWindow(IntPtr h)
    {
        uint pid; GetWindowThreadProcessId(h, out pid);
        return pid == (uint)Process.GetCurrentProcess().Id;
    }
    sealed class InputFilter : IMessageFilter
    {
        public bool Cancel, Undo;
        public int Wheel;
        public bool PreFilterMessage(ref Message m)
        {
            if (m.Msg == 0x20A) Wheel += unchecked((short)((m.WParam.ToInt64() >> 16) & 65535));
            if (m.Msg == 0x100 || m.Msg == 0x104)
            {
                int k = m.WParam.ToInt32();
                if (k == 27 || k == 13 || k == 32) Cancel = true;
                if (k == 85) Undo = true;
            }
            return (m.Msg >= 0x200 && m.Msg <= 0x20E) ||
                   (m.Msg >= 0x100 && m.Msg <= 0x109);
        }
    }
    static ResultBuffer Status(string s)
    { return new ResultBuffer(new TypedValue((int)LispDataType.Text, s)); }

    static void ZoomAt(Point p, int width, int height, int wheel)
    {
        var ed = CadApp.DocumentManager.MdiActiveDocument.Editor;
        using (var view = ed.GetCurrentView())
        {
            double scale = Math.Pow(1.2, -wheel/120.0);
            double effectiveWidth = view.Height * width / height; // ZWCAD 2020 may report Width=1.
            double nw = effectiveWidth * scale, nh = view.Height * scale;
            if (nw < 1e-9 || nh < 1e-9 || nw > 1e15 || nh > 1e15) return;
            double ox = ((double)p.X/width-0.5)*effectiveWidth;
            double oy = (0.5-(double)p.Y/height)*view.Height;
            view.CenterPoint = new ZwSoft.ZwCAD.Geometry.Point2d(
                view.CenterPoint.X+ox*(1-scale), view.CenterPoint.Y+oy*(1-scale));
            var transform = ZwSoft.ZwCAD.Geometry.Matrix3d.PlaneToWorld(view.ViewDirection);
            transform = ZwSoft.ZwCAD.Geometry.Matrix3d.Displacement(view.Target-ZwSoft.ZwCAD.Geometry.Point3d.Origin)*transform;
            transform = ZwSoft.ZwCAD.Geometry.Matrix3d.Rotation(-view.ViewTwist,view.ViewDirection,view.Target)*transform;
            var center = new ZwSoft.ZwCAD.Geometry.Point3d(view.CenterPoint.X,view.CenterPoint.Y,0).TransformBy(transform);
            dynamic acad = CadApp.ZcadApplication;
            acad.ZoomCenter(new double[] { center.X, center.Y, center.Z }, nh);
        }
    }
    static void PanBy(int dx, int dy, int width, int height)
    {
        var ed = CadApp.DocumentManager.MdiActiveDocument.Editor;
        using (var view = ed.GetCurrentView())
        {
            view.CenterPoint = new ZwSoft.ZwCAD.Geometry.Point2d(
                view.CenterPoint.X-dx*view.Height/height,
                view.CenterPoint.Y+dy*view.Height/height);
                        ed.SetCurrentView(view);
        }
    }

    [LispFunction("ZWK_NAVCHECK_101")]
    public static ResultBuffer NavCheck(ResultBuffer args)
    {
        var ed=CadApp.DocumentManager.MdiActiveDocument.Editor;
        using(var original=ed.GetCurrentView())
        {
            bool zoom=false, pan=false;
            try
            {
                var size=(ZwSoft.ZwCAD.Geometry.Point2d)CadApp.GetSystemVariable("SCREENSIZE"); int w=(int)size.X,h=(int)size.Y; var p=new Point(w*2/3,h/3); double effectiveWidth=original.Height*w/h;
                double ax=original.CenterPoint.X+((double)p.X/w-.5)*effectiveWidth;
                double ay=original.CenterPoint.Y+(.5-(double)p.Y/h)*original.Height;
                ZoomAt(p,w,h,120);
                using(var v=ed.GetCurrentView())
                {
                    double bx=v.CenterPoint.X+((double)p.X/w-.5)*(v.Height*w/h);
                    double by=v.CenterPoint.Y+(.5-(double)p.Y/h)*v.Height;
                    zoom=Math.Abs(ax-bx)<Math.Max(1,original.Width)*1e-8 &&
                         Math.Abs(ay-by)<Math.Max(1,original.Height)*1e-8 && v.Height<original.Height;
                    double x=v.CenterPoint.X-45*v.Height/h, y=v.CenterPoint.Y+30*v.Height/h;
                    PanBy(45,30,w,h);
                    using(var moved=ed.GetCurrentView())
                        pan=Math.Abs(moved.CenterPoint.X-x)<Math.Max(1,v.Width)*1e-8 &&
                            Math.Abs(moved.CenterPoint.Y-y)<Math.Max(1,v.Height)*1e-8;
                }
                return Status("ZOOM_ANCHOR="+zoom+";PAN="+pan);
            }
            finally { ed.SetCurrentView(original); }
        }
    }

    [LispFunction("ZWK_CURSORCHECK_101")]
    public static ResultBuffer CursorCheck(ResultBuffer args)
    {
        int before = PrecisionCursor.VisibleFlags();
        bool ok = false;
        var sz = (ZwSoft.ZwCAD.Geometry.Point2d)CadApp.GetSystemVariable("SCREENSIZE");
        IntPtr h = FindCanvas((int)sz.X,(int)sz.Y);
        if (h == IntPtr.Zero) return Status("CANVAS_NOT_FOUND");
        try
        {
            ClearNativeCrosshair(h);
            using (var c = new PrecisionCursor()) { c.Refresh(); ok = c.Check(); }
        }
        finally { RestoreNativeCrosshair(h); }
        return Status("VISIBLE_AND_CENTERED=" + ok + ";RESTORED=" + (before == PrecisionCursor.VisibleFlags()));
    }

    [LispFunction("ZWK_MODULE_101")]
    public static ResultBuffer ModuleCheck(ResultBuffer args) { return Status("READY"); }

    [LispFunction("ZWK_CAPTURE_101")]
    public static ResultBuffer Capture(ResultBuffer args)
    {
        TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
        if (a.Length != 2) return Status("ERROR");
        int width = Convert.ToInt32(a[0].Value), height = Convert.ToInt32(a[1].Value);
        if (width < 20 || height < 20) return Status("ERROR");
        var filter = new InputFilter();
        IntPtr canvas = IntPtr.Zero;
        Graphics graphics = null;
        Pen pen = null;
        var pts = new List<Point>();
        Point origin = new Point();
        bool drawing = false, ready = !Down(1), strokeTrim = false;
        bool panning = false;
        Point panLast = new Point();
        DateTime started = DateTime.UtcNow;
        Application.AddMessageFilter(filter);
        PrecisionCursor precision = null;
        IntPtr nativeCanvas = IntPtr.Zero;
        try
        {
            nativeCanvas = FindCanvas(width,height);
            if (nativeCanvas == IntPtr.Zero)
            {
                CadApp.DocumentManager.MdiActiveDocument.Editor.WriteMessage("\n未找到独立绘图区，已停止以避免准星残留。请使用模型空间单视口。");
                return Status("ERROR");
            }
            ClearNativeCrosshair(nativeCanvas);
            precision = new PrecisionCursor();
            while (true)
            {
                Application.DoEvents();
                precision.Refresh();
                if (filter.Cancel || Down(27) || Down(2)) return Status("CANCEL");
                if (filter.Undo) return Status(drawing ? "CANCEL" : "UNDO");
                if (!OurWindow(GetForegroundWindow())) return Status("CANCEL");
                if ((DateTime.UtcNow - started).TotalMinutes > 10) return Status("CANCEL");
                bool held = Down(1);
                bool middle = Down(4);
                Point screen = Cursor.Position;
                Point navOrigin = new Point(); ClientToScreen(nativeCanvas,ref navOrigin);
                Point navPoint = new Point(screen.X-navOrigin.X,screen.Y-navOrigin.Y);
                bool inside = navPoint.X>=0 && navPoint.Y>=0 && navPoint.X<width && navPoint.Y<height;
                int wheel = filter.Wheel; filter.Wheel = 0;
                if (middle || wheel != 0)
                {
                    // Never interpret points collected before a view change in the new view.
                    if (drawing)
                    {
                        pts.Clear(); drawing=false; ready=false; strokeTrim=false;
                        if (graphics!=null) { graphics.Dispose(); graphics=null; }
                        if (pen!=null) { pen.Dispose(); pen=null; }
                        InvalidateRect(nativeCanvas,IntPtr.Zero,false);
                        CadApp.DocumentManager.MdiActiveDocument.Editor.WriteMessage("\n已取消尚未完成的延伸轨迹，正在调整视图。松开左键后可继续延伸。");
                    }
                    if (inside)
                    {
                        bool viewChanged=wheel!=0;
                        if (wheel!=0) ZoomAt(navPoint,width,height,wheel);
                        if (middle)
                        {
                            if (panning && (screen.X!=panLast.X || screen.Y!=panLast.Y))
                            {
                                PanBy(screen.X-panLast.X,screen.Y-panLast.Y,width,height);
                                viewChanged=true;
                            }
                            panLast=screen; panning=true;
                        }
                        else panning=false;
                        if (viewChanged) ClearNativeCrosshair(nativeCanvas);
                    }
                    else panning=false;
                    precision.Refresh();
                    // A stroke cannot start while either navigation gesture is active.
                    if (held) ready=false;
                    Thread.Sleep(8);
                    continue;
                }
                panning=false;
                if (!held) ready = true;
                if (!drawing && held && ready)
                {
                    // Preserve Shift from the start of the stroke.  ZWCAD can
                    // clear the key state when the left button is released.
                    strokeTrim = Down(16);
                    // Find the drawing child window, using SCREENSIZE supplied by Lisp.
                    IntPtr h = WindowFromPoint(screen);
                    for (int i = 0; h != IntPtr.Zero && i < 8 && OurWindow(h); i++, h = GetParent(h))
                    {
                        RECT r;
                        if (GetClientRect(h, out r) && Math.Abs(r.R-r.L-width)<=2 && Math.Abs(r.B-r.T-height)<=2)
                        { canvas = h; width=r.R-r.L; height=r.B-r.T; break; }
                    }
                    if (canvas == IntPtr.Zero)
                    {
                        CadApp.DocumentManager.MdiActiveDocument.Editor.WriteMessage(
                            "\n未识别到匹配的模型绘图区。请在绘图区内拖动；若仍失败，请提供命令行截图。");
                        return Status("ERROR");
                    }
                    origin = new Point(0,0); ClientToScreen(canvas, ref origin);
                    graphics = Graphics.FromHwnd(canvas); pen = new Pen(Color.Cyan, 1);
                    drawing = true;
                }
                if (drawing)
                {
                    // Holding Shift at any point before release changes this
                    // entire stroke to trim, matching AutoCAD's Shift behavior.
                    if (Down(16)) strokeTrim = true;
                    Point p = new Point(screen.X-origin.X, screen.Y-origin.Y);
                    // Leaving the drawing cancels the entire stroke; never bridge across UI.
                    if (p.X<0 || p.Y<0 || p.X>=width || p.Y>=height) return Status("CANCEL");
                    Point last = pts.Count == 0 ? p : pts[pts.Count-1];
                    int dx=p.X-last.X, dy=p.Y-last.Y;
                    if (pts.Count==0 || dx*dx+dy*dy>=9 || (!held && !p.Equals(last)))
                    {
                        pen.Color = strokeTrim ? Color.OrangeRed : Color.Cyan;
                        if (pts.Count>0) graphics.DrawLine(pen,last,p);
                        pts.Add(p);
                    }
                    if (pts.Count>2048)
                    {
                        CadApp.DocumentManager.MdiActiveDocument.Editor.WriteMessage("\n本次轨迹过长，已取消；请分几笔延伸。");
                        return Status("CANCEL");
                    }
                    if (!held)
                    {
                        if (pts.Count==0) return Status("EMPTY");
                        var values=new List<TypedValue>();
                        values.Add(new TypedValue((int)LispDataType.ListBegin));
                        if (strokeTrim) values.Add(new TypedValue((int)LispDataType.Text,"TRIM"));
                        foreach(Point q in pts)
                        {
                            values.Add(new TypedValue((int)LispDataType.ListBegin));
                            values.Add(new TypedValue((int)LispDataType.Double,(double)q.X/width));
                            values.Add(new TypedValue((int)LispDataType.Double,1.0-(double)q.Y/height));
                            values.Add(new TypedValue((int)LispDataType.ListEnd));
                        }
                        values.Add(new TypedValue((int)LispDataType.ListEnd));
                        return new ResultBuffer(values.ToArray());
                    }
                }
                Thread.Sleep(8);
            }
        }
        catch (System.Exception ex)
        {
            CadApp.DocumentManager.MdiActiveDocument.Editor.WriteMessage("\n拖动捕捉失败："+ex.Message);
            return Status("ERROR");
        }
        finally
        {
            Application.RemoveMessageFilter(filter);
            if (precision != null) precision.Dispose();
            if (pen!=null) pen.Dispose();
            if (graphics!=null) graphics.Dispose();
            if (canvas!=IntPtr.Zero) InvalidateRect(canvas,IntPtr.Zero,false);
            if (nativeCanvas!=IntPtr.Zero) RestoreNativeCrosshair(nativeCanvas);
        }
    }
}





