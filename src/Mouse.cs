using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;
using ZwSoft.ZwCAD.DatabaseServices;
using ZwSoft.ZwCAD.EditorInput;
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
                        info.Icon = false; info.X = 7; info.Y = 7;
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
        // AutoCAD 的样子：没有十字臂，只有中间一个小方框（ZWCAD 侧靠 CURSORSIZE=1 也是这个形态）。
        // 黑框套白框，深浅两种底图上都看得清；正中心留一个点当拾取点。
        internal static Bitmap MakeBitmap()
        {
            Bitmap b = new Bitmap(15, 15, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            using (Graphics g = Graphics.FromImage(b))
            {
                g.Clear(Color.Transparent);
                g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.None;
                g.FillRectangle(Brushes.Black, 3, 3, 9, 9);
                g.FillRectangle(Brushes.White, 5, 5, 5, 5);
                g.FillRectangle(Brushes.Black, 7, 7, 1, 1);
            }
            return b;
        }
        public void Refresh() { SetCursor(handle); }
        internal bool Check()
        {
            ICONINFO icon;
            if (!GetIconInfo(handle, out icon)) return false;
            bool hotspot = !icon.Icon && icon.X == 7 && icon.Y == 7;
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
    // ===== 悬停高亮 =====
    // 复刻 AutoCAD：准星压到要修剪的对象上时，那个对象自己亮起来，用户才知道「确实选中了它」。
    // 只有 TR / EX 的交互层（ZWKCAP103）会用到；拖动修剪 / 拖动延伸（ZWKCAP102）一个字节都不变。
    //
    // 为什么这么绕：2026-09-21 在真机（ZWCAD 2020）上逐一带抓屏验过——
    // Entity.Highlight() / Highlight()+Regen / SetImpliedSelection / (redraw e 3)
    // 前后一个像素都不变；往绘图区窗口 DC 上画（连拖动笔画的那些线也一样）进程内
    // 抓得到、屏幕上根本看不见。所以这里自己算实体的屏幕折线，画在一个自己创建的
    // 置顶透明「贴纸」窗口上：CAD 的画布是 GPU 合成的，只有独立窗口才盖得住。
    sealed class HoverPreview : IDisposable
    {
        const int PenWidth = 3;
        static readonly Color Highlight = Color.FromArgb(0, 232, 255);
        readonly List<ObjectId> lit = new List<ObjectId>();
        Rectangle dirty = Rectangle.Empty;
        Point last = new Point(int.MinValue, int.MinValue);
        DateTime stamp = DateTime.MinValue;
        IntPtr canvas = IntPtr.Zero;
        Point origin = Point.Empty;      // 绘图区左上角在屏幕上的位置
        Sticker sticker;
        Sticker cover;
        Bitmap image;
        string paper = "-";
        internal int Hits { get { return lit.Count; } }
        internal string Paper { get { return paper; } }
        internal string Bounds { get { return dirty.Width > 0 ? dirty.Left+","+dirty.Top+","+dirty.Right+","+dirty.Bottom : "-"; } }

        internal void Attach(IntPtr h)
        {
            canvas = h;
            Point p = new Point(0, 0);
            if (h != IntPtr.Zero && ClientToScreen(h, ref p)) origin = p;
        }

        // 撤掉高亮：把贴纸藏起来就行，不用麻烦 CAD 重画。
        void Wipe()
        {
            if (sticker != null) sticker.Show(false);
            if (image != null) { image.Dispose(); image = null; }
            dirty = Rectangle.Empty;
            lit.Clear();
        }

        internal void Clear()
        { Wipe(); last = new Point(int.MinValue, int.MinValue); stamp = DateTime.MinValue; }

        // 每帧调用；内部节流（移动 >= 2 像素、距上次 >= 45 毫秒才真的去查），
        // 免得每个 8 毫秒的空转都去问一次选择集。
        internal void Update(Point p, int width, int height, bool inside)
        {
            if (!inside) { Clear(); return; }
            if (Math.Abs(p.X-last.X) < 2 && Math.Abs(p.Y-last.Y) < 2) return;
            if ((DateTime.UtcNow-stamp).TotalMilliseconds < 45) return;
            stamp = DateTime.UtcNow; last = p;
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc == null || width < 1 || height < 1) return;
            var ed = doc.Editor;
            ZwSoft.ZwCAD.Geometry.Point3d a, b;
            Projector pj;
            try
            {
                using (var view = ed.GetCurrentView())
                {
                    double vh = view.Height, vw = vh*width/height;
                    double cx = view.CenterPoint.X + ((double)p.X/width-0.5)*vw;
                    double cy = view.CenterPoint.Y + (0.5-(double)p.Y/height)*vh;
                    double d = 2.0*vh/height;   // 与 ze:box 的「2 像素半格」一致
                    var t = ZwSoft.ZwCAD.Geometry.Matrix3d.PlaneToWorld(view.ViewDirection);
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Displacement(view.Target-ZwSoft.ZwCAD.Geometry.Point3d.Origin)*t;
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Rotation(-view.ViewTwist,view.ViewDirection,view.Target)*t;
                    a = new ZwSoft.ZwCAD.Geometry.Point3d(cx-d,cy-d,0).TransformBy(t);
                    b = new ZwSoft.ZwCAD.Geometry.Point3d(cx+d,cy+d,0).TransformBy(t);
                    pj = new Projector(t.Inverse(), view.CenterPoint, vw, vh, width, height);
                }
            }
            catch { return; }
            var ids = new List<ObjectId>();
            try
            {
                PromptSelectionResult r = ed.SelectCrossingWindow(a,b);
                if (r != null && r.Status == PromptStatus.OK && r.Value != null)
                    try { ids.AddRange(r.Value.GetObjectIds()); } finally { r.Value.Dispose(); }
            }
            catch { ids.Clear(); }   // 查询失败就当作「没压到东西」
            if (Same(ids)) return;   // 还是同一批对象，别动它，免得闪
            Wipe();
            Paint(ids, pj);
        }

        bool Same(List<ObjectId> ids)
        {
            if (ids.Count != lit.Count) return false;
            foreach (ObjectId id in ids) if (!lit.Contains(id)) return false;
            return true;
        }

        void Paint(List<ObjectId> ids, Projector pj)
        {
            if (canvas == IntPtr.Zero || ids.Count == 0) return;
            var shapes = new List<PointF[]>();
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc == null) return;
            using (var tr = doc.Database.TransactionManager.StartTransaction())
                foreach (ObjectId id in ids)
                    try
                    {
                        Entity ent = tr.GetObject(id, OpenMode.ForRead) as Entity;
                        if (ent != null) Shape(ent, pj, shapes);
                    }
                    catch { }   // 不在图里 / 打不开的对象直接放过
            if (shapes.Count == 0) return;
            float l = float.MaxValue, t = float.MaxValue, r = float.MinValue, bt = float.MinValue;
            foreach (PointF[] s in shapes)
                foreach (PointF q in s)
                {
                    if (q.X < l) l = q.X; if (q.X > r) r = q.X;
                    if (q.Y < t) t = q.Y; if (q.Y > bt) bt = q.Y;
                }
            if (r < l || bt < t) return;
            int pad = PenWidth+2;
            int x0 = (int)Math.Floor(l)-pad, y0 = (int)Math.Floor(t)-pad;
            int w = (int)Math.Ceiling(r)-x0+pad+1, h = (int)Math.Ceiling(bt)-y0+pad+1;
            if (w < 1 || h < 1 || w > 20000 || h > 20000) return;
            image = new Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            using (Graphics g = Graphics.FromImage(image))
            {
                g.Clear(Color.Transparent);
                g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
                using (Pen pen = new Pen(Highlight, PenWidth))
                    foreach (PointF[] s in shapes)
                    {
                        if (s.Length < 2) continue;
                        var q = new PointF[s.Length];
                        for (int i = 0; i < s.Length; i++) q[i] = new PointF(s[i].X-x0, s[i].Y-y0);
                        g.DrawLines(pen, q);
                    }
            }
            dirty = Rectangle.FromLTRB(x0, y0, x0+w, y0+h);
            if (sticker == null) sticker = new Sticker();
            bool ok = Paste(sticker, image, origin.X+x0, origin.Y+y0, w, h);
            paper = sticker.Handle + ",ok=" + ok + ",on=" + sticker.On + ",at=" + sticker.Bounds;
            lit.AddRange(ids);
        }

        // 盖住 CAD 那个冻住的准星：进循环时它停在鼠标当时的位置上，之后 CAD 不再重画它，
// 不盖掉就会和自绘光标同时存在（用户看到的就是「两个准星」）。取一圈像素的众数当背景色，
// 画一块纯色贴纸压上去；循环结束贴纸销毁，画面自然恢复。
        internal void Cover(Point p)
        {
            if (canvas == IntPtr.Zero) return;
            try
            {
                if (cover == null) cover = new Sticker();
                Color bg = SampleCanvas(p, 18);
                using (Bitmap bmp = new Bitmap(38, 38, System.Drawing.Imaging.PixelFormat.Format32bppArgb))
                {
                    using (Graphics g = Graphics.FromImage(bmp)) g.Clear(bg);
                    bool ok = Paste(cover, bmp, origin.X + p.X - 19, origin.Y + p.Y - 19, 38, 38);
                    paper = "cover=" + cover.Handle + ",ok=" + ok;
                }
            }
            catch (System.Exception ex) { paper = "coverERR:" + ex.Message; }
        }

        Color SampleCanvas(Point p, int r)
        {
            var tally = new Dictionary<int, int>();
            IntPtr dc = GetDC(canvas);
            try
            {
                for (int i = 0; i < 12; i++)
                {
                    double a = i * Math.PI / 6;
                    int c = (int)(GetPixel(dc, p.X + (int)Math.Round(Math.Cos(a)*r), p.Y + (int)Math.Round(Math.Sin(a)*r)) & 0x00FFFFFF);
                    tally[c] = (tally.ContainsKey(c) ? tally[c] : 0) + 1;
                }
            }
            finally { ReleaseDC(canvas, dc); }
            int best = 0, bestN = -1;
            foreach (var kv in tally) if (kv.Value > bestN) { bestN = kv.Value; best = kv.Key; }
            return Color.FromArgb((best >> 16) & 0xFF, (best >> 8) & 0xFF, best & 0xFF);
        }

        // 把一块位图贴到置顶透明窗口上（每像素 alpha，只有画上去的像素不透明，其余点击穿透）。
        bool Paste(Sticker target, Bitmap bmp, int sx, int sy, int w, int h)
        {
            target.Show(false);
            IntPtr screenDc = IntPtr.Zero, memDc = IntPtr.Zero, hbitmap = IntPtr.Zero;
            try
            {
                screenDc = GetDC(IntPtr.Zero);
                memDc = CreateCompatibleDC(screenDc);
                hbitmap = bmp.GetHbitmap(Color.FromArgb(0));
                IntPtr old = SelectObject(memDc, hbitmap);
                POINT dst = new POINT(sx, sy), src = new POINT(0, 0);
                SIZE size = new SIZE(w, h);
                BLENDFUNCTION blend;
                blend.BlendOp = 0; blend.BlendFlags = 0; blend.SourceConstantAlpha = 255; blend.AlphaFormat = 1;
                bool ok = UpdateLayeredWindow(target.Handle, screenDc, ref dst, ref size, memDc, ref src, 0, ref blend, 2);
                SelectObject(memDc, old);
                target.Show(true);
                return ok;
            }
            catch { return false; }
            finally
            {
                if (hbitmap != IntPtr.Zero) DeleteObject(hbitmap);
                if (memDc != IntPtr.Zero) DeleteDC(memDc);
                if (screenDc != IntPtr.Zero) ReleaseDC(IntPtr.Zero, screenDc);
            }
        }

        // 只用来当「贴纸」的窗口：裸 Win32 弹窗（借系统 STATIC 类），不用 WinForms——
        // 在 CAD 里 new Form() + Show() 会把整个 LISP 卡死（真机实测），裸窗口没这个毛病。
        // 置顶 + 分层 + 鼠标穿透 + 不抢焦点。
        sealed class Sticker : IDisposable
        {
            IntPtr hwnd;
            internal IntPtr Handle { get { return hwnd; } }
            internal Sticker()
            {
                uint ex = 0x00080000 | 0x00000020 | 0x00000080 | 0x00000008 | 0x08000000;
                hwnd = CreateWindowEx(ex, "STATIC", "", 0x80000000, -4000, -4000, 1, 1,
                                      IntPtr.Zero, IntPtr.Zero, GetModuleHandle(null), IntPtr.Zero);
                if (hwnd == IntPtr.Zero) throw new InvalidOperationException("无法创建高亮贴纸窗口。");
            }
            // 显示时要抬到「置顶那一层的最上面」，否则会被别的置顶窗口（比如 CAD 自己的
            // 对话窗口）压住看不见。SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|SWP_SHOWWINDOW。
            internal void Show(bool on)
            {
                if (on) SetWindowPos(hwnd, (IntPtr)(-1), 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0010 | 0x0040);
                else ShowWindow(hwnd, 0);
            }
            internal bool On { get { return IsWindowVisible(hwnd); } }
            internal Rectangle Bounds
            {
                get
                {
                    RECT r;
                    if (!GetWindowRect(hwnd, out r)) return Rectangle.Empty;
                    return Rectangle.FromLTRB(r.L, r.T, r.R, r.B);
                }
            }
            public void Dispose()
            {
                if (hwnd != IntPtr.Zero) { DestroyWindow(hwnd); hwnd = IntPtr.Zero; }
            }
        }

        // ---- 实体 -> 绘图区像素折线 ----
        // 视图平面 <-> 像素 的换算，和 ze:view / ze:wpts 那一套完全一致。
        sealed class Projector
        {
            readonly ZwSoft.ZwCAD.Geometry.Matrix3d back;
            readonly double cx, cy, vw, vh;
            readonly int width, height;
            internal Projector(ZwSoft.ZwCAD.Geometry.Matrix3d back, ZwSoft.ZwCAD.Geometry.Point2d center, double vw, double vh, int width, int height)
            { this.back = back; this.cx = center.X; this.cy = center.Y; this.vw = vw; this.vh = vh; this.width = width; this.height = height; }
            internal PointF P(ZwSoft.ZwCAD.Geometry.Point3d w) { return P(w.X, w.Y, w.Z); }
            internal PointF P(double x, double y, double z)
            {
                ZwSoft.ZwCAD.Geometry.Point3d q = new ZwSoft.ZwCAD.Geometry.Point3d(x,y,z).TransformBy(back);
                return new PointF((float)(((q.X-cx)/vw+0.5)*width), (float)((0.5-(q.Y-cy)/vh)*height));
            }
        }

        static void Shape(Entity ent, Projector pj, List<PointF[]> outList)
        {
            Line ln = ent as Line;
            if (ln != null) { outList.Add(new PointF[] { pj.P(ln.StartPoint), pj.P(ln.EndPoint) }); return; }
            Circle ci = ent as Circle;
            if (ci != null) { outList.Add(Samples(pj, ci.Center, ci.Radius, ci.Radius, 0, 2*Math.PI)); return; }
            Arc ar = ent as Arc;
            if (ar != null) { outList.Add(Samples(pj, ar.Center, ar.Radius, ar.Radius, ar.StartAngle, ar.EndAngle)); return; }
            Ellipse el = ent as Ellipse;
            if (el != null)
            {
                ZwSoft.ZwCAD.Geometry.Vector3d mx = el.MajorAxis;
                outList.Add(Samples(pj, el.Center, el.MajorRadius, el.MinorRadius, mx.X, mx.Y, el.StartAngle, el.EndAngle));
                return;
            }
            Polyline pl = ent as Polyline;
            if (pl != null) { Polyline(pl, pj, outList); return; }
            // 认不出来的（样条、块、文字、标注、填充……）就画外框：也算「有反馈」。
            try
            {
                Extents3d ex = ent.GeometricExtents;
                outList.Add(new PointF[] {
                    pj.P(ex.MinPoint), pj.P(ex.MaxPoint.X, ex.MinPoint.Y, ex.MinPoint.Z),
                    pj.P(ex.MaxPoint), pj.P(ex.MinPoint.X, ex.MaxPoint.Y, ex.MinPoint.Z),
                    pj.P(ex.MinPoint) });
            }
            catch { }
        }

        // 圆/圆弧：角度按 OCS 的 XY 平面算（TR / EX 只支持平面视图，拉伸方向就是 +Z）。
        static PointF[] Samples(Projector pj, ZwSoft.ZwCAD.Geometry.Point3d c, double major, double minor, double a0, double a1)
        { return Samples(pj, c, major, minor, 1, 0, a0, a1); }

        static PointF[] Samples(Projector pj, ZwSoft.ZwCAD.Geometry.Point3d c, double major, double minor, double ux, double uy, double a0, double a1)
        {
            while (a1 <= a0) a1 += 2*Math.PI;          // ARC 允许 a1 < a0，补足一圈
            int n = Math.Max(12, Math.Min(180, (int)((a1-a0)/Math.PI*36)));
            var pts = new PointF[n+1];
            for (int i = 0; i <= n; i++)
            {
                double t = a0 + (a1-a0)*i/n, cs = Math.Cos(t), sn = Math.Sin(t);
                pts[i] = pj.P(c.X + major*cs*ux - minor*sn*uy,
                              c.Y + major*cs*uy + minor*sn*ux,
                              c.Z);
            }
            return pts;
        }

        static void Polyline(Polyline pl, Projector pj, List<PointF[]> outList)
        {
            int n = pl.NumberOfVertices;
            if (n < 2) return;
            bool closed = pl.Closed;
            var pts = new List<PointF>(n+1);
            for (int i = 0; i < n; i++) pts.Add(pj.P(pl.GetPoint3dAt(i)));
            if (closed) pts.Add(pts[0]);
            outList.Add(pts.ToArray());
        }

        public void Dispose()
        {
            Wipe();
            if (sticker != null)
            {
                try { sticker.Dispose(); } catch { }
                sticker = null;
            }
            if (cover != null)
            {
                try { cover.Dispose(); } catch { }
                cover = null;
            }
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
    [StructLayout(LayoutKind.Sequential)] struct POINT { public int X, Y; internal POINT(int x, int y) { X = x; Y = y; } }
    [StructLayout(LayoutKind.Sequential)] struct SIZE { public int CX, CY; internal SIZE(int x, int y) { CX = x; CY = y; } }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] struct BLENDFUNCTION { public byte BlendOp, BlendFlags, SourceConstantAlpha, AlphaFormat; }
    [DllImport("user32.dll")] static extern IntPtr GetDC(IntPtr h);
    [DllImport("user32.dll")] static extern int ReleaseDC(IntPtr h, IntPtr dc);
    [DllImport("gdi32.dll")] static extern IntPtr CreateCompatibleDC(IntPtr dc);
    [DllImport("gdi32.dll")] static extern IntPtr SelectObject(IntPtr dc, IntPtr obj);
    [DllImport("gdi32.dll")] static extern bool DeleteDC(IntPtr dc);
    [DllImport("gdi32.dll")] static extern bool DeleteObject(IntPtr obj);
    [DllImport("gdi32.dll")] static extern uint GetPixel(IntPtr dc, int x, int y);
    [DllImport("user32.dll")] static extern bool UpdateLayeredWindow(IntPtr h, IntPtr dstDc, ref POINT dst, ref SIZE size, IntPtr srcDc, ref POINT src, int key, ref BLENDFUNCTION blend, int flags);
    [DllImport("user32.dll", CharSet = CharSet.Unicode)] static extern IntPtr CreateWindowEx(uint ex, string cls, string name, uint style, int x, int y, int w, int h, IntPtr parent, IntPtr menu, IntPtr inst, IntPtr param);
    [DllImport("user32.dll")] static extern bool DestroyWindow(IntPtr h);
    [DllImport("user32.dll")] static extern bool ShowWindow(IntPtr h, int cmd);
    [DllImport("user32.dll")] static extern bool SetWindowPos(IntPtr h, IntPtr after, int x, int y, int cx, int cy, uint flags);
    [DllImport("user32.dll")] static extern bool IsWindowVisible(IntPtr h);
    [DllImport("user32.dll")] static extern bool GetWindowRect(IntPtr h, out RECT r);
    [DllImport("kernel32.dll")] static extern IntPtr GetModuleHandle(string name);
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
        public bool Cancel, Undo, AllowKeys;
        // 0 = 没有按键；否则是 VK 码。只有 AllowKeys 打开时才会被赋值，
        // 这样 ZWKCAP102 的既有行为与以前完全一致。
        public int Key;
        public int Wheel;
        // 说明：捕获循环跑在 CAD 命令内部，这期间 CAD 根本不处理鼠标消息（真机实测），
        // 所以它的准星必然僵在原地 —— 只能自己画光标，并把那个冻住的准星盖掉。
        public bool PreFilterMessage(ref Message m)
        {
            if (m.Msg == 0x20A) Wheel += unchecked((short)((m.WParam.ToInt64() >> 16) & 65535));
            if (m.Msg == 0x100 || m.Msg == 0x104)
            {
                int k = m.WParam.ToInt32();
                if (k == 27 || k == 13 || k == 32) Cancel = true;
                else if (k == 85 && !AllowKeys) Undo = true;
                else if (AllowKeys && Key == 0 && ((k >= 48 && k <= 57) || (k >= 65 && k <= 90))) Key = k;
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

    // 自检：在指定的绘图区比例坐标上跑一次悬停高亮，保持 hold 毫秒（期间照常抽消息，
    // 所以屏幕上真的能看到高亮），返回压到的对象数。fx / fy 与捕获循环里用的那一套一致
    // （0,0 = 绘图区左上角，1,1 = 右下角）。只给自检用，不参与 TR / EX 的正常流程。
    [LispFunction("ZWK_HOVERCHECK_101")]
    public static ResultBuffer HoverCheck(ResultBuffer args)
    {
        TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
        if (a.Length < 2 || a.Length > 3) return Status("ERROR");
        var sz = (ZwSoft.ZwCAD.Geometry.Point2d)CadApp.GetSystemVariable("SCREENSIZE");
        int w = (int)sz.X, h = (int)sz.Y;
        if (w < 20 || h < 20) return Status("ERROR");
        int x = (int)Math.Round(Convert.ToDouble(a[0].Value)*w);
        int y = (int)Math.Round(Convert.ToDouble(a[1].Value)*h);
        int hold = a.Length == 3 ? Convert.ToInt32(a[2].Value) : 0;
        int n; string box, paper;
        IntPtr probeCanvas = FindCanvas(w,h);
        using (var hover = new HoverPreview())
        {
            hover.Attach(probeCanvas);
            hover.Clear();
            hover.Update(new Point(x,y), w, h, true);
            n = hover.Hits; box = hover.Bounds; paper = hover.Paper;
            DateTime until = DateTime.UtcNow.AddMilliseconds(hold);
            while (DateTime.UtcNow < until) { Application.DoEvents(); Thread.Sleep(10); }
        }
        return Status("HOVER_HITS=" + n + ";BOX=" + box + ";PAPER=" + paper);
    }

    [LispFunction("ZWK_MODULE_101")]
    public static ResultBuffer ModuleCheck(ResultBuffer args) { return Status("READY"); }

    [LispFunction("ZWK_CAPTURE_101")]
    public static ResultBuffer Capture(ResultBuffer args)
    { return CaptureCore(args, false); }

    // 给 TR/EX 交互层用：除了笔画，还把用户按下的字母/数字键回传成 ("KEY" "C")。
    // 单独一个入口，保证 ZWKCAP102 的既有行为一个字节都不变。
    public static ResultBuffer CaptureKeys(ResultBuffer args)
    { return CaptureCore(args, true); }

    static ResultBuffer CaptureCore(ResultBuffer args, bool allowKeys)
    {
        TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
        if (a.Length != 2) return Status("ERROR");
        int width = Convert.ToInt32(a[0].Value), height = Convert.ToInt32(a[1].Value);
        if (width < 20 || height < 20) return Status("ERROR");
        var filter = new InputFilter();
        filter.AllowKeys = allowKeys;
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
        // 悬停高亮只给 TR / EX 的交互层；ZWKCAP102 连这个对象都不创建。
        HoverPreview hover = allowKeys ? new HoverPreview() : null;
        int viewWidth = width, viewHeight = height;
        try
        {
            nativeCanvas = FindCanvas(width,height);
            if (nativeCanvas == IntPtr.Zero)
            {
                CadApp.DocumentManager.MdiActiveDocument.Editor.WriteMessage("\n未找到独立绘图区，已停止以避免准星残留。请使用模型空间单视口。");
                return Status("ERROR");
            }
            if (hover != null)
            {
                hover.Attach(nativeCanvas);
                // CAD 的准星会冻在「命令启动那一刻」的位置，盖上它，免得和自绘光标同时出现。
                Point c0 = new Point(0, 0);
                ClientToScreen(nativeCanvas, ref c0);
                Point mp = Cursor.Position;
                mp = new Point(mp.X - c0.X, mp.Y - c0.Y);
                if (mp.X > 0 && mp.Y > 0 && mp.X < width && mp.Y < height) hover.Cover(mp);
            }
            ClearNativeCrosshair(nativeCanvas);
            precision = new PrecisionCursor();
            while (true)
            {
                Application.DoEvents();
                if (precision != null) precision.Refresh();
                if (filter.Cancel || Down(27) || Down(2)) return Status("CANCEL");
                if (filter.Undo) return Status(drawing ? "CANCEL" : "UNDO");
                // 只有空闲状态（没在画笔画）才把按键交还给 Lisp；画到一半按键不算选项。
                if (allowKeys && !drawing && filter.Key != 0)
                {
                    int k = filter.Key; filter.Key = 0;
                    return new ResultBuffer(new TypedValue[] {
                        new TypedValue((int)LispDataType.Text, "KEY"),
                        new TypedValue((int)LispDataType.Text, ((char)k).ToString()) });
                }
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
                    if (precision != null) precision.Refresh();
                    // 视图动了，高亮的位置也就不对了，直接撤掉。
                    if (hover != null) hover.Clear();
                    // A stroke cannot start while either navigation gesture is active.
                    if (held) ready=false;
                    Thread.Sleep(8);
                    continue;
                }
                panning=false;
                if (!held) ready = true;
                // 空闲时跟着准星走：压到哪条线上，哪条线就亮起来。
                if (hover != null)
                {
                    if (drawing) hover.Clear();
                    else hover.Update(navPoint, viewWidth, viewHeight, inside);
                }
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
            if (hover != null) hover.Dispose();
            if (precision != null) precision.Dispose();
            if (pen!=null) pen.Dispose();
            if (graphics!=null) graphics.Dispose();
            if (canvas!=IntPtr.Zero) InvalidateRect(canvas,IntPtr.Zero,false);
            if (nativeCanvas!=IntPtr.Zero) RestoreNativeCrosshair(nativeCanvas);
        }
    }
}





