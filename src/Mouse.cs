using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
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
    // 观察型消息过滤器：只看不吞（永远返回 false）。grread 期间 CAD 不把鼠标消息交给 LISP，
    // 但滚轮/中键这类「视图动作」还是走 CAD 自己的消息泵 —— 借这儿把贴纸先收起来，
    // 免得它们停在旧位置（滚轮缩放、中键平移都会让贴纸过期）。
    // 2026-09-21 用户反馈：滚轮缩放后高亮留在原地、松开中键后方块在旧位置闪一下。
    sealed class ViewFilter : IMessageFilter
    {
        internal HoverPreview owner;
        internal static int Hits;        // 自检用：收到几次滚轮/中键（ZWK_VIEW_101 会报出来）
        internal static int Last;
        public bool PreFilterMessage(ref Message m)
        {
            if (m.Msg == 0x20A || m.Msg == 0x207 || m.Msg == 0x208)   // WHEEL / MBUTTONDOWN / MBUTTONUP
            {
                Hits++; Last = m.Msg;
                try { if (owner != null) owner.Clear(); } catch { }
            }
            return false;
        }
    }

    sealed class HoverPreview : IDisposable
    {
        const int PenWidth = 3;
        static readonly Color Highlight = Color.FromArgb(0, 232, 255);
        // 修剪边界集合（实体的句柄）。null = 全部对象（快速模式的默认）。
        // TR / EX 的 LISP 侧用 ZWK_BOUND_101 传进来，悬停预览靠它算「会被剪掉的那一段」。
        static HashSet<string> boundary;
        static int boundaryVer;          // 边界每变一次 +1，邻居缓存据此作废
        internal static void SetBoundary(string list)
        {
            boundaryVer++;
            if (string.IsNullOrEmpty(list)) { boundary = null; return; }
            var set = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (string s in list.Split(',')) if (s.Length > 0) set.Add(s.Trim());
            boundary = set.Count == 0 ? null : set;
        }
        static bool InBoundary(Entity e)
        {
            if (boundary == null) return true;
            try { return boundary.Contains(e.Handle.ToString()); } catch { return true; }
        }
        readonly List<ObjectId> lit = new List<ObjectId>();
        string sig = "";                 // 亮起来的「一段」的签名（对象 + 参数区间），变了才重画
        Rectangle dirty = Rectangle.Empty;
        Point last = new Point(int.MinValue, int.MinValue);
        DateTime stamp = DateTime.MinValue;
        IntPtr canvas = IntPtr.Zero;
        Point origin = Point.Empty;      // 绘图区左上角在屏幕上的位置
        Sticker sticker;
        Sticker pick;                    // 光标处那个拾取框方块（每帧跟着走）
        Bitmap image;
        string paper = "-";
        string viewSig = "";             // 视图指纹：动过视图（平移/缩放）就必须重算，见 ViewSignature
        // 中键按住时 CAD 自己在平移，这期间 grread 不再派发鼠标事件（真机实测：
        // 视图确实平移了，但贴纸停在旧位置 —— 用户反馈的「按住中键准星会漂移」）。
        // 主线程收不到事件，就找一个旁路：只在 TR / EX 交互期间存在的小看门狗线程，
        // 每 25 毫秒查一次中键，按住就把两个贴纸收起来（只碰我们自己的窗口，
        // 不调用任何 CAD API，所以线程安全）。松开后主线程在下一个鼠标事件里照常重画。
        Thread watch;
        volatile bool watchStop;
        ViewFilter vfilter;
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
        // 注意不含光标方块 —— 高亮换段时会走这里，方块得一直跟着手（Clear 才收方块）。
        void Wipe()
        {
            if (sticker != null) sticker.Show(false);
            if (image != null) { image.Dispose(); image = null; }
            dirty = Rectangle.Empty;
            lit.Clear();
        }

        // 光标处的拾取框方块（AutoCAD 那个「小方块」的样子）。ZWCAD 在 grread 跟踪模式下
        // 不画自己的拾取框（2026-09-21 放大截图确认：只有十字），所以自己贴一个。
        // 每帧都重贴、不参与下面的节流 —— 它就是「触发范围」的可视化，滞后就没意义了。
        void PickBox(Point p)
        {
            if (canvas == IntPtr.Zero) return;
            try
            {
                int side = PickPixels();
                int w = side + 2;
                using (Bitmap bmp = new Bitmap(w, w, System.Drawing.Imaging.PixelFormat.Format32bppArgb))
                {
                    using (Graphics g = Graphics.FromImage(bmp))
                    {
                        g.Clear(Color.Transparent);
                        g.DrawRectangle(Pens.White, 0, 0, w-1, w-1);
                    }
                    if (pick == null) pick = new Sticker();
                    Paste(pick, bmp, origin.X+p.X-w/2, origin.Y+p.Y-w/2, w, w, false);
                }
            }
            catch { }
        }

        internal void Clear()
        {
            Wipe();
            if (pick != null) pick.Show(false);
            sig = ""; last = new Point(int.MinValue, int.MinValue); stamp = DateTime.MinValue;
        }

        // 看门狗：只在 TR / EX 交互期间跑（第一次 Update 起、每次 capture 结束（hover-end）停）。
        void Watch()
        {
            while (!watchStop)
            {
                try
                {
                    bool hide = (GetAsyncKeyState(4) & 0x8000) != 0;      // 中键按住：CAD 自己在平移
                    // CAD 不在前台（Alt+Tab 切走了）也要收：贴纸是独立的置顶窗口，
                    // 不收就会浮在别的软件上面（2026-09-21 用户反馈「居然跨软件显示了」）。
                    if (!hide && !OurWindow(GetForegroundWindow())) hide = true;
                    if (hide)
                    {
                        // 窗口属于主线程：这里只能用 ShowWindowAsync（投递消息、不等待）。
                        // 同步的 ShowWindow 会等主线程处理，主线程在 WatchOff 里 Join 本线程时就会互等。
                        Sticker s = sticker, k = pick;
                        if (s != null) s.HideAsync();
                        if (k != null) k.HideAsync();
                    }
                }
                catch { }
                Thread.Sleep(25);
            }
        }

        void WatchOn()
        {
            if (watch == null)
            {
                watchStop = false;
                watch = new Thread(Watch);
                watch.IsBackground = true;
                watch.Start();
            }
            if (vfilter == null)
            {
                vfilter = new ViewFilter();
                vfilter.owner = this;
                try { Application.AddMessageFilter(vfilter); } catch { }
            }
        }

        internal void WatchOff()
        {
            // 每次拾取结束（hover-end）后 LISP 侧就会执行修剪 / 延伸，几何变了，邻居缓存作废
            nbCache.Clear();
            watchStop = true;
            Thread t = watch;
            watch = null;
            if (t != null) { try { t.Join(200); } catch { } }
            ViewFilter f = vfilter;
            vfilter = null;
            if (f != null) { try { f.owner = null; Application.RemoveMessageFilter(f); } catch { } }
        }

        // 每帧调用；内部节流（移动 >= 2 像素、距上次 >= 45 毫秒才真的去查），
        // 免得每个 8 毫秒的空转都去问一次选择集。
        // shift = 按住 Shift（延伸模式）：那句提示要延伸，不做「剪掉哪一段」的预览。
        internal void Update(Point p, int width, int height, bool inside, bool shift)
        {
            if (!inside) { Clear(); return; }
            WatchOn();                                   // 中键平移期间要有人负责收贴纸，见 Watch
            // 中键按住 = CAD 自己在平移：先把贴纸收起来，别让它们停在旧位置看着像「漂移」。
            // 松手后靠下面的视图指纹强制重算（2026-09-21 用户反馈「按住中键准星会漂移」）。
            if (Down(4)) { Clear(); return; }
            // 视图动过（平移/缩放）就作废重算：平移后光标可能还在同一个像素上，
            // 只靠「移动 >= 2 像素」的节流会漏掉重算，贴纸会盖在别的图元上。
            string vs = ViewSignature();
            if (vs != viewSig) { viewSig = vs; Clear(); }
            PickBox(p);                                  // 方块每帧跟手，不受下面两道节流限制
            if (Math.Abs(p.X-last.X) < 2 && Math.Abs(p.Y-last.Y) < 2) return;
            if ((DateTime.UtcNow-stamp).TotalMilliseconds < 45) return;
            stamp = DateTime.UtcNow; last = p;
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc == null || width < 1 || height < 1) return;
            var ed = doc.Editor;
            ZwSoft.ZwCAD.Geometry.Point3d a, b, cursor;
            Projector pj;
            try
            {
                using (var view = ed.GetCurrentView())
                {
                    double vh = view.Height, vw = vh*width/height;
                    double d = 0.5*PickPixels()*vh/height;   // 半格 = 一半拾取框，与 ze:box 一致
                    var t = ZwSoft.ZwCAD.Geometry.Matrix3d.PlaneToWorld(view.ViewDirection);
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Displacement(view.Target-ZwSoft.ZwCAD.Geometry.Point3d.Origin)*t;
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Rotation(-view.ViewTwist,view.ViewDirection,view.Target)*t;
                    // view.CenterPoint 在 ZWCAD 2020 里给的就是 WCS 下的视图中心（真机实测：
                    // 4483.6647 = VIEWCTR 1657.0552 + UCSORG 2826.6095，Target=(0,0,0)），
                    // 配合 t 算出来的就是光标的世界坐标，这一段没问题。
                    // 悬停查不到东西的原因在选择集调用：ZWCAD 的 SelectCrossingWindow 收 UCS 点，
                    // 见 CrossWindow 的注释。
                    double cx = view.CenterPoint.X + ((double)p.X/width-0.5)*vw;
                    double cy = view.CenterPoint.Y + (0.5-(double)p.Y/height)*vh;
                    a = new ZwSoft.ZwCAD.Geometry.Point3d(cx-d,cy-d,0).TransformBy(t);
                    b = new ZwSoft.ZwCAD.Geometry.Point3d(cx+d,cy+d,0).TransformBy(t);
                    // 光标本身的世界坐标（就是那个小窗的中心），用来判断现下会剪掉哪一段
                    cursor = new ZwSoft.ZwCAD.Geometry.Point3d((a.X+b.X)/2, (a.Y+b.Y)/2, (a.Z+b.Z)/2);
                    // 屏幕四个角的世界坐标 -> 屏幕范围（有视图转角时取外接矩形）
                    double minX = double.MaxValue, minY = double.MaxValue, maxX = double.MinValue, maxY = double.MinValue;
                    foreach (double sxk in new double[] { -0.5, 0.5 })
                        foreach (double syk in new double[] { -0.5, 0.5 })
                        {
                            var c = new ZwSoft.ZwCAD.Geometry.Point3d(view.CenterPoint.X + sxk*vw, view.CenterPoint.Y + syk*vh, 0).TransformBy(t);
                            minX = Math.Min(minX, c.X); minY = Math.Min(minY, c.Y);
                            maxX = Math.Max(maxX, c.X); maxY = Math.Max(maxY, c.Y);
                        }
                    viewLo = new ZwSoft.ZwCAD.Geometry.Point3d(minX, minY, 0);
                    viewHi = new ZwSoft.ZwCAD.Geometry.Point3d(maxX, maxY, 0);
                    viewOk = true;
                    pj = new Projector(t.Inverse(), view.CenterPoint, vw, vh, width, height);
                }
            }
            catch { return; }
            var ids = new List<ObjectId>();
            try
            {
                PromptSelectionResult r = CrossWindow(ed, a, b);
                if (r != null && r.Status == PromptStatus.OK && r.Value != null)
                    try { ids.AddRange(r.Value.GetObjectIds()); } finally { r.Value.Dispose(); }
            }
            catch { ids.Clear(); }   // 查询失败就当作「没压到东西」
            var shapes = new List<PointF[]>();
            var keep = new List<ObjectId>();
            var sb = new System.Text.StringBuilder();
            using (var tr = doc.Database.TransactionManager.StartTransaction())
                foreach (ObjectId id in ids)
                    try
                    {
                        Entity ent = tr.GetObject(id, OpenMode.ForRead) as Entity;
                        if (ent == null) continue;
                        // 标注类不参与预览：没有「会被剪掉的那一段」，画外框只会变成
                        // 盖住半张图的大方框（2026-09-21 用户反馈「这个很明显不对」）。
                        if (IsAnnotative(ent)) continue;
                        PointF[] poly; string what;
                        if (!shift && Piece(ent, cursor, pj, tr, out poly, out what)) shapes.Add(poly);
                        else { Shape(ent, pj, shapes); what = "*"; }
                        keep.Add(id);
                        sb.Append(id.ToString()).Append(':').Append(what).Append(';');
                    }
                    catch { }   // 不在图里 / 打不开的对象直接放过
            // 亮起来的还是同一段就别动它，免得闪（同一个对象、同一段参数区间才算没变）。
            string s = sb.ToString();
            if (s == sig) return;
            sig = s;
            Wipe();
            lit.AddRange(keep);
            Render(shapes);
        }

        // 「点下去会被剪掉的那一段」——AutoCAD 快速模式的预览就是这样：
        // 以光标在对象上的位置为界，取它到左右最近两个交点之间的部分；一个交点都没有
        // （那这个对象剪不动，快速模式下会被直接删掉）就整条亮。
        // 只认曲线（直线/圆弧/圆/多段线…）；块、文字、标注这类判断不了，返回 false 由调用方按整条画。
        bool Piece(Entity ent, ZwSoft.ZwCAD.Geometry.Point3d cursor, Projector pj, Transaction tr,
                   out PointF[] poly, out string what)
        {
            poly = null; what = "";
            Curve cv = ent as Curve;
            if (cv == null) return false;
            double p, lo, hi;
            List<ObjectId> near;
            try
            {
                p = cv.GetParameterAtPoint(cv.GetClosestPointTo(cursor, false));
                lo = cv.StartParam; hi = cv.EndParam;
                near = Neighbours(ent, tr);
            }
            catch { return false; }
            // 闭合曲线（圆 / 闭合多段线 / 闭合样条）的参数是一圈：光标两侧最近的交点可能
            // 隔着参数起点（0 点），不能再按「比 p 小 / 比 p 大」去找，要按环上的距离找，
            // 否则高亮的会是另一半或一小截（审查发现）。
            bool closed = false;
            try { closed = cv.Closed; } catch { }
            double period = hi - lo, start = lo;
            double dLo = double.MaxValue, dHi = double.MaxValue;
            bool any = false;
            foreach (ObjectId bid in near)
                try
                {
                    Entity b = tr.GetObject(bid, OpenMode.ForRead) as Entity;
                    if (b == null) continue;
                    // 边界只认曲线：标注线 / 尺寸线不算修剪边界。2026-09-21 用户反馈
                    // 「这个是一整个线，不应该受到标注线影响」——原来压到线上，那一段
                    // 会被穿过它的绿色尺寸线截断，看着莫名其妙。
                    if (!(b is Curve)) continue;
                    var pts = new ZwSoft.ZwCAD.Geometry.Point3dCollection();
                    ent.IntersectWith(b, Intersect.OnBothOperands, pts, IntPtr.Zero, IntPtr.Zero);
                    foreach (ZwSoft.ZwCAD.Geometry.Point3d q in pts)
                    {
                        double t;
                        try { t = cv.GetParameterAtPoint(q); } catch { continue; }
                        if (closed && period > 1e-12)
                        {
                            double back = p - t, fwd = t - p;
                            while (back <= 1e-9) back += period;
                            while (fwd <= 1e-9) fwd += period;
                            if (back < period - 1e-9 && back < dLo) dLo = back;
                            if (fwd < period - 1e-9 && fwd < dHi) dHi = fwd;
                            any = true;
                        }
                        else
                        {
                            if (t < p-1e-9) { if (t > lo) lo = t; }
                            else if (t > p+1e-9) { if (t < hi) hi = t; }
                        }
                    }
                }
                catch { }
            if (closed && period > 1e-12)
            {
                // 一个交点都没有：整圈剪不动，交给调用方按整条画
                if (!any || dLo == double.MaxValue || dHi == double.MaxValue) return false;
                lo = p - dLo; hi = p + dHi;
            }
            if (!(hi > lo + 1e-12)) return false;
            int n = Math.Max(2, Math.Min(180, (int)((hi-lo)/Math.PI*36)));
            var arr = new PointF[n+1];
            for (int i = 0; i <= n; i++)
            {
                double u = lo + (hi-lo)*i/n;
                if (closed && period > 1e-12)
                {
                    while (u < start) u += period;
                    while (u > start + period) u -= period;
                }
                arr[i] = pj.P(cv.GetPointAtParameter(u));
            }
            poly = arr;
            what = lo.ToString("0.#####", CultureInfo.InvariantCulture) + "," + hi.ToString("0.#####", CultureInfo.InvariantCulture);
            return true;
        }

        // 悬停对象附近的对象（拿它的范围开个小窗去问，绝不整图扫），求交点用。
        // 只留下在修剪边界集合里的；边界集合为空（快速模式的默认）= 全部对象都算边界。
        List<ObjectId> Neighbours(Entity ent, Transaction tr)
        {
            var outList = new List<ObjectId>();
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc == null) return outList;
            // 同一视图、同一边界集合下，同一个对象的邻居不会变：缓存起来，
            // 光标沿着一根线移动时不用每 45 毫秒重新开窗选一次。
            if (nbVer != boundaryVer || nbView != viewSig) { nbCache.Clear(); nbVer = boundaryVer; nbView = viewSig; }
            List<ObjectId> cached;
            if (nbCache.TryGetValue(ent.ObjectId, out cached)) return cached;
            ZwSoft.ZwCAD.DatabaseServices.Extents3d ex;
            try { ex = ent.GeometricExtents; } catch { return outList; }
            var p0 = ex.MinPoint; var p1 = ex.MaxPoint;
            // 裁到当前屏幕范围：长直线 / 大多段线的范围接近整张图，不裁的话每次悬停都在全图里
            // 选一遍、逐个求交，大图会卡（审查发现）。选择集接口本来也只认屏幕内的对象，裁掉不丢结果。
            if (viewOk)
            {
                p0 = new ZwSoft.ZwCAD.Geometry.Point3d(Math.Max(p0.X, viewLo.X), Math.Max(p0.Y, viewLo.Y), p0.Z);
                p1 = new ZwSoft.ZwCAD.Geometry.Point3d(Math.Min(p1.X, viewHi.X), Math.Min(p1.Y, viewHi.Y), p1.Z);
                if (p1.X < p0.X || p1.Y < p0.Y) { nbCache[ent.ObjectId] = outList; return outList; }
            }
            double m = 1e-6 + 0.001*Math.Max(p1.X-p0.X, p1.Y-p0.Y);   // 略微放大，免得正好卡端点的交点漏掉
            PromptSelectionResult r;
            try
            {
                r = CrossWindow(doc.Editor,
                    new ZwSoft.ZwCAD.Geometry.Point3d(p0.X-m, p0.Y-m, p0.Z),
                    new ZwSoft.ZwCAD.Geometry.Point3d(p1.X+m, p1.Y+m, p1.Z));
            }
            catch { return outList; }
            if (r == null || r.Status != PromptStatus.OK || r.Value == null) return outList;
            try
            {
                foreach (ObjectId id in r.Value.GetObjectIds())
                {
                    if (id == ent.ObjectId) continue;
                    try
                    {
                        Entity e = tr.GetObject(id, OpenMode.ForRead) as Entity;
                        if (e != null && InBoundary(e)) outList.Add(id);
                    }
                    catch { }
                }
            }
            finally { r.Value.Dispose(); }
            nbCache[ent.ObjectId] = outList;
            return outList;
        }
        readonly Dictionary<ObjectId, List<ObjectId>> nbCache = new Dictionary<ObjectId, List<ObjectId>>();
        int nbVer = -1;
        string nbView = null;
        // 当前屏幕在 WCS 下的范围（Update 里算），Neighbours 用它裁窗口
        ZwSoft.ZwCAD.Geometry.Point3d viewLo, viewHi;
        bool viewOk;

        // 把算好的屏幕折线贴到置顶透明窗上。
        void Render(List<PointF[]> shapes)
        {
            if (canvas == IntPtr.Zero || shapes.Count == 0) return;
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
        }

        // 把一块位图贴到置顶透明窗口上（每像素 alpha，只有画上去的像素不透明，其余点击穿透）。
        // 笔画（InkPreview）也用它，所以是 internal static。
        internal static bool Paste(Sticker target, Bitmap bmp, int sx, int sy, int w, int h)
        { return Paste(target, bmp, sx, sy, w, h, true); }

        // hide = true：先藏后贴再显示（换位置明显时不闪）；false 给「每帧跟手」的拾取框用，
        // 它只在必要的时候显示一次，之后一直复用同一块面，避免高频 show/hide 抖。
        internal static bool Paste(Sticker target, Bitmap bmp, int sx, int sy, int w, int h, bool hide)
        {
            if (hide) target.Show(false);
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
                if (hide || !target.On) target.Show(true);
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
        // 置顶 + 分层 + 鼠标穿透 + 不抢焦点。悬停高亮与拖动笔画共用。
        internal sealed class Sticker : IDisposable
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
            // 给看门狗线程用：跨线程隐藏，不阻塞。
            internal void HideAsync() { IntPtr h = hwnd; if (h != IntPtr.Zero) ShowWindowAsync(h, 0); }
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
            WatchOff();
            Wipe();
            if (pick != null)
            {
                try { pick.Dispose(); } catch { }
                pick = null;
            }
            if (sticker != null)
            {
                try { sticker.Dispose(); } catch { }
                sticker = null;
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
    [DllImport("user32.dll")] static extern bool UpdateLayeredWindow(IntPtr h, IntPtr dstDc, ref POINT dst, ref SIZE size, IntPtr srcDc, ref POINT src, int key, ref BLENDFUNCTION blend, int flags);
    [DllImport("user32.dll", CharSet = CharSet.Unicode)] static extern IntPtr CreateWindowEx(uint ex, string cls, string name, uint style, int x, int y, int w, int h, IntPtr parent, IntPtr menu, IntPtr inst, IntPtr param);
    [DllImport("user32.dll")] static extern bool DestroyWindow(IntPtr h);
    [DllImport("user32.dll")] static extern bool ShowWindow(IntPtr h, int cmd);
    [DllImport("user32.dll")] static extern bool ShowWindowAsync(IntPtr h, int cmd);
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

    // ===== TR / EX 交互层（LISP 侧用 grread 跟踪模式驱动）=====
    // 2026-09-21 在 ZWCAD 2020 上真机抓取：grread 跟踪模式下 CAD 自己的准星会跟着鼠标走，
    // 事件只有 (5 移动)/(3 左键按下)/(25 右键)/(2 键盘)，坐标就是 UCS 点；左键「松开」不产生
    // 任何事件（只能靠 GetAsyncKeyState 查）。所以输入循环交给 LISP 的 grread，这里只帮忙做三件事：
    //   1) 查按键状态（Shift / 左键）与鼠标当前位置；
    //   2) 画悬停高亮（复刻 AutoCAD 的「压到哪条线，哪条线亮起来」）；
    //   3) 画拖动时的笔画（青色；按住 Shift 是橙红色）。
    static HoverPreview hover;
    static InkPreview ink;

    static string N(double v) { return v.ToString("0.######", CultureInfo.InvariantCulture); }

    static bool ScreenSize(out int w, out int h)
    {
        w = h = 0;
        try
        {
            var sz = (ZwSoft.ZwCAD.Geometry.Point2d)CadApp.GetSystemVariable("SCREENSIZE");
            w = (int)sz.X; h = (int)sz.Y;
        }
        catch { return false; }
        return w >= 20 && h >= 20;
    }

    // 拾取光圈边长（像素）：直接取 ZWCAD 的 PICKBOX —— 它本来就是「拾取框」的大小，
    // 也是点选时的判定孔径。悬停判定与 LISP 侧 ze:box 用同一个值，屏幕上再照原样
    // 贴一个方块（HoverPreview.PickBox），看到多大、触发就是多大。
    static int PickPixels()
    {
        try
        {
            int n = Convert.ToInt32(CadApp.GetSystemVariable("PICKBOX"));
            if (n >= 3 && n <= 60) return n;
        }
        catch { }
        return 8;
    }

    // 标注类对象（文字 / 尺寸 / 引线 / 公差 / 填充 / 表格）不参与 TR / EX 的悬停预览：
    // 它们没有「会被剪掉的那一段」，画外框只会变成一个盖住半张图的大方框（2026-09-21
    // 真机实测：压到尺寸文字上高亮整个文字外框，用户反馈「这个很明显不对」）。
    // 按 DXF 名判断（ZWCAD 2020 的 API 里没有 Tolerance 实体类，逐类型判会漏）；
    // LISP 侧 ze:skip 排除了同一批名字（修剪边界与「删除修剪不到的」），两边一致。
    static readonly HashSet<string> AnnotativeNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase) {
        "TEXT", "MTEXT", "ATTDEF", "ATTRIB", "DIMENSION", "LEADER", "MULTILEADER", "MLEADER",
        "TOLERANCE", "HATCH", "TABLE" };
    static bool IsAnnotative(Entity e)
    {
        try { return AnnotativeNames.Contains(e.GetRXClass().DxfName); }
        catch { return false; }
    }

    // 视图指纹（视图中心 + 高度 + 转角）：用来判断「视图动过没有」。
    // 平移/缩放之后光标可能还停在同一个像素上，只靠「移动 >= 2 像素」的节流会漏掉重算，
    // 悬停贴纸就会留在旧位置、盖住平移后的别的图元（真机反馈的「中键漂移」就是这么来的）。
    static string ViewSignature()
    {
        try
        {
            var c = (ZwSoft.ZwCAD.Geometry.Point3d)CadApp.GetSystemVariable("VIEWCTR");
            return N(c.X) + "," + N(c.Y) + "," + Convert.ToString(CadApp.GetSystemVariable("VIEWSIZE"))
                 + "," + Convert.ToString(CadApp.GetSystemVariable("VIEWTWIST"));
        }
        catch { return ""; }
    }

    // UCS→WCS 矩阵，用来把 WCS 点换算成 ZWCAD 选择集接口要的 UCS 点。
    // ZWCAD 2020 的 UCSXDIR / UCSYDIR 返回的是 Point3d（AutoCAD 是 Vector3d，实测如此）。
    static bool TryWcsToUcs(out ZwSoft.ZwCAD.Geometry.Matrix3d w2u)
    {
        w2u = ZwSoft.ZwCAD.Geometry.Matrix3d.Identity;
        try
        {
            var org = (ZwSoft.ZwCAD.Geometry.Point3d)CadApp.GetSystemVariable("UCSORG");
            var px = (ZwSoft.ZwCAD.Geometry.Point3d)CadApp.GetSystemVariable("UCSXDIR");
            var py = (ZwSoft.ZwCAD.Geometry.Point3d)CadApp.GetSystemVariable("UCSYDIR");
            var xd = new ZwSoft.ZwCAD.Geometry.Vector3d(px.X, px.Y, px.Z);
            var yd = new ZwSoft.ZwCAD.Geometry.Vector3d(py.X, py.Y, py.Z);
            var u2w = ZwSoft.ZwCAD.Geometry.Matrix3d.AlignCoordinateSystem(
                ZwSoft.ZwCAD.Geometry.Point3d.Origin,
                ZwSoft.ZwCAD.Geometry.Vector3d.XAxis,
                ZwSoft.ZwCAD.Geometry.Vector3d.YAxis,
                ZwSoft.ZwCAD.Geometry.Vector3d.ZAxis,
                org, xd, yd, xd.CrossProduct(yd));
            w2u = u2w.Inverse();
            return true;
        }
        catch { return false; }
    }

    // 选择集调用：ZWCAD 2020 的 Editor.SelectCrossingWindow 收的是 **UCS** 点，不是 AutoCAD
    // 文档里写的 WCS（2026-09-21 真机实测：UCSORG=(2826.6,254.2) 的图纸里传 WCS 点拿到的是
    // PromptStatus.Error、一个对象都选不到；传 UCS 点立刻 OK。当时的合成测试图纸 UCS=WCS，
    // 所以侥幸通过，真实图纸里就表现为「悬停压到线上不变色」）。
    // 这里统一换成 UCS 再调；换算失败（拿不到 UCS 变量）就退回原样，保持旧行为。
    static PromptSelectionResult CrossWindow(Editor ed, ZwSoft.ZwCAD.Geometry.Point3d a, ZwSoft.ZwCAD.Geometry.Point3d b)
    {
        ZwSoft.ZwCAD.Geometry.Matrix3d w2u;
        if (TryWcsToUcs(out w2u))
        {
            try { return ed.SelectCrossingWindow(a.TransformBy(w2u), b.TransformBy(w2u)); }
            catch { }
        }
        return ed.SelectCrossingWindow(a, b);
    }

    // 鼠标当前在绘图区里的比例坐标（0,0 = 左下角），与 LISP 的 ze:view / ze:wpts 一套。
    static bool CursorFrac(IntPtr canvas, int width, int height, out double fx, out double fy)
    {
        fx = fy = 0.0;
        Point origin = new Point(0, 0);
        if (canvas == IntPtr.Zero || !ClientToScreen(canvas, ref origin)) return false;
        Point p = Cursor.Position;
        int x = p.X-origin.X, y = p.Y-origin.Y;
        fx = (double)x/width;
        fy = 1.0-(double)y/height;
        return x >= 0 && y >= 0 && x < width && y < height;
    }

    static bool Frac(TypedValue[] a, out double fx, out double fy)
    {
        fx = fy = 0.0;
        if (a == null || a.Length < 2) return false;
        try { fx = Convert.ToDouble(a[0].Value); fy = Convert.ToDouble(a[1].Value); }
        catch { return false; }
        return true;
    }

    // 拖动修剪 / 拖动延伸时的笔画。画在独立的置顶透明窗上：CAD 的画布是 GPU 合成的，
    // 直接往画布 DC 上画会被它自己的重绘冲掉（1.2.34/1.2.35 真机实测），透明窗才稳。
    // 做法与悬停高亮一样，只是内容换成整条折线，并且每次只重画笔画的外接矩形。
    sealed class InkPreview : IDisposable
    {
        const int PenWidth = 2;
        static readonly Color Cyan = Color.FromArgb(0, 232, 255);
        static readonly Color Orange = Color.FromArgb(255, 96, 64);
        readonly HoverPreview.Sticker sticker = new HoverPreview.Sticker();
        readonly List<PointF> pts = new List<PointF>();
        IntPtr canvas = IntPtr.Zero;
        Point origin = Point.Empty;
        bool orange;
        DateTime stamp = DateTime.MinValue;
        internal bool Empty { get { return pts.Count == 0; } }

        internal void Add(IntPtr h, double fx, double fy, int width, int height, bool red)
        {
            if (Empty)
            {
                canvas = h;
                origin = new Point(0, 0);
                ClientToScreen(h, ref origin);
            }
            orange = red;
            pts.Add(new PointF((float)(fx*width), (float)((1.0-fy)*height)));
            Render(false);
        }

        // 节流 35 毫秒：每个鼠标事件都去贴一次大位图吃不消；松开时那一笔会被 End() 直接撤掉，
        // 所以这里不用补最后一帧。
        void Render(bool force)
        {
            if (canvas == IntPtr.Zero || pts.Count == 0) return;
            if (!force && (DateTime.UtcNow-stamp).TotalMilliseconds < 35) return;
            stamp = DateTime.UtcNow;
            try
            {
                int pad = PenWidth+1;
                float l = float.MaxValue, t = float.MaxValue, r = float.MinValue, b = float.MinValue;
                foreach (PointF q in pts)
                {
                    if (q.X < l) l = q.X; if (q.X > r) r = q.X;
                    if (q.Y < t) t = q.Y; if (q.Y > b) b = q.Y;
                }
                int x0 = (int)Math.Floor(l)-pad, y0 = (int)Math.Floor(t)-pad;
                int w = (int)Math.Ceiling(r)-x0+pad+1, h = (int)Math.Ceiling(b)-y0+pad+1;
                if (w < 1 || h < 1 || w > 8000 || h > 8000) return;
                using (Bitmap bmp = new Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb))
                {
                    using (Graphics g = Graphics.FromImage(bmp))
                    {
                        g.Clear(Color.Transparent);
                        g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
                        using (Pen pen = new Pen(orange ? Orange : Cyan, PenWidth))
                        {
                            if (pts.Count == 1)
                            {
                                g.DrawLine(pen, pts[0].X-x0, pts[0].Y-y0, pts[0].X-x0+0.1f, pts[0].Y-y0);
                            }
                            else
                            {
                                var q = new PointF[pts.Count];
                                for (int i = 0; i < pts.Count; i++) q[i] = new PointF(pts[i].X-x0, pts[i].Y-y0);
                                g.DrawLines(pen, q);
                            }
                        }
                    }
                    HoverPreview.Paste(sticker, bmp, origin.X+x0, origin.Y+y0, w, h);
                }
            }
            catch { }
        }

        internal void End() { pts.Clear(); try { sticker.Show(false); } catch { } }
        public void Dispose() { try { sticker.Dispose(); } catch { } }
    }

    // (ZWK_HOVER_101 fx fy [shift]) —— fx/fy 是 0~1 的绘图区比例坐标（0,0 = 左下角，与 ze:frac 一致）；
    // shift = 1 表示此刻按住 Shift（延伸），不做「剪掉哪一段」的预览。
    // 返回 "OK;<命中对象数>" / "OFF" / "NOCANVAS" / "NOSCREEN" / "ERR:..."。
    [LispFunction("ZWK_HOVER_101")]
    public static ResultBuffer HoverCheck2(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            double fx, fy;
            if (!Frac(a, out fx, out fy)) return Status("ERROR");
            bool shift = false;
            if (a.Length > 2)
                try { shift = Convert.ToInt32(a[2].Value) != 0; } catch { }
            int w, h;
            if (!ScreenSize(out w, out h)) return Status("NOSCREEN");
            IntPtr canvas = FindCanvas(w, h);
            if (canvas == IntPtr.Zero) return Status("NOCANVAS");
            // 一律用真实硬件光标位置，不用 grread 送来的点：平移/缩放之后 grread 可能把
            // 平移前的旧点再送一次，拿它画方块就会在错位处闪一下（2026-09-21 用户反馈）。
            double ux, uy;
            if (CursorFrac(canvas, w, h, out ux, out uy)) { fx = ux; fy = uy; }
            else { if (hover != null) hover.Clear(); return Status("OFF"); }   // 光标不在绘图区
            bool inside = fx >= 0.0 && fx <= 1.0 && fy >= 0.0 && fy <= 1.0;
            if (hover == null) hover = new HoverPreview();
            hover.Attach(canvas);
            hover.Update(new Point((int)Math.Round(fx*w), (int)Math.Round((1.0-fy)*h)), w, h, inside, shift);
            return Status(inside ? "OK;" + hover.Hits : "OFF");
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    // (ZWK_BOUND_101 "句柄,句柄,...") —— 修剪/延伸的边界集合；空串 = 全部对象（快速模式的默认）。
    // 悬停预览要按它算「会被剪掉的那一段」，所以 LISP 侧每次边界变化后都要送一次。
    [LispFunction("ZWK_BOUND_101")]
    public static ResultBuffer Bound(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            string s = a.Length > 0 ? Convert.ToString(a[0].Value) : "";
            HoverPreview.SetBoundary(s);
            return Status("OK");
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    [LispFunction("ZWK_HOVER_END_101")]
    public static ResultBuffer HoverOff(ResultBuffer args)
    {
        try { if (hover != null) { hover.Clear(); hover.WatchOff(); } } catch { }
        return Status("OK");
    }

    // (ZWK_STATE_101) —— "1;fx;fy" = 左键按着 / "0;fx;fy" = 松着；"OUT" 鼠标不在绘图区。
    [LispFunction("ZWK_STATE_101")]
    public static ResultBuffer MouseState(ResultBuffer args)
    {
        try
        {
            int w, h;
            if (!ScreenSize(out w, out h)) return Status("NOSCREEN");
            double fx, fy;
            if (!CursorFrac(FindCanvas(w, h), w, h, out fx, out fy)) return Status("OUT");
            return Status((Down(1) ? "1;" : "0;") + N(fx) + ";" + N(fy));
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    // (ZWK_SHIFT_101) —— "1" 按着 Shift。
    [LispFunction("ZWK_SHIFT_101")]
    public static ResultBuffer ShiftState(ResultBuffer args) { return Status(Down(16) ? "1" : "0"); }

    // (ZWK_PRESS_101 fx fy [超时毫秒]) —— 左键按下之后的判定（fx/fy 是按下时的比例坐标）。
    //   "UP;fx;fy"    松开了（fx/fy = 松开的位置）
    //   "MOVED;fx;fy" 还按着，但已经动了 3.5 像素以上（＝拖动的开始）
    //   "HOLD;fx;fy"  超时了还按着、几乎没动
    //   "OUT"         跑出了绘图区
    // 阻塞时间是毫秒级（6 毫秒一查），只有「用户没动」时才可能等满整个超时；
    // 一旦动起来或者松手就立刻返回，准星几乎不会僵住。
    [LispFunction("ZWK_PRESS_101")]
    public static ResultBuffer PressWait(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            double fx, fy;
            if (!Frac(a, out fx, out fy)) return Status("ERROR");
            int w, h;
            if (!ScreenSize(out w, out h)) return Status("NOSCREEN");
            IntPtr canvas = FindCanvas(w, h);
            if (canvas == IntPtr.Zero) return Status("NOCANVAS");
            int timeout = a.Length > 2 ? Convert.ToInt32(a[2].Value) : 220;
            if (timeout < 0) timeout = 0;
            Point origin = new Point(0, 0);
            ClientToScreen(canvas, ref origin);
            double px = origin.X + fx*w, py = origin.Y + (1.0-fy)*h;
            DateTime until = DateTime.UtcNow.AddMilliseconds(timeout);
            while (true)
            {
                Point now = Cursor.Position;
                double dx = now.X-px, dy = now.Y-py;
                double cx, cy;
                bool inside = CursorFrac(canvas, w, h, out cx, out cy);
                if (Math.Sqrt(dx*dx+dy*dy) > 3.5)
                    return Status((inside ? "MOVED;" : "OUT;") + N(cx) + ";" + N(cy));
                if (!Down(1)) return Status("UP;" + N(cx) + ";" + N(cy));
                if (DateTime.UtcNow >= until) return Status("HOLD;" + N(cx) + ";" + N(cy));
                Thread.Sleep(6);
            }
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    // (ZWK_INK_101 fx fy red) —— 记一个笔画点并重画整条笔画；red = 1 画橘红色（按住 Shift 的延伸）。
    [LispFunction("ZWK_INK_101")]
    public static ResultBuffer InkAdd(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            double fx, fy;
            if (!Frac(a, out fx, out fy)) return Status("ERROR");
            bool red = a.Length > 2 && Convert.ToInt32(a[2].Value) != 0;
            int w, h;
            if (!ScreenSize(out w, out h)) return Status("NOSCREEN");
            IntPtr canvas = FindCanvas(w, h);
            if (canvas == IntPtr.Zero) return Status("NOCANVAS");
            if (ink == null) ink = new InkPreview();
            ink.Add(canvas, fx, fy, w, h, red);
            return Status("OK");
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    [LispFunction("ZWK_INK_END_101")]
    public static ResultBuffer InkEnd(ResultBuffer args)
    {
        try { if (ink != null) ink.End(); } catch { }
        return Status("OK");
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
            hover.Update(new Point(x,y), w, h, true, false);
            n = hover.Hits; box = hover.Bounds; paper = hover.Paper;
            DateTime until = DateTime.UtcNow.AddMilliseconds(hold);
            while (DateTime.UtcNow < until) { Application.DoEvents(); Thread.Sleep(10); }
        }
        return Status("HOVER_HITS=" + n + ";BOX=" + box + ";PAPER=" + paper);
    }

    // 自检：把悬停坐标换算用到的几个量原样吐出来（VIEWCTR / UCSORG / view.CenterPoint /
    // Target 等），用来定位「UCS 原点不在 (0,0) 的图纸里悬停不亮」这类问题。
    // 返回 "key=value;key=value;..."，不参与 TR / EX 的正常流程。
    [LispFunction("ZWK_VIEW_101")]
    public static ResultBuffer ViewDiag(ResultBuffer args)
    {
        try
        {
            var ed = CadApp.DocumentManager.MdiActiveDocument.Editor;
            var sb = new System.Text.StringBuilder();
            object vc = CadApp.GetSystemVariable("VIEWCTR");
            object uo = CadApp.GetSystemVariable("UCSORG");
            sb.Append("VIEWCTR=" + Convert.ToString(vc) + "[" + (vc == null ? "null" : vc.GetType().Name) + "]");
            sb.Append(";UCSORG=" + Convert.ToString(uo) + "[" + (uo == null ? "null" : uo.GetType().Name) + "]");
            int w, h;
            sb.Append(ScreenSize(out w, out h) ? ";SCREENSIZE=" + w + "x" + h : ";SCREENSIZE=none");
            ZwSoft.ZwCAD.Geometry.Matrix3d w2u;
            sb.Append(";WcsToUcs=" + TryWcsToUcs(out w2u));
            sb.Append(";PICKBOX=" + PickPixels());
            sb.Append(";FilterHits=" + ViewFilter.Hits + "/" + ViewFilter.Last);
            using (var view = ed.GetCurrentView())
            {
                sb.Append(";CenterPoint=" + N(view.CenterPoint.X) + "," + N(view.CenterPoint.Y));
                sb.Append(";Target=" + N(view.Target.X) + "," + N(view.Target.Y) + "," + N(view.Target.Z));
                sb.Append(";Height=" + N(view.Height) + ";Width=" + N(view.Width) + ";Twist=" + N(view.ViewTwist));
            }
            return Status(sb.ToString());
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    // 自检：新版（grread 交互层）返回 "READY2"；1.2.35 及以前返回 "READY"。
    // trimext.lsp 的 ze:ready 靠它判断 bin 里的 DLL 是不是配套的这一版。
    [LispFunction("ZWK_MODULE_101")]
    public static ResultBuffer ModuleCheck(ResultBuffer args) { return Status("READY2"); }

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
            if (hover != null) hover.Attach(nativeCanvas);
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
                    else hover.Update(navPoint, viewWidth, viewHeight, inside, false);
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





