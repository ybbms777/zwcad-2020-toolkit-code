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
            CadOverlay.SetPieces(null, false, Color.Empty);
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

        // ---- 滚轮残影（2026-09-22 用户反馈：悬停在线上时滚轮缩放，青色高亮留在原来的屏幕位置）----
        // grread 期间滚轮消息既不给 LISP、也不经过上面的 ViewFilter（消息过滤器只看 WinForms 消息泵），
        // 所以在看门狗线程上挂一个底层鼠标钩子（WH_MOUSE_LL）：系统级的滚轮事件一定经过它。
        // 一滚动就收起两个贴纸，并在 WheelQuietMs 内不重画（CAD 的缩放稍后才生效，太早重画会按旧视图
        // 再画出一个残影）；之后鼠标一动，Update 按新视图重算。钩子回调必须极快，只做「记时间 + 异步隐藏」。
        internal static int WheelHookHits;                  // 自检用：钩子收到几次滚轮（ZWK_VIEW_101 报出）
        const int WheelQuietMs = 250;
        volatile int wheelTick = Environment.TickCount - 100000;
        LowLevelMouseProc hookProc;                          // 必须存成字段，防止委托被 GC 回收

        IntPtr OnLowLevelMouse(int nCode, IntPtr wParam, IntPtr lParam)
        {
            try
            {
                int msg = wParam.ToInt32();
                if (nCode >= 0 && (msg == 0x020A || msg == 0x020E) && OurWindow(GetForegroundWindow()))
                {
                    wheelTick = Environment.TickCount;
                    WheelHookHits++;
                    // 预览、栏选虚线都由 CAD 自己画（CadOverlay），随缩放一起重画，这里不用收。
                    // 拾取框贴在光标上，滚轮不移动光标，位置依然正确，也不用收。
                    viewChangeTick = Environment.TickCount;
                    viewPostPending = true;
                }
                // 拖动中松开左键：grread 不产生「松开」事件，LISP 要等鼠标再动一下才知道松手了
                // （2026-09-22 真机：松手后不动鼠标，这一笔就一直不执行）。这里补发一个「原地移动」，
                // 让 grread 立刻醒来、LISP 马上结算这一笔 —— 与 AutoCAD「松手即执行」一致。
                if (nCode >= 0 && msg == 0x0202)
                {
                    LButtonUpHits++;
                    if (DragActive) { upTick = Environment.TickCount; PostMoveLater = true; postStage = 0; }
                }
            }
            catch { }
            return CallNextHookEx(IntPtr.Zero, nCode, wParam, lParam);
        }

        // 由 LISP 侧的拖动笔画置位 / 清除（见 ZWK_INK_101 / ZWK_INK_END_101）
        internal static volatile bool DragActive;
        // 需要补发「原地移动」：看门狗线程下一轮就发（钩子回调里不做耗时的事）
        internal static volatile bool PostMoveLater;
        static volatile int upTick, postStage;
        // 悬停因节流被跳过：鼠标若就此停住，最终位置永远算不到 —— 记下来，稍后补发一次移动
        volatile bool hoverPending;
        volatile int hoverPendingTick;
        internal static int SyntheticMoves, LButtonUpHits, HookInstalls, HookFails;   // 自检用（ZWK_VIEW_101 报出）
        bool postFlip;

        // 给绘图区投递一个「光标原地移动」消息（坐标 = 真实光标位置），grread 会把它当成一次移动事件。
        void PostCursorMove()
        {
            IntPtr c = canvas;
            if (c == IntPtr.Zero) return;
            POINT pt;
            if (!GetCursorPos(out pt)) return;
            Point cp = new Point(pt.X, pt.Y);
            if (!ScreenToClient(c, ref cp)) return;
            // 偏 1 像素（每次 +1 / -1 交替）：与上一次位置完全相同的移动消息会被 ZWCAD 当重复丢掉，
            // grread 醒不过来（2026-09-22 真机日志：补发了、但 grread 没收到）。
            // 插件取位置一律用真实光标（CursorFrac），这 1 像素不影响任何结果。
            postFlip = !postFlip;
            int x = cp.X + (postFlip ? 1 : -1);
            PostMessage(c, 0x0200, IntPtr.Zero, (IntPtr)((cp.Y << 16) | (x & 0xFFFF)));
            SyntheticMoves++;
        }

        // ---- 视图巡检定时器（主线程）----
        // 钩子只能在「滚轮那一刻」收起贴纸；ZWCAD 的滚轮缩放带过渡动画，若安静期过后、动画还没结束时
        // 鼠标动了一下，Update 会按缩放前的视图重画，动画结束后就留下错位的高亮 —— 而 grread 期间
        // 没有任何事件再来驱动重算（2026-09-22 用户截图：青线偏在真实线条旁边）。
        // 所以在主线程挂一个 60ms 的 Win32 定时器（TIMERPROC 由 CAD 自己的消息泵派发，跑在主线程上，
        // 读系统变量是安全的）：视图指纹与画高亮时不一致就立刻收起，并作废签名，鼠标一动按新视图重算。
        internal static int TimerTicks, StaleHides;         // 自检用（ZWK_VIEW_101 报出）
        IntPtr viewTimer = IntPtr.Zero;
        TimerProc viewTimerProc;                             // 存成字段，防止委托被 GC 回收

        // 视图变化（滚轮 / 中键平移 / 其它）后：先全部收起，视图稳定 300ms 后补发一次移动，自动按新视图重画
        volatile bool viewPostPending;
        volatile int viewChangeTick = Environment.TickCount - 100000;
        string seenSig = null;

        void OnViewTimer(IntPtr h, uint msg, IntPtr id, uint time)
        {
            try
            {
                TimerTicks++;
                if (Down(4)) return;                      // 中键按住 = 正在平移：松手后下一拍再处理
                string vs = ViewSignature();
                if (seenSig != null && vs != seenSig)
                {
                    // 预览 / 栏选虚线是 CAD 的临时图形（WCS），随视图一起重画，不用收；
                    // 只按新比例重算虚线段长和红 × 大小。拾取框贴在光标上，位置不受缩放影响。
                    sig = ""; viewSig = "";
                    StaleHides++;
                    viewChangeTick = Environment.TickCount;
                    viewPostPending = true;               // 视图停稳后补发一次移动：橡皮筋末端 / 悬停按新视图更新
                    try
                    {
                        var ss = (ZwSoft.ZwCAD.Geometry.Point2d)CadApp.GetSystemVariable("SCREENSIZE");
                        if (ss.Y >= 1) CadOverlay.Rescale(Convert.ToDouble(CadApp.GetSystemVariable("VIEWSIZE")) / ss.Y);
                    }
                    catch { }
                }
                seenSig = vs;
            }
            catch { }   // 定时器回调里的异常会直接打崩 CAD，必须全部吞掉
        }

        void TimerOn()
        {
            if (viewTimer == IntPtr.Zero)
            {
                seenSig = null;
                viewPostPending = false;
                viewTimerProc = OnViewTimer;
                try { viewTimer = SetTimer(IntPtr.Zero, IntPtr.Zero, 60, viewTimerProc); } catch { viewTimer = IntPtr.Zero; }
            }
        }

        void TimerOff()
        {
            if (viewTimer != IntPtr.Zero)
            {
                try { KillTimer(IntPtr.Zero, viewTimer); } catch { }
                viewTimer = IntPtr.Zero;
            }
        }

        // 看门狗：只在 TR / EX 交互期间跑（第一次 Update 起、每次 capture 结束（hover-end）停）。
        void Watch()
        {
            IntPtr hook = IntPtr.Zero;
            try
            {
                hookProc = OnLowLevelMouse;
                hook = SetWindowsHookEx(14, hookProc, GetModuleHandle(null), 0);   // 14 = WH_MOUSE_LL
            }
            catch { hook = IntPtr.Zero; }
            if (hook != IntPtr.Zero) HookInstalls++; else HookFails++;
            try { WatchLoop(); }
            finally { if (hook != IntPtr.Zero) { try { UnhookWindowsHookEx(hook); } catch { } } }
        }

        void WatchLoop()
        {
            MSG m;
            while (!watchStop)
            {
                // 底层钩子的回调要靠本线程的消息泵送达：有消息立刻醒来处理，没有就最多等 25 毫秒
                // 做下面的中键 / 前台检查（原来是 Sleep(25)，会把钩子回调拖住）。
                try
                {
                    MsgWaitForMultipleObjects(0, null, false, 25, 0x04FF);   // QS_ALLINPUT
                    while (PeekMessage(out m, IntPtr.Zero, 0, 0, 1)) { }       // PM_REMOVE
                }
                catch { Thread.Sleep(25); }
                try
                {
                    // 松手后补发移动要「晚一点」：钩子回调比 WM_LBUTTONUP 进队列还早，而投递的消息
                    // 又会先于输入消息被取走 —— 太早发，CAD 处理它时还以为左键按着，grread 不当回事
                    // （2026-09-22 真机：立刻补发叫不醒 grread）。60ms 发一次，150ms 再保底一次。
                    if (PostMoveLater)
                    {
                        int dt = unchecked(Environment.TickCount - upTick);
                        if (postStage == 0 && dt >= 60) { PostCursorMove(); postStage = 1; }
                        else if (postStage == 1 && dt >= 150) { PostCursorMove(); postStage = 2; PostMoveLater = false; }
                    }
                    if (viewPostPending && unchecked(Environment.TickCount - viewChangeTick) > 300
                                        && unchecked(Environment.TickCount - wheelTick) > 300)
                    {
                        viewPostPending = false;
                        PostCursorMove();
                    }
                    if (hoverPending && unchecked(Environment.TickCount - hoverPendingTick) > 70)
                    {
                        hoverPending = false;
                        PostCursorMove();
                    }
                }
                catch { }
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
            }
        }

        internal bool WatchAlive { get { Thread t = watch; return t != null && t.IsAlive; } }

        internal void WatchOn()
        {
            TimerOn();
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
            TimerOff();
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
            // 刚滚过滚轮：CAD 的缩放可能还没生效，这时重画会按旧视图画出新的残影 —— 先保持隐藏，
            // 等安静期过后鼠标再动时按新视图全量重算（Clear 会作废签名，保证真的重画）。
            if (unchecked(Environment.TickCount - wheelTick) < WheelQuietMs) return;   // 缩放还没生效：先不算（停稳后看门狗会补发一次移动）
            // 视图动过（平移/缩放）就作废重算：平移后光标可能还在同一个像素上，
            // 只靠「移动 >= 2 像素」的节流会漏掉重算，贴纸会盖在别的图元上。
            string vs = ViewSignature();
            if (vs != viewSig) { viewSig = vs; Clear(); }
            PickBox(p);                                  // 方块每帧跟手，不受下面两道节流限制
            if (Math.Abs(p.X-last.X) < 2 && Math.Abs(p.Y-last.Y) < 2) return;
            if ((DateTime.UtcNow-stamp).TotalMilliseconds < 45)
            {
                // 被节流跳过：若鼠标就此停住，这个位置就再也不会算到（预览不出现）——让看门狗稍后补发一次移动
                hoverPendingTick = Environment.TickCount;
                hoverPending = true;
                return;
            }
            hoverPending = false;
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
            // shift 参数现在的含义是「延伸模式」：TR 按住 Shift、或 EX 不按 Shift 时为 true（LISP 侧算好传进来）。
            bool extend = shift;
            var shapes = new List<PointF[]>();
            var keep = new List<ObjectId>();
            var sb = new System.Text.StringBuilder(extend ? "E|" : "T|");
            using (var tr = doc.Database.TransactionManager.StartTransaction())
            {
                // 与 AutoCAD 一致：单击只会处理离拾取点最近的那一个对象，所以预览也只画它
                // （原来拾取框里压到几个就亮几个，与实际结果对不上）。
                Entity best = null; double bestD = double.MaxValue;
                foreach (ObjectId id in ids)
                    try
                    {
                        Entity ent = tr.GetObject(id, OpenMode.ForRead) as Entity;
                        if (ent == null) continue;
                        // 标注类不参与预览：没有「会被剪掉的那一段」，画外框只会变成
                        // 盖住半张图的大方框（2026-09-21 用户反馈「这个很明显不对」）。
                        if (IsAnnotative(ent)) continue;
                        double dd = double.MaxValue - 1;
                        Curve cv0 = ent as Curve;
                        if (cv0 != null) { try { dd = cv0.GetClosestPointTo(cursor, false).DistanceTo(cursor); } catch { } }
                        if (dd < bestD) { bestD = dd; best = ent; }
                    }
                    catch { }   // 不在图里 / 打不开的对象直接放过
                if (best != null)
                    try
                    {
                        PointF[] poly; string what = "";
                        bool drew = false;
                        if (!extend)
                        {
                            // 修剪：要剪掉的那一段；一个交点都没有（快速模式下点下去会被删除）就整条
                            if (Piece(best, cursor, pj, tr, out poly, out what)) { shapes.Add(poly); drew = true; }
                            else { Shape(best, pj, shapes); what = "*"; drew = true; }
                        }
                        else if (Extension(best, cursor, pj, tr, out poly, out what)) { shapes.Add(poly); drew = true; }
                        if (drew)
                        {
                            keep.Add(best.ObjectId);
                            sb.Append(best.ObjectId.ToString()).Append(':').Append(what).Append(';');
                        }
                    }
                    catch { }
            }
            // 亮起来的还是同一段就别动它，免得闪（同一个对象、同一段参数区间才算没变）。
            string s = sb.ToString();
            if (s == sig) return;
            sig = s;
            Wipe();
            lit.AddRange(keep);
            extendMode = extend;
            var wshapes = new List<ZwSoft.ZwCAD.Geometry.Point3d[]>();
            foreach (var s0 in shapes) wshapes.Add(pj.W(s0));
            Render(wshapes);
        }

        // ---- 延伸预览（EX，或 TR 按住 Shift）----
        // 与 AutoCAD 一致：从离拾取点较近的那个端点出发，沿对象自身方向延伸到最近的边界，
        // 画出「将要补上的那一段」。只支持直线和圆弧（法向 +Z）；找不到边界就不画。
        bool extendMode;

        static double NormAngle(double a)
        {
            while (a <= 0) a += 2 * Math.PI;
            while (a > 2 * Math.PI) a -= 2 * Math.PI;
            return a;
        }

        bool Extension(Entity ent, ZwSoft.ZwCAD.Geometry.Point3d near, Projector pj, Transaction tr,
                       out PointF[] poly, out string what)
        {
            poly = null; what = "";
            Line ln = ent as Line; Arc ar = ent as Arc;
            if (ln == null && ar == null) return false;
            if (ar != null && ar.Normal.Z < 0) return false;          // 镜像过的圆弧（OCS 反向）暂不预览
            Curve cv = (Curve)ent;
            var s = cv.StartPoint; var e = cv.EndPoint;
            bool atEnd = near.DistanceTo(e) <= near.DistanceTo(s);
            double best = double.MaxValue; var bestPt = ZwSoft.ZwCAD.Geometry.Point3d.Origin; bool found = false;
            double span = ar != null ? NormAngle(ar.EndAngle - ar.StartAngle) : 0;
            foreach (ObjectId id in ExtendCandidates(tr))
            {
                if (id == ent.ObjectId) continue;
                Entity b;
                try { b = tr.GetObject(id, OpenMode.ForRead) as Entity; } catch { continue; }
                if (b == null) continue;
                var pts = new ZwSoft.ZwCAD.Geometry.Point3dCollection();
                try { ent.IntersectWith(b, Intersect.ExtendThis, pts, IntPtr.Zero, IntPtr.Zero); } catch { continue; }
                foreach (ZwSoft.ZwCAD.Geometry.Point3d q in pts)
                {
                    double m;
                    if (ln != null)
                    {
                        var dir = atEnd ? (e - s) : (s - e);
                        double len = dir.Length; if (len < 1e-9) continue;
                        dir = dir / len;
                        m = (q - (atEnd ? e : s)).DotProduct(dir);
                        if (m <= 1e-7) continue;
                    }
                    else
                    {
                        double ang = Math.Atan2(q.Y - ar.Center.Y, q.X - ar.Center.X);
                        m = atEnd ? NormAngle(ang - ar.EndAngle) : NormAngle(ar.StartAngle - ang);
                        if (m <= 1e-7 || m >= 2 * Math.PI - span - 1e-7) continue;   // 必须落在圆弧的缺口里
                    }
                    if (m < best) { best = m; bestPt = q; found = true; }
                }
            }
            if (!found) return false;
            if (ln != null) poly = new PointF[] { pj.P(atEnd ? e : s), pj.P(bestPt) };
            else
            {
                double a0 = atEnd ? ar.EndAngle : ar.StartAngle - best;
                poly = Samples(pj, ar.Center, ar.Radius, ar.Radius, a0, a0 + best);
            }
            what = "x" + (atEnd ? "e" : "s") + best.ToString("0.#####", CultureInfo.InvariantCulture);
            return true;
        }

        // 延伸的候选边界：当前屏幕里的全部曲线（边界集合过滤后）。同一视图、同一边界集合下缓存。
        List<ObjectId> extCache;
        string extView = null; int extVer = -1;
        List<ObjectId> ExtendCandidates(Transaction tr)
        {
            if (extCache != null && extView == viewSig && extVer == boundaryVer) return extCache;
            var list = new List<ObjectId>();
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc != null && viewOk)
            {
                // 选择窗口往里缩 0.5%：窗口正好贴着视口边缘时，ZWCAD 的选择接口会直接失败
                // （2026-09-22 真机：EX 悬停「命中 0」、没有延伸预览）。
                double ix = 0.005 * (viewHi.X - viewLo.X), iy = 0.005 * (viewHi.Y - viewLo.Y);
                var lo = new ZwSoft.ZwCAD.Geometry.Point3d(viewLo.X + ix, viewLo.Y + iy, 0);
                var hi = new ZwSoft.ZwCAD.Geometry.Point3d(viewHi.X - ix, viewHi.Y - iy, 0);
                ObjectId[] found = null;
                PromptSelectionResult r = null;
                try { r = CrossWindow(doc.Editor, lo, hi); } catch { }
                if (r != null && r.Status == PromptStatus.OK && r.Value != null)
                    try { found = r.Value.GetObjectIds(); } finally { r.Value.Dispose(); }
                bool viaAll = false;
                if (found == null)
                {
                    // 兜底：全图选，再按包围盒只留屏幕内的
                    try
                    {
                        var ra = doc.Editor.SelectAll();
                        if (ra.Status == PromptStatus.OK && ra.Value != null)
                            try { found = ra.Value.GetObjectIds(); viaAll = true; } finally { ra.Value.Dispose(); }
                    }
                    catch { }
                }
                if (found != null)
                    foreach (ObjectId id in found)
                        try
                        {
                            Entity en = tr.GetObject(id, OpenMode.ForRead) as Entity;
                            if (!(en is Curve) || IsAnnotative(en) || !InBoundary(en)) continue;
                            if (viaAll)
                            {
                                var ex = en.GeometricExtents;
                                if (ex.MaxPoint.X < viewLo.X || ex.MinPoint.X > viewHi.X ||
                                    ex.MaxPoint.Y < viewLo.Y || ex.MinPoint.Y > viewHi.Y) continue;
                            }
                            list.Add(id);
                        }
                        catch { }
            }
            extCache = list; extView = viewSig; extVer = boundaryVer;
            return list;
        }

        // 栏选 / 拖动预览的入口：fr 是 0~1 的绘图区比例坐标（0,0 = 左下角，与 LISP 的 ze:frac 一致）。
        // 用与 Update 相同的视图换算把它们变成 WCS，并顺手刷新屏幕范围（邻居 / 延伸候选要用）。
        internal void FenceFrac(IntPtr canvasHandle, List<double[]> fr, int width, int height, bool extend, bool live)
        {
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc == null || width < 1 || height < 1 || fr.Count < 1) return;
            var wpts = new List<ZwSoft.ZwCAD.Geometry.Point3d>();
            Projector pj;
            try
            {
                using (var view = doc.Editor.GetCurrentView())
                {
                    double vh = view.Height, vw = vh*width/height;
                    var t = ZwSoft.ZwCAD.Geometry.Matrix3d.PlaneToWorld(view.ViewDirection);
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Displacement(view.Target-ZwSoft.ZwCAD.Geometry.Point3d.Origin)*t;
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Rotation(-view.ViewTwist,view.ViewDirection,view.Target)*t;
                    foreach (var f in fr)
                        wpts.Add(new ZwSoft.ZwCAD.Geometry.Point3d(view.CenterPoint.X + (f[0]-0.5)*vw,
                                                                  view.CenterPoint.Y + (f[1]-0.5)*vh, 0).TransformBy(t));
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
            Fence(canvasHandle, wpts, pj, extend, live);
        }

        // ---- 栏选 / 徒手拖动的实时预览 ----
        // pts 是 WCS 折线；与 AutoCAD 一致：被折线穿过的每个对象，按穿过点处理 ——
        // 修剪预览那一段（剪不动的整条），延伸预览从穿过点较近的端点延伸出去的那一段。
        // 增量计算（2026-09-22 真机：用户图纸里长轨迹每次从头重算要 94~187ms，主线程一卡整个系统的
        // 鼠标都卡 —— 「拖动不流畅」的根源）。现在：
        //   * 已确定的段（徒手笔画的全部段 / 栏选已点下的段）结果缓存，每次只算新增的段；
        //   * 橡皮筋那一段（最后一个栏选点 -> 光标）每次单独重算，不进缓存；
        //   * 每段只在自己的小包围窗里选对象，并先按 DXF 名跳过非曲线（不打开对象）；
        //   * 每次调用最多算 FenceBudgetMs，没算完的留到下一次（主线程不再长时间占住）。
        const int FenceBudgetMs = 15;
        string fKey;                                                   // 模式 + 视图：变了就整体作废
        readonly List<ZwSoft.ZwCAD.Geometry.Point3d> fPts = new List<ZwSoft.ZwCAD.Geometry.Point3d>();
        int fDone;                                                     // 已处理的「确定段」数
        readonly Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]> fPieces = new Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]>();
        Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]> lastLive = new Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]>();
        internal void FenceReset() { fKey = null; fPts.Clear(); fDone = 0; fPieces.Clear(); lastLive.Clear(); }

        static readonly HashSet<string> CurveNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase) {
            "LINE", "ARC", "CIRCLE", "LWPOLYLINE", "POLYLINE", "ELLIPSE", "SPLINE" };

        // 一段栏选线 a-b：被它穿过的每个曲线对象，按穿过点算修剪段 / 延伸段，结果放进 into。
        void FenceSegment(Editor ed, Transaction tr, ZwSoft.ZwCAD.Geometry.Point3d a, ZwSoft.ZwCAD.Geometry.Point3d b,
                          Projector pj, bool extend, Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]> into)
        {
            if (a.DistanceTo(b) < 1e-9) return;
            double pad = 1e-6 + 1e-3 * Math.Max(Math.Abs(b.X - a.X), Math.Abs(b.Y - a.Y));
            var ids = new List<ObjectId>();
            try
            {
                PromptSelectionResult r = CrossWindow(ed,
                    new ZwSoft.ZwCAD.Geometry.Point3d(Math.Min(a.X, b.X) - pad, Math.Min(a.Y, b.Y) - pad, 0),
                    new ZwSoft.ZwCAD.Geometry.Point3d(Math.Max(a.X, b.X) + pad, Math.Max(a.Y, b.Y) + pad, 0));
                if (r != null && r.Status == PromptStatus.OK && r.Value != null)
                    try { ids.AddRange(r.Value.GetObjectIds()); } finally { r.Value.Dispose(); }
            }
            catch { }
            if (ids.Count == 0) return;
            using (var seg = new Line(a, b))
                foreach (ObjectId id in ids)
                    try
                    {
                        string dxf = null;
                        try { dxf = id.ObjectClass.DxfName; } catch { }
                        if (dxf != null && !CurveNames.Contains(dxf)) continue;      // 文字 / 标注 / 块等：不打开
                        Entity ent = tr.GetObject(id, OpenMode.ForRead) as Entity;
                        if (ent == null || IsAnnotative(ent) || !(ent is Curve)) continue;
                        var hits = new ZwSoft.ZwCAD.Geometry.Point3dCollection();
                        try { ent.IntersectWith(seg, Intersect.OnBothOperands, hits, IntPtr.Zero, IntPtr.Zero); } catch { }
                        foreach (ZwSoft.ZwCAD.Geometry.Point3d q in hits)
                        {
                            PointF[] poly; string what;
                            if (!extend)
                            {
                                if (Piece(ent, q, pj, tr, out poly, out what)) into[id + ":" + what] = pj.W(poly);
                                else
                                {
                                    string k = id + ":*";
                                    if (!into.ContainsKey(k)) { var tmp = new List<PointF[]>(); Shape(ent, pj, tmp); if (tmp.Count > 0) into[k] = pj.W(tmp[0]); }
                                }
                            }
                            else if (Extension(ent, q, pj, tr, out poly, out what)) into[id + ":" + what] = pj.W(poly);
                        }
                    }
                    catch { }
        }

        // live = true：最后一个点是光标（橡皮筋），那一段每次重算、不缓存。
        void Fence(IntPtr canvasHandle, List<ZwSoft.ZwCAD.Geometry.Point3d> pts, Projector pj, bool extend, bool live)
        {
            Attach(canvasHandle);
            WatchOn();
            if (pick != null) pick.Show(false);           // 栏选时不显示拾取框（AutoCAD 也不显示）
            viewSig = ViewSignature();
            var doc = CadApp.DocumentManager.MdiActiveDocument;
            if (doc == null || pts.Count < 2) return;
            int committed = live ? pts.Count - 1 : pts.Count;           // 确定点的个数
            string key = extend ? "E|" : "T|";   // 结果是 WCS，与视图无关：缩放 / 平移后不用重算
            // 模式 / 视图变了、或不是同一条栏选（起点不同 / 点变少了，比如按了 U）就整体作废重来
            if (key != fKey || fPts.Count > committed || (fPts.Count > 0 && fPts[0].DistanceTo(pts[0]) > 1e-9))
            {
                FenceReset();
                fKey = key;
            }
            fPts.Clear();
            for (int i = 0; i < committed; i++) fPts.Add(pts[i]);
            var live1 = new Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]>();
            var sw = System.Diagnostics.Stopwatch.StartNew();
            using (var tr = doc.Database.TransactionManager.StartTransaction())
            {
                // 新增的确定段（有时间预算）
                while (fDone < committed - 1 && sw.ElapsedMilliseconds < FenceBudgetMs)
                {
                    FenceSegment(doc.Editor, tr, fPts[fDone], fPts[fDone + 1], pj, extend, fPieces);
                    fDone++;
                }
                // 橡皮筋段：最后一个确定点 -> 光标
                if (live) FenceSegment(doc.Editor, tr, pts[pts.Count - 2], pts[pts.Count - 1], pj, extend, live1);
            }
            FenceMsLast = (int)sw.ElapsedMilliseconds;
            if (FenceMsLast > FenceMsMax) FenceMsMax = FenceMsLast;
            if (fDone < committed - 1) hoverPending = true;              // 还有没算完的：看门狗稍后补一次移动，接着算
            var shapes = new List<ZwSoft.ZwCAD.Geometry.Point3d[]>(fPieces.Values);
            var sb = new System.Text.StringBuilder(key).Append('#').Append(fPieces.Count).Append('#').Append(fDone);
            // 橡皮筋段的结果每次都是新算的：同一对象、同一段（键相同）就沿用上一次的数组，
            // CadOverlay 按数组认人，不会每动一下把穿过的几十条预览全删了重加（2026-09-22 真机：栏选时主线程 p90 42ms）。
            var nextLive = new Dictionary<string, ZwSoft.ZwCAD.Geometry.Point3d[]>();
            foreach (var kv in live1)
            {
                ZwSoft.ZwCAD.Geometry.Point3d[] arr;
                if (!lastLive.TryGetValue(kv.Key, out arr)) arr = kv.Value;
                nextLive[kv.Key] = arr;
                if (!fPieces.ContainsKey(kv.Key)) shapes.Add(arr);
                sb.Append(kv.Key).Append(';');
            }
            lastLive = nextLive;
            string s = sb.ToString();
            if (s == sig) return;
            sig = s;
            Wipe();
            extendMode = extend;
            Render(shapes);
        }
        internal static int FenceMsLast, FenceMsMax;                    // 自检用（ZWK_VIEW_101 报出）

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
                // 往视口里缩 0.5%：窗口贴着视口边缘时 ZWCAD 的选择接口会失败（见 ExtendCandidates）
                double ix = 0.005 * (viewHi.X - viewLo.X), iy = 0.005 * (viewHi.Y - viewLo.Y);
                p0 = new ZwSoft.ZwCAD.Geometry.Point3d(Math.Max(p0.X, viewLo.X + ix), Math.Max(p0.Y, viewLo.Y + iy), p0.Z);
                p1 = new ZwSoft.ZwCAD.Geometry.Point3d(Math.Min(p1.X, viewHi.X - ix), Math.Min(p1.Y, viewHi.Y - iy), p1.Z);
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

        // 绘图区背景色：在绘图区边缘取 16 个点，取出现次数最多的颜色（避开偶尔压到的图元）。
        // 同一视图内缓存；取不到就用 ZWCAD 默认的深色背景。
        Color bgCache = Color.Empty; string bgView = null;
        Color Background()
        {
            if (!bgCache.IsEmpty) return bgCache;           // 背景色一次 TR / EX 内不会变
            // 一次性截绘图区顶部 / 底部各一条 1 像素高的横条来统计（2026-09-22 真机：原来逐点 GetPixel
            // 取 16 个点，开着桌面合成时每个 GetPixel 都要等一次显存回读，合计约 290ms，
            // 每次缩放 / 平移后的第一下悬停或拖动都会卡一下）。
            var counts = new Dictionary<int, int>();
            try
            {
                RECT rc;
                if (canvas != IntPtr.Zero && GetClientRect(canvas, out rc) && rc.R > 20 && rc.B > 20)
                {
                    int w = rc.R - 16;
                    using (var strip = new Bitmap(w, 1, System.Drawing.Imaging.PixelFormat.Format32bppArgb))
                    using (var g = Graphics.FromImage(strip))
                        foreach (int yy in new int[] { 4, rc.B - 5 })
                        {
                            g.CopyFromScreen(origin.X + 8, origin.Y + yy, 0, 0, new Size(w, 1));
                            for (int x = 0; x < w; x += 7)
                            {
                                int key = strip.GetPixel(x, 0).ToArgb() & 0xFFFFFF;
                                int n; counts.TryGetValue(key, out n); counts[key] = n + 1;
                            }
                        }
                }
            }
            catch { }
            int bestKey = -1, bestN = 0;
            foreach (var kv in counts) if (kv.Value > bestN) { bestN = kv.Value; bestKey = kv.Key; }
            // key 是 ARGB 的低 24 位：R 在高位
            bgCache = bestKey < 0 ? Color.FromArgb(33, 40, 48)
                                  : Color.FromArgb((bestKey >> 16) & 0xFF, (bestKey >> 8) & 0xFF, bestKey & 0xFF);
            bgView = viewSig;
            return bgCache;
        }

        // 把算好的屏幕折线贴到置顶透明窗上。
        // 2026-09-22 起改由 CAD 自己画（临时图形，见 CadOverlay）：预览是 WCS 几何，缩放 / 平移时
        // CAD 与图纸一起重画，不会再有贴纸跟不上视图留下的残影；也省掉每次贴整块位图的开销。
        void Render(List<ZwSoft.ZwCAD.Geometry.Point3d[]> shapes)
        {
            if (shapes.Count == 0) { CadOverlay.SetPieces(null, false, Color.Empty); return; }
            Color faint = Color.FromArgb(230, 230, 230);
            if (!extendMode)
            {
                // 修剪预览 = AutoCAD 的样子：要剪掉的那一段「几乎消失」—— 在原线上方用接近背景的淡色重描一遍
                Color bgc = Background();
                faint = Color.FromArgb((bgc.R * 7 + 200 * 3) / 10, (bgc.G * 7 + 200 * 3) / 10, (bgc.B * 7 + 200 * 3) / 10);
            }
            CadOverlay.SetPieces(shapes, !extendMode, faint);
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
        internal sealed class Projector
        {
            readonly ZwSoft.ZwCAD.Geometry.Matrix3d back, fwd;
            readonly double cx, cy, vw, vh;
            readonly int width, height;
            internal Projector(ZwSoft.ZwCAD.Geometry.Matrix3d back, ZwSoft.ZwCAD.Geometry.Point2d center, double vw, double vh, int width, int height)
            { this.back = back; this.fwd = back.Inverse(); this.cx = center.X; this.cy = center.Y; this.vw = vw; this.vh = vh; this.width = width; this.height = height; }
            // 每个像素多少图纸单位（画虚线 / 红 × 用，让它们在屏幕上大小固定）
            internal double UnitsPerPixel { get { return vh / height; } }
            // 屏幕像素（绘图区客户坐标）-> WCS：P 的逆运算
            internal ZwSoft.ZwCAD.Geometry.Point3d W(PointF s)
            {
                return new ZwSoft.ZwCAD.Geometry.Point3d(cx + (s.X / width - 0.5) * vw, cy + (0.5 - s.Y / height) * vh, 0).TransformBy(fwd);
            }
            internal ZwSoft.ZwCAD.Geometry.Point3d[] W(PointF[] s)
            {
                var r = new ZwSoft.ZwCAD.Geometry.Point3d[s.Length];
                for (int i = 0; i < s.Length; i++) r[i] = W(s[i]);
                return r;
            }
            // 按当前视图造一个投影器（与 Update / FenceFrac 的换算完全相同）
            internal static Projector Current(int width, int height)
            {
                var doc = CadApp.DocumentManager.MdiActiveDocument;
                if (doc == null || width < 1 || height < 1) return null;
                using (var view = doc.Editor.GetCurrentView())
                {
                    double vh = view.Height, vw = vh * width / height;
                    var t = ZwSoft.ZwCAD.Geometry.Matrix3d.PlaneToWorld(view.ViewDirection);
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Displacement(view.Target - ZwSoft.ZwCAD.Geometry.Point3d.Origin) * t;
                    t = ZwSoft.ZwCAD.Geometry.Matrix3d.Rotation(-view.ViewTwist, view.ViewDirection, view.Target) * t;
                    return new Projector(t.Inverse(), view.CenterPoint, vw, vh, width, height);
                }
            }
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
    [DllImport("gdi32.dll")] static extern uint GetPixel(IntPtr dc, int x, int y);
    [DllImport("user32.dll")] static extern bool PostMessage(IntPtr h, uint msg, IntPtr w, IntPtr l);
    [DllImport("user32.dll")] static extern bool ScreenToClient(IntPtr h, ref Point p);
    [DllImport("user32.dll")] static extern bool GetCursorPos(out POINT p);
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
    delegate IntPtr LowLevelMouseProc(int nCode, IntPtr wParam, IntPtr lParam);
    delegate void TimerProc(IntPtr hwnd, uint msg, IntPtr id, uint time);
    [DllImport("user32.dll")] static extern IntPtr SetTimer(IntPtr hWnd, IntPtr id, uint ms, TimerProc fn);
    [DllImport("user32.dll")] static extern bool KillTimer(IntPtr hWnd, IntPtr id);
    [DllImport("user32.dll", SetLastError = true)] static extern IntPtr SetWindowsHookEx(int idHook, LowLevelMouseProc fn, IntPtr hMod, uint threadId);
    [DllImport("user32.dll")] static extern bool UnhookWindowsHookEx(IntPtr hook);
    [DllImport("user32.dll")] static extern IntPtr CallNextHookEx(IntPtr hook, int nCode, IntPtr wParam, IntPtr lParam);
    [DllImport("user32.dll")] static extern uint MsgWaitForMultipleObjects(uint count, IntPtr[] handles, bool waitAll, uint ms, uint wakeMask);
    [DllImport("user32.dll")] static extern bool PeekMessage(out MSG msg, IntPtr h, uint min, uint max, uint remove);
    [StructLayout(LayoutKind.Sequential)] struct MSG { public IntPtr hwnd; public uint message; public IntPtr wParam; public IntPtr lParam; public uint time; public int ptX; public int ptY; }
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
    // ---- CAD 自己画的临时图形（TransientManager）----
    // 栏选虚线、起点红 ×、修剪「变暗」/ 延伸预览都放在这一个临时图形里，坐标是 WCS。
    // 原来用置顶透明贴纸：贴纸是屏幕像素，CAD 缩放 / 平移（尤其带过渡动画的快速滚轮）时永远慢一拍，
    // 收起再重画也总会漏出残影（2026-09-22 用户：「只要速度快了就会出现，很影响体验」）。
    // 临时图形由 CAD 在重画视图时一起画，天然与图纸同步；只能在主线程上调用（LISP 函数 / 主线程定时器）。

    static class CadOverlay
    {
        // ZWCAD 2020 不支持托管的自定义 Drawable（new Drawable() 在 DisposableWrapper.Attach 里直接抛
        // 「对象的当前状态使该操作无效」，2026-09-22 真机），所以用不入库的普通实体（Line / Polyline）当临时图形。
        // 虚线两种画法：
        //   1. 图里有虚线线型（HIDDEN / DASHED 之类，工程图基本都有）：整条路径一根多段线套这个线型 —— 一根实体，最省；
        //   2. 没有：一池短 Line，每段虚线一根，只增删 / 挪动变了的那几根。
        // 橡皮筋每动一下整条都在变，第 2 种要挪几十上百根，主线程明显变重（2026-09-22 真机：p90 从 7ms 涨到 40ms），
        // 所以优先第 1 种；第 2 种限制根数（路径长了就把每段拉长）。
        static readonly Dictionary<ZwSoft.ZwCAD.Geometry.Point3d[], Entity[]> pieceMap = new Dictionary<ZwSoft.ZwCAD.Geometry.Point3d[], Entity[]>(RefEq.I);
        static string pieceStyle;
        static readonly List<Line> dashes = new List<Line>();   // 池
        static int dashOn;                                     // 池里前 dashOn 根已加到屏幕上
        static readonly Line[] cross = new Line[4];
        static bool crossOn;
        static readonly List<ZwSoft.ZwCAD.Geometry.Point3d> ink = new List<ZwSoft.ZwCAD.Geometry.Point3d>();
        static double upp = 1;                                 // 每像素图纸单位：虚线段长、红 × 大小按屏幕像素固定
        internal static int Pushes, PushFails;                 // 自检用（ZWK_VIEW_101 报出）
        internal static string LastError = "-";
        internal static int MaxDashes = 80;
        internal static bool NoLinetype;                       // 调试开关（ZWK_OPT_101）：强制走短线池
        static Polyline pathEnt;                               // 画法 1 的那根多段线
        static bool pathOn;
        static Database ltDb; static ObjectId ltId = ObjectId.Null; static double ltLen; static bool ltBroken;

        static ZwSoft.ZwCAD.GraphicsInterface.TransientManager TM
        { get { return ZwSoft.ZwCAD.GraphicsInterface.TransientManager.CurrentTransientManager; } }
        static readonly ZwSoft.ZwCAD.Geometry.IntegerCollection All = new ZwSoft.ZwCAD.Geometry.IntegerCollection();
        const ZwSoft.ZwCAD.GraphicsInterface.TransientDrawingMode Mode = ZwSoft.ZwCAD.GraphicsInterface.TransientDrawingMode.DirectTopmost;

        static ZwSoft.ZwCAD.Colors.Color C(Color c) { return ZwSoft.ZwCAD.Colors.Color.FromRgb(c.R, c.G, c.B); }

        static void Add(Entity e) { if (!TM.AddTransient(e, Mode, 128, All)) { PushFails++; LastError = "AddTransient=false"; } Pushes++; }
        static void Erase(Entity e) { try { TM.EraseTransient(e, All); } catch { } }

        static Polyline Poly(ZwSoft.ZwCAD.Geometry.Point3d[] p, Color c, LineWeight lw)
        {
            var pl = new Polyline();
            for (int i = 0; i < p.Length; i++) pl.AddVertexAt(i, new ZwSoft.ZwCAD.Geometry.Point2d(p[i].X, p[i].Y), 0, 0, 0);
            pl.Color = C(c);
            pl.LineWeight = lw;
            return pl;
        }

        internal static void SetPieces(List<ZwSoft.ZwCAD.Geometry.Point3d[]> p, bool fade, Color c) { SetPieces(p, fade, c, Color.Empty); }
        internal static void SetPieces(List<ZwSoft.ZwCAD.Geometry.Point3d[]> p, bool fade, Color c, Color bg)
        {
            if ((p == null || p.Count == 0) && pieceMap.Count == 0) return;   // 本来就空：不惊动 CAD
            var sw = Stopwatch.StartNew();
            try
            {
                if (bg.IsEmpty) bg = Color.FromArgb(33, 40, 48);
                string style = fade + "|" + c.ToArgb() + "|" + bg.ToArgb();
                if (style != pieceStyle) { ClearPieces(); pieceStyle = style; }
                // 增量：栏选 / 拖动时已确定的段是同一批数组（缓存），只增删有变化的那几条，
                // 不再每动一下就把几十条全删了重加（2026-09-22 真机：整体重加让拖动慢到 4 秒 / 41 点）。
                var keep = new HashSet<ZwSoft.ZwCAD.Geometry.Point3d[]>(RefEq.I);
                if (p != null) foreach (var s in p) if (s != null && s.Length >= 2) keep.Add(s);
                var gone = new List<ZwSoft.ZwCAD.Geometry.Point3d[]>();
                foreach (var kv in pieceMap) if (!keep.Contains(kv.Key)) gone.Add(kv.Key);
                foreach (var k in gone) { foreach (var e in pieceMap[k]) { Erase(e); e.Dispose(); } pieceMap.Remove(k); }
                foreach (var s in keep)
                {
                    if (pieceMap.ContainsKey(s)) continue;
                    // 修剪「变暗」：先用背景色盖住原线（线宽显示打开时原线较粗，盖厚一点），再描一条淡线
                    var ents = fade ? new Entity[] { Poly(s, bg, LineWeight.LineWeight050), Poly(s, c, LineWeight.LineWeight000) }
                                    : new Entity[] { Poly(s, c, LineWeight.LineWeight000) };
                    foreach (var e in ents) Add(e);
                    pieceMap[s] = ents;
                }
            }
            catch (System.Exception ex) { PushFails++; LastError = ex.GetType().Name + ":" + ex.Message; }
            Note(sw);
        }

        static void ClearPieces()
        {
            foreach (var kv in pieceMap) foreach (var e in kv.Value) { Erase(e); e.Dispose(); }
            pieceMap.Clear();
        }

        sealed class RefEq : IEqualityComparer<ZwSoft.ZwCAD.Geometry.Point3d[]>
        {
            internal static readonly RefEq I = new RefEq();
            public bool Equals(ZwSoft.ZwCAD.Geometry.Point3d[] a, ZwSoft.ZwCAD.Geometry.Point3d[] b) { return ReferenceEquals(a, b); }
            public int GetHashCode(ZwSoft.ZwCAD.Geometry.Point3d[] a) { return System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(a); }
        }

        internal static int MsLast, MsMax;                     // 自检用：一次更新占主线程多久
        static void Note(Stopwatch sw) { MsLast = (int)sw.ElapsedMilliseconds; if (MsLast > MsMax) MsMax = MsLast; }
        internal static void SetInk(List<ZwSoft.ZwCAD.Geometry.Point3d> pts, double unitsPerPixel)
        {
            if ((pts == null || pts.Count == 0) && ink.Count == 0) return;
            ink.Clear();
            if (pts != null) ink.AddRange(pts);
            if (unitsPerPixel > 0) upp = unitsPerPixel;
            DrawInk();
        }

        // 视图缩放后：虚线段长 / 红 × 按新比例重算（几何本身是 WCS，不用动）
        internal static void Rescale(double unitsPerPixel)
        {
            if (ink.Count == 0 || unitsPerPixel <= 0 || Math.Abs(unitsPerPixel - upp) < 1e-9 * unitsPerPixel) return;
            upp = unitsPerPixel;
            DrawInk();
        }

        static void DrawInk()
        {
            var sw = Stopwatch.StartNew();
            try
            {
                if (ink.Count >= 2 && DrawPath()) goto Cross;
                if (pathOn) { Erase(pathEnt); pathOn = false; }
                // ---- 白色虚线：沿折线按「3 像素实 / 3 像素空」切段（跨拐点连续计长）----
                var segs = new List<ZwSoft.ZwCAD.Geometry.Point3d>();
                double dash = 3 * upp, gap = 3 * upp, period = dash + gap;
                double total = 0;
                for (int i = 0; i + 1 < ink.Count; i++) total += ink[i].DistanceTo(ink[i + 1]);
                // 超长路径（大图里整屏拖）时放大步长，保证根数有上限
                if (total / period > MaxDashes) { double k = total / period / MaxDashes; dash *= k; gap *= k; period *= k; }
                // 按「沿路径的累计长度」逐根取虚线：第 k 根占 [k*period, k*period+dash]，每步必然前进
                // （原来按剩余长度累加相位，浮点残差会让某一步前进 0，死循环直到内存耗尽 —— 真机实测）。
                if (ink.Count >= 2 && total > 0 && period > 0)
                {
                    var cum = new double[ink.Count];
                    for (int i = 1; i < ink.Count; i++) cum[i] = cum[i - 1] + ink[i - 1].DistanceTo(ink[i]);
                    int seg = 0;
                    Func<double, ZwSoft.ZwCAD.Geometry.Point3d> at = u =>
                    {
                        while (seg < ink.Count - 2 && cum[seg + 1] < u) seg++;
                        double l = cum[seg + 1] - cum[seg];
                        double r = l > 1e-12 ? (u - cum[seg]) / l : 0;
                        if (r < 0) r = 0; if (r > 1) r = 1;
                        return ink[seg] + (ink[seg + 1] - ink[seg]) * r;
                    };
                    int count = (int)Math.Ceiling(total / period);
                    for (int k = 0; k < count && k < MaxDashes + 2; k++)
                    {
                        double u0 = k * period, u1 = Math.Min(total, u0 + dash);
                        if (u1 <= u0) break;
                        segs.Add(at(u0)); segs.Add(at(u1));
                    }
                }
                int n = segs.Count / 2;
                var white = C(Color.FromArgb(235, 235, 235));
                for (int i = 0; i < n; i++)
                {
                    if (i >= dashes.Count) { var ln = new Line(); ln.Color = white; dashes.Add(ln); }
                    var d = dashes[i];
                    if (i < dashOn && d.StartPoint == segs[2 * i] && d.EndPoint == segs[2 * i + 1]) continue;   // 没动的不惊动 CAD
                    d.StartPoint = segs[2 * i]; d.EndPoint = segs[2 * i + 1];
                    if (i < dashOn) TM.UpdateTransient(d, All); else Add(d);
                }
                for (int i = n; i < dashOn; i++) Erase(dashes[i]);
                dashOn = n;

            Cross:
                // ---- 起点红 ×（半边 4 像素；错开 0.6 像素各画一遍，看着有两像素粗）----
                if (ink.Count == 0)
                {
                    if (crossOn) { foreach (var x in cross) Erase(x); crossOn = false; }
                    return;
                }
                var o = ink[0];
                double m = 4 * upp, dx = 0.6 * upp;
                for (int i = 0; i < 4; i++)
                {
                    if (cross[i] == null) { cross[i] = new Line(); cross[i].Color = C(Color.FromArgb(255, 60, 60)); }
                    double off = i < 2 ? 0 : dx;
                    bool diag = (i % 2) == 0;
                    var cs = new ZwSoft.ZwCAD.Geometry.Point3d(o.X - m + off, diag ? o.Y - m : o.Y + m, o.Z);
                    var ce = new ZwSoft.ZwCAD.Geometry.Point3d(o.X + m + off, diag ? o.Y + m : o.Y - m, o.Z);
                    if (crossOn && cross[i].StartPoint == cs && cross[i].EndPoint == ce) continue;
                    cross[i].StartPoint = cs; cross[i].EndPoint = ce;
                    if (crossOn) TM.UpdateTransient(cross[i], All); else Add(cross[i]);
                }
                crossOn = true;
            }
            catch (System.Exception ex) { PushFails++; LastError = ex.GetType().Name + ":" + ex.Message + " @ " + ((ex.StackTrace ?? "").Split('\n')[0]).Trim(); }
            finally { Note(sw); }
        }

        // 画法 1：成功返回 true（虚线池随之收起）；图里没有合适的线型 / 出错返回 false，改走画法 2。
        static bool DrawPath()
        {
            if (NoLinetype || ltBroken) return false;
            try
            {
                var doc = CadApp.DocumentManager.MdiActiveDocument;
                if (doc == null) return false;
                var db = doc.Database;
                if (db != ltDb)
                {
                    // 换了图纸：线型要按新图重找，多段线也按新图重建
                    ltDb = db; FindDashLinetype(db);
                    if (pathEnt != null) { if (pathOn) { Erase(pathEnt); pathOn = false; } pathEnt.Dispose(); pathEnt = null; }
                }
                if (ltId.IsNull || ltLen <= 0) return false;
                // 屏幕上一个周期 6 像素：显示长度 = 图案长 × LTSCALE × 实体线型比例（模型空间 MSLTSCALE=1 时再除以注释比例）
                double ltscale = 1, anno = 1;
                try { ltscale = Convert.ToDouble(CadApp.GetSystemVariable("LTSCALE")); } catch { }
                try
                {
                    if (Convert.ToInt32(CadApp.GetSystemVariable("MSLTSCALE")) == 1 && Convert.ToInt32(CadApp.GetSystemVariable("TILEMODE")) == 1)
                    {
                        double cv = Convert.ToDouble(CadApp.GetSystemVariable("CANNOSCALEVALUE"));
                        if (cv > 0) anno = 1 / cv;
                    }
                }
                catch { }
                if (ltscale <= 0) ltscale = 1;
                double scale = 6 * upp / (ltLen * ltscale * anno);
                if (pathEnt == null)
                {
                    pathEnt = new Polyline();
                    pathEnt.SetDatabaseDefaults(db);
                    pathEnt.Color = C(Color.FromArgb(235, 235, 235));
                    pathEnt.Plinegen = true;                 // 线型跨顶点连续（徒手路径是一串很短的小段）
                }
                if (pathEnt.LinetypeId != ltId) pathEnt.LinetypeId = ltId;
                var pl = pathEnt;
                while (pl.NumberOfVertices > ink.Count) pl.RemoveVertexAt(pl.NumberOfVertices - 1);
                for (int i = 0; i < ink.Count; i++)
                {
                    var q = new ZwSoft.ZwCAD.Geometry.Point2d(ink[i].X, ink[i].Y);
                    if (i < pl.NumberOfVertices) pl.SetPointAt(i, q); else pl.AddVertexAt(i, q, 0, 0, 0);
                }
                pl.LinetypeScale = scale;
                if (pathOn) TM.UpdateTransient(pl, All); else { Add(pl); pathOn = true; }
                for (int i = 0; i < dashOn; i++) Erase(dashes[i]);
                dashOn = 0;
                return true;
            }
            catch (System.Exception ex)
            {
                ltBroken = true;                               // 这条路走不通：本次会话改用短线池
                LastError = "path:" + ex.GetType().Name + ":" + ex.Message;
                if (pathOn) { Erase(pathEnt); pathOn = false; }
                return false;
            }
        }

        // 找一个「一段实 + 一段空」的简单虚线线型，优先 HIDDEN / DASHED
        static void FindDashLinetype(Database db)
        {
            ltId = ObjectId.Null; ltLen = 0;
            int bestRank = int.MaxValue;
            using (var tr = db.TransactionManager.StartTransaction())
            {
                var table = (LinetypeTable)tr.GetObject(db.LinetypeTableId, OpenMode.ForRead);
                foreach (ObjectId id in table)
                    try
                    {
                        var r = (LinetypeTableRecord)tr.GetObject(id, OpenMode.ForRead);
                        if (r.NumDashes != 2 || r.PatternLength <= 0) continue;
                        double a = r.DashLengthAt(0), b = r.DashLengthAt(1);
                        if (!(a > 0 && b < 0)) continue;
                        if (r.ShapeNumberAt(0) != 0 || r.ShapeNumberAt(1) != 0) continue;   // 带文字 / 形的不要
                        string n = (r.Name ?? "").ToUpperInvariant();
                        int rank = n == "HIDDEN" ? 0 : n == "DASHED" ? 1 : n.StartsWith("HIDDEN") ? 2 : n.StartsWith("DASHED") ? 3 : 5;
                        if (rank < bestRank) { bestRank = rank; ltId = id; ltLen = r.PatternLength; }
                    }
                    catch { }
                tr.Commit();
            }
        }

        internal static void ClearAll()
        {
            ClearPieces();
            SetInk(null, 0);
        }
    }

    // 拖动笔画 / 栏选橡皮筋：点换成 WCS 交给 CadOverlay 画（白色虚线 + 起点红 ×）
    sealed class InkPreview : IDisposable
    {
        readonly List<ZwSoft.ZwCAD.Geometry.Point3d> wpts = new List<ZwSoft.ZwCAD.Geometry.Point3d>();
        DateTime wstamp = DateTime.MinValue;
        internal bool Empty { get { return wpts.Count == 0; } }

        internal void Add(IntPtr h, double fx, double fy, int width, int height, bool red)
        {
            var pj = HoverPreview.Projector.Current(width, height);
            if (pj == null) return;
            wpts.Add(pj.W(new PointF((float)(fx * width), (float)((1.0 - fy) * height))));
            if (wpts.Count > 2 && (DateTime.UtcNow - wstamp).TotalMilliseconds < 15) return;
            wstamp = DateTime.UtcNow;
            CadOverlay.SetInk(wpts, pj.UnitsPerPixel);
        }

        // 栏选的「橡皮筋」：整条折线一次给齐（已点的栏选点 + 当前光标），每次都重画。
        internal void SetPath(List<PointF> screenPts, int width, int height)
        {
            var pj = HoverPreview.Projector.Current(width, height);
            if (pj == null) return;
            wpts.Clear();
            foreach (var s in screenPts) wpts.Add(pj.W(s));
            CadOverlay.SetInk(wpts, pj.UnitsPerPixel);
        }

        internal void End() { wpts.Clear(); CadOverlay.SetInk(null, 0); }
        public void Dispose() { End(); }
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
        FenceActive = false;
        try { if (hover != null) { hover.Clear(); hover.FenceReset(); hover.WatchOff(); } } catch { }
        try { if (ink != null) ink.End(); CadOverlay.ClearAll(); } catch { }
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
                // 拖动期间 LISP 用本函数轮询（不走 grread），所以 Esc 也得在这里看
                if (Down(0x1B)) return Status("ESC;" + N(cx) + ";" + N(cy));
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
            // 第 4 个参数（可选）：实时预览模式 0 = 修剪 / 1 = 延伸 / 省略或 -1 = 不预览
            int mode = a.Length > 3 ? Convert.ToInt32(a[3].Value) : -1;
            int w, h;
            if (!ScreenSize(out w, out h)) return Status("NOSCREEN");
            IntPtr canvas = FindCanvas(w, h);
            if (canvas == IntPtr.Zero) return Status("NOCANVAS");
            if (ink == null) ink = new InkPreview();
            if (ink.Empty) inkFrac.Clear();
            inkFrac.Add(new double[] { fx, fy });
            ink.Add(canvas, fx, fy, w, h, red);
            // 拖动期间要有底层钩子盯着「松开左键」（见 HoverPreview.OnLowLevelMouse）
            if (hover == null) hover = new HoverPreview();
            hover.Attach(canvas);
            hover.WatchOn();
            HoverPreview.DragActive = true;
            if (mode >= 0 && inkFrac.Count >= 2 && (DateTime.UtcNow - fenceStamp).TotalMilliseconds >= 30)
            {
                fenceStamp = DateTime.UtcNow;
                if (hover == null) hover = new HoverPreview();
                hover.FenceFrac(canvas, inkFrac, w, h, mode == 1, false);
            }
            return Status("OK");
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    static readonly List<double[]> inkFrac = new List<double[]>();
    internal static volatile bool FenceActive;                       // 栏选橡皮筋正在显示（视图变化时按新视图重画它）   // 徒手笔画的比例坐标（给实时预览用）
    static DateTime fenceStamp = DateTime.MinValue;

    [LispFunction("ZWK_INK_END_101")]
    public static ResultBuffer InkEnd(ResultBuffer args)
    {
        HoverPreview.DragActive = false;
        FenceActive = false;
        try { if (ink != null) ink.End(); inkFrac.Clear(); if (hover != null) hover.FenceReset(); } catch { }
        return Status("OK");
    }

    // (ZWK_FENCE_101 mode fx1 fy1 fx2 fy2 ...) —— 栏选的橡皮筋 + 实时预览：
    // 画出经过这些点的白色虚线（起点红 ×），并预览被穿过对象的修剪（mode 0）/ 延伸（mode 1）结果。
    // 点是 0~1 的比例坐标，最后一个点通常是当前光标。返回 "OK;点数"。
    // (ZWK_OPT_101 dashmax nolinetype) —— 调试 / 测性能用：短线池根数上限；1 = 不用图里的虚线线型
    [LispFunction("ZWK_OPT_101")]
    public static ResultBuffer Opt(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            if (a.Length > 0) CadOverlay.MaxDashes = Math.Max(1, Convert.ToInt32(a[0].Value));
            if (a.Length > 1) CadOverlay.NoLinetype = Convert.ToInt32(a[1].Value) != 0;
            return Status("OK;" + CadOverlay.MaxDashes + ";" + CadOverlay.NoLinetype);
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

    [LispFunction("ZWK_FENCE_101")]
    public static ResultBuffer FenceShow(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            if (a.Length < 3) return Status("ERROR");
            int mode = Convert.ToInt32(a[0].Value);
            var fr = new List<double[]>();
            for (int i = 1; i + 1 < a.Length; i += 2)
                fr.Add(new double[] { Convert.ToDouble(a[i].Value), Convert.ToDouble(a[i + 1].Value) });
            int w, h;
            if (!ScreenSize(out w, out h)) return Status("NOSCREEN");
            IntPtr canvas = FindCanvas(w, h);
            if (canvas == IntPtr.Zero) return Status("NOCANVAS");
            if (ink == null) ink = new InkPreview();
            if (hover == null) hover = new HoverPreview();
            // 缩放 / 平移途中不画（否则虚线和红 × 跟着缩放动画跑）；视图停稳后定时器会按最终视图画回来
            var sp = new List<PointF>();
            foreach (var f in fr) sp.Add(new PointF((float)(f[0] * w), (float)((1.0 - f[1]) * h)));
            ink.SetPath(sp, w, h);
            FenceActive = true;
            if (hover == null) hover = new HoverPreview();
            if (fr.Count >= 2 && (DateTime.UtcNow - fenceStamp).TotalMilliseconds >= 25)
            {
                fenceStamp = DateTime.UtcNow;
                hover.FenceFrac(canvas, fr, w, h, mode == 1, true);
            }
            return Status("OK;" + fr.Count);
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
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
    // 自检：(ZWK_TIMERTEST_101 1) 开一个与视图巡检同款的 60ms 主线程定时器并清零计数；
    // (ZWK_TIMERTEST_101 0) 关掉并返回 "TICKS;n"。用来确认 grread 等待期间 CAD 的消息泵会派发定时器。
    static IntPtr testTimer = IntPtr.Zero;
    static TimerProc testTimerProc;
    static int testTicks;
    static void OnTestTimer(IntPtr h, uint msg, IntPtr id, uint time) { try { testTicks++; } catch { } }
    [LispFunction("ZWK_TIMERTEST_101")]
    public static ResultBuffer TimerTest(ResultBuffer args)
    {
        try
        {
            TypedValue[] a = args == null ? new TypedValue[0] : args.AsArray();
            bool on = a.Length > 0 && Convert.ToInt32(a[0].Value) != 0;
            if (testTimer != IntPtr.Zero) { KillTimer(IntPtr.Zero, testTimer); testTimer = IntPtr.Zero; }
            if (on)
            {
                testTicks = 0;
                testTimerProc = OnTestTimer;
                testTimer = SetTimer(IntPtr.Zero, IntPtr.Zero, 60, testTimerProc);
                return Status(testTimer != IntPtr.Zero ? "ON" : "FAIL");
            }
            return Status("TICKS;" + testTicks);
        }
        catch (System.Exception ex) { return Status("ERR:" + ex.Message); }
    }

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
            sb.Append(";WheelHookHits=" + HoverPreview.WheelHookHits);
            sb.Append(";TimerTicks=" + HoverPreview.TimerTicks + ";StaleHides=" + HoverPreview.StaleHides);
            sb.Append(";HookInstalls=" + HoverPreview.HookInstalls + ";HookFails=" + HoverPreview.HookFails +
                      ";LButtonUpHits=" + HoverPreview.LButtonUpHits + ";SyntheticMoves=" + HoverPreview.SyntheticMoves +
                      ";FenceMs=" + HoverPreview.FenceMsLast + "/" + HoverPreview.FenceMsMax + ";OverlayMs=" + CadOverlay.MsLast + "/" + CadOverlay.MsMax + ";Overlay=" + CadOverlay.Pushes + "/" + CadOverlay.PushFails + "/" + CadOverlay.LastError + ";DragActive=" + HoverPreview.DragActive + ";WatchAlive=" + (hover != null && hover.WatchAlive));
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





