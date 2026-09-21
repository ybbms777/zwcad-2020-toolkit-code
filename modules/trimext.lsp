;;; trimext.lsp --- AutoCAD 2025 TRIM / EXTEND 交互层复刻（B 线，规格书 3.3）。
;;; 提示文本逐字取自 AutoCAD 2025 简体中文实机抓取（规格书 3.4），不要凭印象改措辞。
;;; 几何层委托 ZWCAD 原生 TRIM / EXTEND，用 CMDECHO=0 屏蔽它自己的提示（规格书 3.2 T1）。
;;; 鼠标手势走 CAD 自己的 grread 跟踪模式（准星才是 CAD 原生那一个）。悬停高亮 / 笔画 /
;;; Shift 与左键状态借 bin/ZWKit.Core.102.dll 的 ZWK_*_101 一组接口（LispFunction 直调）。
;;; 分发文件是 GBK 编码。
(vl-load-com)

;; ============================================================ 模拟 TRIMEXTENDMODE
;; LISP 建不了真系统变量，只能做到「敲 TRIMEXTENDMODE 能读写」（规格书 3.2 T5）。
(setq ze:md nil)
(setq ze:bounds nil)
(setq ze:mark nil)
(setq ze:shift nil)

(defun ze:mode (/ v)
  (if ze:md ze:md
    (progn
      (setq v (vl-catch-all-apply 'getenv (list "ZWK_TRIMEXTENDMODE")))
      (if (vl-catch-all-error-p v) (setq v nil))
      (setq ze:md (if (equal v "0") 0 1)))))

(defun ze:setmode (n / r)
  (setq ze:md (if (= n 0) 0 1))
  (setq r (vl-catch-all-apply 'setenv (list "ZWK_TRIMEXTENDMODE" (if (= n 0) "0" "1"))))
  ze:md)

(defun c:TRIMEXTENDMODE (/ cur s)
  (setq cur (ze:mode))
  (initget 4)
  (setq s (getint (strcat "\n输入 TRIMEXTENDMODE 的新值 <" (itoa cur) ">: ")))
  (if s
    (if (or (= s 0) (= s 1))
      (ze:setmode s)
      (princ "\nTRIMEXTENDMODE 只能是 0 或 1。")))
  (princ))

;; ============================================================ 提示文本（规格书 3.4 逐字）
(defun ze:proj (/ v)
  (setq v (getvar "PROJMODE"))
  (cond ((equal v 0) "无") ((equal v 2) "视图") (T "UCS")))

;; EDGEMODE=0 -> "无" 是实机确认的；1 -> "延伸" 属推断（规格书 3.4 待校准第 2 项）。
(defun ze:edge (/ v)
  (setq v (getvar "EDGEMODE"))
  (if (equal v 1) "延伸" "无"))

(defun ze:setline ()
  (strcat "当前设置: 投影=" (ze:proj) ",边=" (ze:edge)
          ",模式=" (if (= (ze:mode) 1) "快速" "标准")))

;; 选项表：顺序严格按实机原文 —— 边界 / [栏选] / 窗交 / 模式 / 投影 / [边] / [删除] / [放弃]。
(defun ze:opts (isExt md u / s)
  (setq s (if isExt "边界边(B)" "剪切边(T)"))
  (if (= md 0) (setq s (strcat s "/栏选(F)")))
  (setq s (strcat s "/窗交(C)/模式(O)/投影(P)"))
  (if (= md 0) (setq s (strcat s "/边(E)")))
  (if (not isExt) (setq s (strcat s "/删除(R)")))
  (if u (setq s (strcat s "/放弃(U)")))
  s)

;; 主提示末字是「或」且其后直接换行，选项行另起一行、行首一个半角空格（实机原文如此）。
(defun ze:prompt (isExt u)
  (strcat "\n" (if isExt
                   "选择要延伸的对象，或按住 Shift 键选择要修剪的对象或"
                   "选择要修剪的对象，或按住 Shift 键选择要延伸的对象或")
          "\n [" (ze:opts isExt (ze:mode) u) "]: "))

;; 无效选择的两行报错（规格书 3.4，逐字；注意快速模式 EXTEND 那行没有「投影」）。
(defun ze:need (isExt md)
  (strcat "\n需要点或栏选(F)/" (if isExt "边界边(B)" "剪切边(T)") "/窗交(C)/模式(O)"
          (cond ((and isExt (= md 1)) "")
                ((and (not isExt) (= md 0)) "/投影(P)/边(E)")
                (T "/投影(P)"))))

(defun ze:bad (isExt)
  (princ "\n*无效选择*")
  (princ (ze:need isExt (ze:mode)))
  (princ))

;; ============================================================ 坐标换算（与 trim.lsp 的 zt:points 一致）
(defun ze:view (/ sz h w ctr)
  (setq sz (getvar "SCREENSIZE") h (getvar "VIEWSIZE")
        w (* h (/ (car sz) (cadr sz)))
        ctr (trans (getvar "VIEWCTR") 1 2))
  (list w h ctr))

;; 捕捉模块给的是 0~1 的屏幕比例坐标，这里换算成 UCS 点（command 与 ssget 都按 UCS 解释点）。
(defun ze:wpts (raw / v w h ctr)
  (setq v (ze:view) w (car v) h (cadr v) ctr (caddr v))
  (mapcar '(lambda (p)
    (trans (list (+ (car ctr) (* (- (car p) 0.5) w))
                 (+ (cadr ctr) (* (- (cadr p) 0.5) h))
                 (caddr ctr)) 2 1)) raw))

(defun ze:pend (/ sz)
  (setq sz (getvar "SCREENSIZE"))
  (if (and sz (> (cadr sz) 0)) (/ (getvar "VIEWSIZE") (cadr sz)) 0.0))

;; 拾取点周围 2 像素的小交叉框，只用来「判断这个点上有没有对象」。
;; 单个选择本身已经改用裸点喂原生命令 —— 真机实测（T3）ZWCAD 并没有把裸点
;; 当成矩形框选（CMDACTIVE=0，剪掉的那一段也对），比小窗交更贴近 AutoCAD 的
;; 「拾取点决定剪哪一端」。
(defun ze:box (p / d)
  (setq d (* 2.0 (ze:pend)))
  (list (list (- (car p) d) (- (cadr p) d) (caddr p))
        (list (+ (car p) d) (+ (cadr p) d) (caddr p))))

(defun ze:hit (p / b ss)
  (setq b (ze:box p) ss (ssget "_C" (car b) (cadr b)))
  (ze:dbg (strcat "hit " (vl-princ-to-string p) " -> " (if ss (itoa (sslength ss)) "nil")))
  (vl-catch-all-apply 'sssetfirst (list nil nil))
  ss)

;; 对象快照 / 图层可编辑判断：与 trim.lsp 的 zt:state / zt:unlocked 完全一致（那边已验证）。
(defun ze:state (ent / data result sub item)
  (if (setq data (entget ent))
    (progn
      (setq result (list data))
      (if (= (cdr (assoc 0 data)) "POLYLINE")
        (progn
          (setq sub (entnext ent))
          (while (and sub (setq item (entget sub)) (/= (cdr (assoc 0 item)) "SEQEND"))
            (setq result (cons item result) sub (entnext sub)))))
      result)))

(defun ze:unlocked (ent / data layer)
  (and (setq data (entget ent))
       (setq layer (tblsearch "LAYER" (cdr (assoc 8 data))))
       (= 0 (logand 4 (cdr (assoc 70 layer))))))

;; 把选择集里的对象存成「实体名 + 数据快照」，供操作后比对。
(defun ze:snap (ss / i n e out)
  (setq out nil)
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (setq e (ssname ss i))
        (if (ze:unlocked e) (setq out (cons (list e (ze:state e)) out)))
        (setq i (1+ i)))))
  out)

;; 本次选择会碰到哪些对象（给「删掉修剪不到的」用）。
(defun ze:cand (mode plist)
  (cond
    ((equal mode "_C") (ssget "_C" (car plist) (cadr plist)))
    ((equal mode "_F") (ssget "_F" plist '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,ELLIPSE,SPLINE"))))
    (T nil)))

;; 快速模式专用：AutoCAD 会「删除无法修剪的选定对象」。
;; ZWCAD 原生不会（2026-09-21 真机实测 T1：不可修剪的对象原样留着、也不报错），
;; 所以这一条得我们自己补。判据同 trim.lsp 的 zt:trim —— 操作前后实体数据没变 = 没被修剪到。
;; 这里不打印提示：AutoCAD 对这条没有可抓取的文字，不编造措辞。
(defun ze:trim-del (snap / n)
  (setq n 0)
  (foreach it snap
    (if (and (ze:unlocked (car it)) (equal (cadr it) (ze:state (car it))))
      (if (entdel (car it)) (setq n (1+ n)))))
  n)

(defun ze:union (a b / i n)
  (if (null a) (setq a (ssadd)))
  (if b
    (progn
      (setq i 0 n (sslength b))
      (while (< i n) (ssadd (ssname b i) a) (setq i (1+ i)))))
  a)

(defun ze:win (p1 p2 / m)
  (setq m (if (< (car p1) (car p2)) "_W" "_C"))
  (ssget m p1 p2))

;; ============================================================ 几何层：委托原生
;; bnd 为 nil 表示「全部对象作边界」。
;; 注意：这里必须给原生命令一个「真实的选择集」，不能给 ""（回车=全选）——
;; 2026-09-21 在 ZWCAD 2020 上实测：传 "" 时 TRIM / EXTEND 会从头到尾走完、
;; 不报任何错、也不改图形（V1/V2/V3/V5 均无变化）；改成 (ssget "_X") 后
;; 栏选 / 窗交 / 修剪 / 延伸 四种组合全部生效（A1~A4）。
(defun ze:cmd (isExt bnd mode plist tail / lst p s)
  (setq s (if bnd bnd (ssget "_X")))
  (if (null s) (setq s ""))
  (setq lst (list (if isExt "_.EXTEND" "_.TRIM") s ""))
  (if mode (setq lst (append lst (list mode))))
  (foreach p plist (setq lst (append lst (list "_non" p))))
  (foreach p tail (setq lst (append lst (list p))))
  (ze:dbg (strcat "cmd=" (vl-princ-to-string (mapcar 'type lst))))
  (apply 'command lst))

;; 一次操作 = 一个 UNDO 组，配合「放弃(U)」的 (_.UNDO 1) 粒度。
(defun ze:apply (isExt mode plist tail cand / doc snap)
  (ze:dbg (strcat "apply isExt=" (vl-princ-to-string isExt) " mode=" (vl-princ-to-string mode)))
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  ;; 快照必须在执行「之前」拍：操作后再拍就变成「自己跟自己比」，永远判成没修剪到，
  ;; 结果把修剪成功的对象也一起删掉（真机实测踩过）。
  (setq snap (ze:snap cand))
  (vla-StartUndoMark doc) (setq ze:mark T)
  (ze:cmd isExt ze:bounds mode plist tail)
  (if (not isExt) (ze:trim-del snap))
  (vla-EndUndoMark doc) (setq ze:mark nil))

(defun ze:close-mark (/ doc)
  (if ze:mark
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list (vlax-get-acad-object))))
      (if (not (vl-catch-all-error-p doc))
        (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
      (setq ze:mark nil))))

;; 一笔轨迹：单点＝单个选择（裸点，让拾取点自己决定剪哪端），多点＝栏选。
(defun ze:stroke (isExt pts)
  (if (= (length pts) 1)
    (ze:apply isExt nil (list (car pts)) (list "") (ze:hit (car pts)))
    (ze:apply isExt "_F" pts (list "" "") (ze:cand "_F" pts))))

(defun ze:undo () (command "_.UNDO" 1))

;; ============================================================ 选项处理
(defun ze:opt-proj (/ s)
  (initget "N U V")
  (setq s (getkword (strcat "\n输入投影选项 [无(N)/UCS(U)/视图(V)] <" (ze:proj) ">: ")))
  (cond ((equal s "N") (setvar "PROJMODE" 0))
        ((equal s "U") (setvar "PROJMODE" 1))
        ((equal s "V") (setvar "PROJMODE" 2))))

(defun ze:opt-edge (/ s)
  (initget "E N")
  (setq s (getkword (strcat "\n输入隐含边延伸模式 [延伸(E)/不延伸(N)] <"
                            (if (equal (getvar "EDGEMODE") 1) "延伸" "不延伸") ">: ")))
  (cond ((equal s "E") (setvar "EDGEMODE" 1))
        ((equal s "N") (setvar "EDGEMODE" 0))))

(defun ze:opt-mode (isExt / s)
  (initget "Q S")
  (setq s (getkword (strcat "\n输入" (if isExt "延伸" "修剪") "模式选项 [快速(Q)/标准(S)] <"
                            (if (= (ze:mode) 1) "快速(Q)" "标准(S)") ">: ")))
  (cond ((equal s "Q") (ze:setmode 1))
        ((equal s "S") (ze:setmode 0))))

;; 删除：返回实际删掉的对象数。
;; 收尾提示「已删除 N 个对象。」未在实机日志里出现，属推断（规格书 3.4 待校准清单）。
(defun ze:opt-del (isExt / p b ss i k n)
  (setq n 0)
  (while (setq p (getpoint "\n选择要删除的对象或 <退出>: "))
    (setq b (ze:box p) ss (ssget "_C" (car b) (cadr b)))
    (if ss
      (progn
        (setq i 0 k (sslength ss))
        (while (< i k)
          (if (entdel (ssname ss i)) (setq n (1+ n)))
          (setq i (1+ i))))
      (ze:bad isExt)))
  (if (> n 0) (princ (strcat "\n已删除 " (itoa n) " 个对象。")))
  n)

;; 剪切边 / 边界边。start 为 T 表示命令起始的那一次（选择对象行带 [模式(O)]）。
(defun ze:pickbounds (isExt start / p b ss p2 go)
  (setq ss nil go T)
  (princ (strcat "\n" (ze:setline)))
  (princ (if isExt "\n选择边界边... " "\n选择剪切边..."))
  (while go
    (initget "O")
    (setq p (getpoint (strcat "\n选择对象或 " (if start "[模式(O)] " "") "<全部选择>: ")))
    (cond
      ((null p) (setq go nil))
      ((equal p "O") (ze:opt-mode isExt))
      (T
        (setq b (ze:box p))
        (setq ss (ze:union ss (ssget "_C" (car b) (cadr b))))
        (if (> (sslength ss) 0)
          (princ (strcat "\n找到 " (itoa (sslength ss)) " 个"))
          (progn
            (setq p2 (getpoint "\n指定对角点: "))
            (if p2
              (progn
                (setq ss (ze:union ss (ze:win p p2)))
                (princ (strcat "\n找到 " (itoa (sslength ss)) " 个")))))))))
  ss)

;; 栏选：只在标准模式下是命令选项（实机确认快速模式没有它）。
(defun ze:opt-fence (isExt / p pts go)
  (setq pts nil go T)
  (while go
    ;; 第二个栏选点的提示未在实机日志里出现，暂按第一个点的措辞推（规格书 3.4 待校准第 1 项）。
    (setq p (getpoint (if pts
                        "\n指定下一个栏选点或拾取/拖动光标: "
                        "\n指定第一个栏选点或拾取/拖动光标: ")))
    (if p (setq pts (append pts (list p))) (setq go nil)))
  (if (> (length pts) 1) (ze:apply isExt "_F" pts (list "" "") (ze:cand "_F" pts))))

;; 窗交：两个角点交给原生自己选，顺时针规则由原生保证。
(defun ze:opt-cross (isExt / p1 p2)
  (setq p1 (getpoint "\n指定第一个角点: "))
  (if p1
    (progn
      (setq p2 (getpoint p1 "\n指定对角点: "))
      (if p2 (ze:apply isExt "_C" (list p1 p2) (list "") (ze:cand "_C" (list p1 p2)))))))

;; 主提示的按键分发，返回更新后的「是否已有操作」。
(defun ze:key (isExt u k / md)
  (setq md (ze:mode))
  (cond
    ((and (not isExt) (equal k "T")) (ze:pickbounds isExt nil) u)
    ((and isExt (equal k "B")) (ze:pickbounds isExt nil) u)
    ((equal k "C") (ze:opt-cross isExt) T)
    ((equal k "O") (ze:opt-mode isExt) u)
    ((equal k "P") (ze:opt-proj) u)
    ((equal k "E") (if (= md 0) (ze:opt-edge) (ze:bad isExt)) u)
    ((equal k "F") (if (= md 0) (ze:opt-fence isExt) (ze:bad isExt)) u)
    ((equal k "R") (if isExt (ze:bad isExt) (if (> (ze:opt-del isExt) 0) T u)))
    ((equal k "U") (if u (ze:undo) (ze:bad isExt)) u)
    (T (ze:bad isExt) u)))

;; ============================================================ 鼠标捕捉（CAD 原生 grread）
;; 为什么不用自建消息循环：命令内部自己起循环时 CAD 根本不处理鼠标消息（2026-09-21 真机实测），
;; 它的准星必然僵在原地 —— 只能拿假光标顶上，看着别扭。grread 是 CAD 自己的输入循环，
;; 准星由它自己管、正常跟随鼠标（配 CURSORSIZE=1 就是 AutoCAD 那种小十字）。
;; 真机抓取到的 grread 事件：
;;   (5 (x y 0)) 移动    (3 (x y 0)) 左键按下    (25 (x y 0)) 右键    (2 n) 键盘
;;   Esc 时 grread 直接报「函数被取消」；左键「松开」不产生任何事件（只能查按键状态）。
;;   坐标就是 UCS 点，和 ssget / getpoint 一套；与屏幕比例坐标的换算见 ze:frac / ze:ptf。
(setq ze:grk nil)
(setq ze:hwarn nil)

;; 排查用的步骤日志：只有设了环境变量 ZWK_TRIMDEBUG=1 才写 %TEMP%\zwk-trim-debug.txt。
;; 正常使用完全不产生文件，出问题时让同事设一下变量、把日志发回来即可。
(defun ze:dbg (s / f)
  (if (getenv "ZWK_TRIMDEBUG")
    (progn
      (setq f (open (strcat (getenv "TEMP") "/zwk-trim-debug.txt") "a"))
      (if f (progn (write-line s f) (close f))))))

;; A-Z 的字符（不依赖 chr）。
(defun ze:ch (n / base)
  (setq base "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (cond ((and (>= n 65) (<= n 90)) (substr base (- n 64) 1))
        ((and (>= n 97) (<= n 122)) (substr base (- n 96) 1))
        (T "")))

;; 读一个 grread 事件：("MOVE"/"PRESS"/"KEY"/"RIGHT"/"ENTER"/"CANCEL"/"OTHER") + 点或字符。
(defun ze:next (/ k p)
  (setq k (vl-catch-all-apply 'grread (list T 13 0)) ze:grk k)
  (cond
    ((vl-catch-all-error-p k)
      (ze:dbg (strcat "grread-err=" (vl-catch-all-error-message k)))
      (list "CANCEL" nil))
    ((not (listp k)) (list "OTHER" nil))
    ((= (car k) 5) (list "MOVE" (cadr k)))
    ((= (car k) 3) (list "PRESS" (cadr k)))
    ((= (car k) 25) (list "RIGHT" (cadr k)))
    ((= (car k) 2)
      (setq p (cadr k))
      (cond ((and (numberp p) (= p 27)) (list "CANCEL" nil))
            ((and (numberp p) (or (= p 13) (= p 32))) (list "ENTER" nil))
            ((numberp p) (list "KEY" (ze:ch p)))
            (T (list "OTHER" nil))))
    (T (list "OTHER" nil))))

;; ===== DLL 小工具（bin/ZWKit.Core.102.dll）=====
;; 直接调 LispFunction，不走 zwk:bridge 的临时文件：每个鼠标事件都过一遍文件太慢。
;; 三个真机踩过的坑（ZWCAD 2020，2026-09-21）：
;;   1) LispFunction 的回包在 LISP 里是「一个元素的表」——("OK;3") 而不是 "OK;3"，这里统一拆开；
;;   2) 零参数的包装函数不能用 (vl-catch-all-apply 'F (list 0)) 调（会报「参数太多」），传 nil；
;;   3) 函数不存在时报的「undefined function」vl-catch-all-apply 拦不住，所以先用 ze:ready 自检版本。
(defun ze:dll (f a / r v)
  (setq r (vl-catch-all-apply f (if a a nil)))
  (cond
    ((vl-catch-all-error-p r) nil)
    ((null r) nil)
    ((listp r) (setq v (car r)) (if (stringp v) v nil))
    ((stringp r) r)
    (T nil)))

(defun zwk:module () (ze:dll 'ZWK_MODULE_101 nil))
(defun zwk:shift () (ze:dll 'ZWK_SHIFT_101 nil))
(defun zwk:state () (ze:dll 'ZWK_STATE_101 nil))
(defun zwk:hover (f) (ze:dll 'ZWK_HOVER_101 f))
(defun zwk:hover-end () (ze:dll 'ZWK_HOVER_END_101 nil))
(defun zwk:press (f ms) (ze:dll 'ZWK_PRESS_101 (append f (list ms))))
(defun zwk:ink (f red) (ze:dll 'ZWK_INK_101 (append f (list red))))
(defun zwk:ink-end () (ze:dll 'ZWK_INK_END_101 nil))

;; 左键按着吗（DLL 回包 "1;fx;fy" 的第一段）。
(defun zwk:down (/ s)
  (setq s (zwk:state))
  (and (stringp s) (= (substr s 1 1) "1")))

;; "UP;0.5123;0.4" -> ("UP" 0.5123 0.4)；解析不出来就是 nil。
(defun ze:stat (s / i n c out cur v)
  (if (not (stringp s)) (setq s ""))
  (setq i 1 n (strlen s) out nil cur "")
  (while (<= i n)
    (setq c (substr s i 1))
    (if (= c ";") (setq out (cons cur out) cur "") (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (setq out (reverse (cons cur out)))
  (if (/= (car out) "")
    (cons (car out)
          (mapcar '(lambda (x)
                     (setq v (vl-catch-all-apply 'atof (list x)))
                     (if (numberp v) v 0.0))
                  (cdr out)))))

;; ===== 坐标换算 =====
;; UCS 点 -> 屏幕比例坐标（0,0 = 绘图区左下角，与 DLL 里的口径一致）。
(defun ze:frac (p / v w h ctr d)
  (setq v (ze:view) w (car v) h (cadr v) ctr (caddr v) d (trans p 1 2))
  (list (+ 0.5 (/ (- (car d) (car ctr)) w))
        (+ 0.5 (/ (- (cadr d) (cadr ctr)) h))))

;; 屏幕比例 -> UCS 点（复用 ze:wpts）。
(defun ze:ptf (f) (car (ze:wpts (list f))))

;; 两个 UCS 点在屏幕上的像素距离（distance 必须是两点两个参数）。
(defun ze:px (p1 p2 / f1 f2 sz)
  (setq f1 (ze:frac p1) f2 (ze:frac p2) sz (getvar "SCREENSIZE"))
  (distance (list (* (- (car f1) (car f2)) (car sz))
                  (* (- (cadr f1) (cadr f2)) (cadr sz)) 0.0)
            (list 0.0 0.0 0.0)))

;; ===== 悬停高亮 =====
;; 压到哪条线，哪条线亮起来（DLL 算实体的屏幕折线，画在置顶透明窗上）。
(defun ze:hover (p / r)
  (setq r (zwk:hover (ze:frac p))
        ze:hovst r)
  (if (and (not ze:hwarn) (or (null r) (/= (substr r 1 2) "OK")))
    (progn (setq ze:hwarn T)
           (princ (strcat "\n悬停高亮不可用（" (if r r "无响应") "）。")))))

;; ===== 一次拾取手势 =====
;; 返回 ("KEY" "C") / ("STROKE" 点表) / ("CANCEL" nil)。
;; 单击 = 1 点（ze:loop 按「单点拾取」处理）；按住拖动 = 多点（按栏选处理）。
(defun ze:capture (/ e kind pt res done)
  (setq res nil done nil ze:shift nil)
  (ze:dbg "capture-start")
  (while (not done)
    (setq e (ze:next) kind (car e) pt (cadr e))
    ;; 注意：键盘事件的 pt 是字符串（"T" 这种），这里不能拿它当点用（rtos / car 都会报参数类型错误）。
    (ze:dbg (strcat "ev=" kind " data=" (vl-princ-to-string pt)))
    (cond
      ((equal kind "MOVE") (ze:hover pt))
      ((equal kind "PRESS") (setq res (ze:gesture pt)))
      ((equal kind "KEY") (setq res e))
      ((or (equal kind "RIGHT") (equal kind "ENTER") (equal kind "CANCEL"))
        (setq res (list "CANCEL" nil))))
    (if res (setq done T)))
  (ze:dbg (strcat "capture-end res=" (vl-princ-to-string res)))
  (zwk:hover-end)
  (zwk:ink-end)
  res)

;; 左键按下之后：点一下就松 = 单击；按住并移动 = 拖动（笔画）。
;; ze:shift 在「按下那一刻」锁定，之后中途按过 Shift 整笔都算（与 1.2.34 的行为一致）。
(defun ze:gesture (p / st r q pts e kind done)
  (ze:dbg (strcat "gesture p=" (vl-princ-to-string p) " frac=" (vl-princ-to-string (ze:frac p)) " shift=" (vl-princ-to-string (zwk:shift))))
  (setq st (equal (zwk:shift) "1")
        r (ze:stat (zwk:press (ze:frac p) 260))
        pts nil done nil)
  (ze:dbg (strcat "press=" (vl-princ-to-string r)))
  (if (equal (zwk:shift) "1") (setq st T))
  (cond
    ;; 松开了：单击；极快的短划（>4 像素）算两点的一段栏选。
    ((or (null r) (equal (car r) "UP"))
      (ze:dbg "branch=up")
      (setq ze:shift st)
      (if (and r (>= (length r) 3) (> (ze:px p (ze:ptf (cdr r))) 4.0))
        (list "STROKE" (list p (ze:ptf (cdr r))))
        (list "STROKE" (list p))))
    ((equal (car r) "OUT") (ze:dbg "branch=out") (list "CANCEL" nil))
    (T
      (ze:dbg "branch=hold")
      ;; 还按着：再给 150 毫秒 —— 只有「停住没动」才会等满，动一下就立刻回来。
      ;; 这一下是为了把「按住停一会儿再松手」也认成单击，不至于卡在那里等下一次移动。
      (setq r (ze:stat (zwk:press (ze:frac p) 150)))
      (ze:dbg (strcat "press2=" (vl-princ-to-string r)))
      (if (or (null r) (equal (car r) "UP"))
        (progn (setq ze:shift st) (list "STROKE" (list p)))
        (progn
          (ze:dbg "branch=drag")
          ;; 拖动：笔画跟着鼠标走，松开时结算成一笔栏选
          (setq pts (list p))
          (zwk:ink (ze:frac p) (if st 1 0))
          (while (not done)
            (setq e (ze:next) kind (car e))
            (cond
              ((equal kind "MOVE")
                (setq q (cadr e))
                ;; 先判松开：松开之后的那一次移动不该再算进笔画里。
                (if (not (zwk:down))
                  (setq done T)
                  (progn
                    (if (> (ze:px (car pts) q) 0.2)
                      (progn (setq pts (cons q pts)) (zwk:ink (ze:frac q) (if st 1 0))))
                    (if (equal (zwk:shift) "1") (setq st T))
                    ;; grread 不给「松开」事件，只能查（见 ze:next 的注释）。
                    (setq r (ze:stat (zwk:press (ze:frac q) 200)))
                    (if (or (null r) (equal (car r) "UP") (equal (car r) "OUT")) (setq done T)))))
              ((equal kind "CANCEL") (setq pts nil done T))
              ((equal kind "PRESS") nil)
              (T (if (not (zwk:down)) (setq done T)))))
          (ze:dbg (strcat "drag-end pts=" (vl-princ-to-string (length pts)) " st=" (vl-princ-to-string st)))
          (zwk:ink-end)
          (cond
            ((null pts) (list "CANCEL" nil))
            ((> (length pts) 2048)
              (princ "\n本次轨迹过长，已取消；请分几笔修剪。")
              (list "CANCEL" nil))
            (T (setq ze:shift st) (list "STROKE" (reverse pts)))))))))

;; 新版 DLL 自检：ZWK_MODULE_101 返回 "READY2"（1.2.35 及以前是 "READY"，没有这些接口）。
(defun ze:ready (/ r)
  (setq r (vl-catch-all-apply 'zwk:ready nil))
  (and (not (vl-catch-all-error-p r)) r (equal (zwk:module) "READY2")))

(defun ze:planar ()
  (and (= (getvar "TILEMODE") 1)
       (member (getvar "PERSPECTIVE") '(nil 0))
       (equal (car (getvar "VIEWDIR")) 0.0 1e-8)
       (equal (cadr (getvar "VIEWDIR")) 0.0 1e-8)
       (> (caddr (getvar "VIEWDIR")) 0.0)))

;; ============================================================ 光标
;; 进 TR / EX 时把十字光标缩到最小，只剩中间那个小方块（就是 AutoCAD 的样子）。
;; 注意：CURSORSIZE 是全局设置，退出时必须原样还原，所以挂在 ze:restore 上，
;; 正常结束和出错（*error*）两条路都会经过它。ZWCAD 若不支持该变量则自动跳过。
(setq ze:cur nil)

(defun ze:cursor-on (/ v)
  (setq v (vl-catch-all-apply 'getvar (list "CURSORSIZE")))
  (if (vl-catch-all-error-p v)
    nil
    (progn
      (setq ze:cur v)
      (vl-catch-all-apply 'setvar (list "CURSORSIZE" 1)))))

(defun ze:cursor-off ()
  (if ze:cur
    (progn
      (ze:dbg (strcat "cursor-off cur=" (vl-princ-to-string ze:cur) " fix=" (vl-princ-to-string (fix ze:cur))
                      " r=" (vl-princ-to-string (vl-catch-all-apply 'setvar (list "CURSORSIZE" (fix ze:cur))))
                      " now=" (vl-princ-to-string (vl-catch-all-apply 'getvar (list "CURSORSIZE")))))
      (setq ze:cur nil))))

(defun ze:restore (echo snap)
  (ze:cursor-off)
  (if echo (setvar "CMDECHO" echo))
  (if snap (setvar "OSMODE" snap))
  (redraw))

;; ============================================================ 主循环
(defun ze:loop (isExt / u pending res kind data pts act go)
  (setq u nil pending nil go T)
  (princ (ze:prompt isExt u))
  (while go
    (setq res (ze:capture) kind (car res) data (cadr res))
    (ze:dbg (strcat "loop kind=" kind " n=" (if (listp data) (vl-princ-to-string (length data)) (vl-princ-to-string data))))
    (cond
      ((equal kind "KEY")
        (setq u (ze:key isExt u data)))
      ((equal kind "STROKE")
        ;; data 已经是 UCS 点（grread 给的就是 UCS），不用再过 ze:wpts（那是比例坐标用的）。
        (setq pts data
              act (if ze:shift (not isExt) isExt))
        (cond
          (pending
            (setq pts (cons pending pts) pending nil)
            (ze:apply act "_F" pts (list "" "") (ze:cand "_F" pts))
            (setq u T))
          ((= (length pts) 1)
            (if (ze:hit (car pts))
              (progn (ze:apply act nil (list (car pts)) (list "") (ze:hit (car pts))) (setq u T))
              (setq pending (car pts))))
          (T (ze:apply act "_F" pts (list "" "") (ze:cand "_F" pts)) (setq u T))))
      ((equal kind "EMPTY")
        (princ "\n请单击对象，或按住左键划过对象。"))
      ((equal kind "ERROR")
        (princ "\n鼠标捕捉模块无响应，已停止。请确认 bin/ZWKit.Core.102.dll 已换成新版并重启 CAD。")
        (setq go nil))
      (T (setq go nil)))
    (if go
      (if pending
        (princ "\n指定下一个栏选点或拾取/拖动光标: ")
        (princ (ze:prompt isExt u)))))
  (princ))

(defun ze:run (isExt / *error* echo snap)
  (setq echo (getvar "CMDECHO") snap (getvar "OSMODE"))
  (ze:cursor-on)
  (defun *error* (msg)
    (ze:dbg (strcat "ERROR " (vl-princ-to-string msg)))
    (ze:close-mark)
    (ze:restore echo snap)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n修剪/延伸提示：" msg)))
    (princ))
  (cond
    ((not (ze:ready))
      (princ "\n鼠标模块未通过检查，已停止。请重建并加载本版 bin/ZWKit.Core.102.dll 后重试。"))
    ((not (ze:planar))
      (princ "\n本版支持模型空间的二维平面视图，请在当前 UCS 的俯视平面下使用。"))
    (T
      (setq ze:bounds nil)
      (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
      (if (= (ze:mode) 0)
        (setq ze:bounds (ze:pickbounds isExt T))
        (princ (strcat "\n" (ze:setline))))
      (ze:loop isExt)
      (ze:restore echo snap)))
  (princ))

;; ============================================================ 命令
;; TR / EX 是完整复刻版；拖动版仍是 ZT / TRD / EXD（click.lsp 里保留下来的），
;; TRC / EXC 作为 TR / EX 的别名。
(defun c:TR () (ze:run nil))
(defun c:TRC () (ze:run nil))
(defun c:EX () (ze:run T))
(defun c:EXC () (ze:run T))
(princ "\nTR / EX 已按 AutoCAD 2025 复刻：提示与选项逐字一致，按 TRIMEXTENDMODE 决定快速或标准模式。")
(princ)