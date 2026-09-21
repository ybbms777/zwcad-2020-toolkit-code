;;; trimext.lsp --- AutoCAD 2025 TRIM / EXTEND 交互层复刻（B 线，规格书 3.3）。
;;; 提示文本逐字取自 AutoCAD 2025 简体中文实机抓取（规格书 3.4），不要凭印象改措辞。
;;; 几何层委托 ZWCAD 原生 TRIM / EXTEND，用 CMDECHO=0 屏蔽它自己的提示（规格书 3.2 T1）。
;;; 鼠标手势走 bin/ZWKit.Core.102.dll 的 ZWKCAP103（比 ZWKCAP102 多回传按键）。
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
  (apply 'command lst))

;; 一次操作 = 一个 UNDO 组，配合「放弃(U)」的 (_.UNDO 1) 粒度。
(defun ze:apply (isExt mode plist tail cand / doc snap)
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

;; ============================================================ 鼠标捕捉（ZWKCAP103）
;; 返回 ("KEY" "C") / ("STROKE" 点表) / ("CANCEL" nil) / ("EMPTY" nil) / ("ERROR" nil)。
;; 笔画带 Shift 时 DLL 会加 "TRIM" 前缀，这里翻成 ze:shift。
(defun ze:capture (/ r)
  (setq ze:shift nil r (zwk:bridge "ZWKCAP103"))
  (if (and (listp r) (= (length r) 1) (listp (car r))) (setq r (car r)))
  (cond
    ((not (listp r)) (list "ERROR" nil))
    ((equal (car r) "TRIM") (setq ze:shift T) (list "STROKE" (cdr r)))
    ((equal (car r) "KEY") (list "KEY" (cadr r)))
    ((and (listp (car r)) (numberp (caar r))) (list "STROKE" r))
    (T (list (car r) nil))))

(defun ze:ready (/ r)
  (setq r (vl-catch-all-apply 'zwk:ready nil))
  (and (not (vl-catch-all-error-p r)) r))

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
    (progn (vl-catch-all-apply 'setvar (list "CURSORSIZE" ze:cur))
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
    (cond
      ((equal kind "KEY")
        (setq u (ze:key isExt u data)))
      ((equal kind "STROKE")
        (setq pts (ze:wpts data)
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