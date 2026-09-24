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
(setq ze:mark nil)   ;; 整条命令的外层 UNDO 组开着吗（见 ze:group-begin）
(setq ze:shift nil)
(setq ze:isext nil)  ;; 当前是 EX（T）还是 TR（nil），悬停 / 栏选预览要用
(setq ze:ops 0)      ;; 本次命令里做了几次可撤销的操作 —— 放弃(U) 只能撤这么多次，不能撤到命令之前

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

;; 拾取光圈 = CAD 自己的「拾取框」（PICKBOX，单位像素）。屏幕上那个小方块就是它，
;; 看到多大、判定就是多大。2026-09-21 用户反馈：原来写死 ±2 像素，准星压到线上
;; 半天不亮，触发面积太小（和 CURSORSIZE 无关，把十字调大也没用）。
(defun ze:pick (/ n)
  (setq n (vl-catch-all-apply 'getvar (list "PICKBOX")))
  (if (and (numberp n) (>= n 3) (<= n 60)) n 8))

;; 标注类对象（文字 / 尺寸 / 引线 / 公差 / 填充 / 表格）不参与 TR / EX：
;; 它们没有「会被剪掉的那一段」，悬停时画外框只会变成盖住半张图的大方框
;; （2026-09-21 用户反馈「不应该受到标注线影响」）；光圈变大后它们还会被
;; 算成修剪边界、被「删除修剪不到的」误删，所以统一在这里排除。
(defun ze:skip ()
  '((-4 . "<NOT")
    (0 . "TEXT,MTEXT,ATTDEF,ATTRIB,DIMENSION,LEADER,MULTILEADER,TOLERANCE,HATCH,TABLE")
    (-4 . "NOT>")))

;; 拾取点周围一个光圈大小的框，只用来「判断这个点上有没有对象」。
;; 单个选择本身已经改用裸点喂原生命令 —— 真机实测（T3）ZWCAD 并没有把裸点
;; 当成矩形框选（CMDACTIVE=0，剪掉的那一段也对），比小窗交更贴近 AutoCAD 的
;; 「拾取点决定剪哪一端」。
(defun ze:box (p / d)
  (setq d (* (/ (ze:pick) 2.0) (ze:pend)))
  (list (list (- (car p) d) (- (cadr p) d) (caddr p))
        (list (+ (car p) d) (+ (cadr p) d) (caddr p))))

(defun ze:hit (p / b ss)
  (setq b (ze:box p) ss (ssget "_C" (car b) (cadr b) (ze:skip)))
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

;; 能被修剪的曲线类对象。只有它们才有资格进「删除修剪不到的」候选：
;; 块参照 / 点 / 构造线 / 图像等原生 TRIM 本来就剪不动，进了候选就会被当成
;; 「没修剪到」整个删掉（审查发现：窗交框到图框块，整个图框被删）。
(defun ze:curvep (e / d)
  (and (setq d (entget e))
       (wcmatch (cdr (assoc 0 d)) "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,ELLIPSE,SPLINE")))

;; 把选择集里的对象存成「实体名 + 数据快照」，供操作后比对。只收曲线（见 ze:curvep）。
(defun ze:snap (ss / i n e out)
  (setq out nil)
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (setq e (ssname ss i))
        (if (and (ze:unlocked e) (ze:curvep e)) (setq out (cons (list e (ze:state e)) out)))
        (setq i (1+ i)))))
  out)

;; 离拾取点（UCS）最近的曲线；非曲线或算不出距离的跳过。ents 为实体名表。
(defun ze:nearest (ents p / w best bd e q d)
  (setq w (trans p 1 0) best nil bd nil)
  (foreach e ents
    (setq q (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list e w)))
    (if (and (not (vl-catch-all-error-p q)) q)
      (progn
        (setq d (distance (list (car q) (cadr q)) (list (car w) (cadr w))))
        (if (or (null bd) (< d bd)) (setq bd d best e)))))
  best)

;; 选择集 -> 实体名表
(defun ze:names (ss / i out)
  (setq out nil i 0)
  (if ss (while (< i (sslength ss)) (setq out (cons (ssname ss i) out) i (1+ i))))
  out)

;; 本次选择会碰到哪些对象（给「删掉修剪不到的」用）。
;; 标注类排除在外 —— 尺寸/文字本来就不参与修剪，进了候选就会被「删除修剪不到的」
;; 当成没修剪到而误删（光圈放大后特别容易发生）。
(defun ze:cand (mode plist)
  (cond
    ((equal mode "_C") (ssget "_C" (car plist) (cadr plist) (ze:skip)))
    ((equal mode "_F") (ssget "_F" plist '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,ELLIPSE,SPLINE"))))
    (T nil)))

;; 快速模式专用：AutoCAD 会「删除无法修剪的选定对象」。
;; ZWCAD 原生不会（2026-09-21 真机实测 T1：不可修剪的对象原样留着、也不报错），
;; 所以这一条得我们自己补。判据同 trim.lsp 的 zt:trim —— 操作前后实体数据没变 = 没被修剪到。
;; 这里不打印提示：AutoCAD 对这条没有可抓取的文字，不编造措辞。
;; 单个选择（single = 拾取点）时拾取框里可能有好几根线，原生 TRIM 只剪其中一根 ——
;; 不能把其余没变的也当成「剪不动」删掉（审查发现：在交点附近点一下，另一根整根消失）。
;; 所以单选只在「框里所有曲线都没变」（这一下确实什么都没剪到）时，删离拾取点最近的那一根。
;;
;; 2026-09-22 查明的删图漏洞：原来只要「修剪前后数据没变」就删。一旦原生 TRIM 因任何原因
;; 没真正执行（被取消、输入被打乱、边界选择集失效……），被划过的对象全部「没变」，
;; 于是全被删掉 —— 测试中出现过的「栏选回车后整张图被删光」就是这样来的
;; （真机复现：给原生 TRIM 一个失效的边界集，与横线相交的竖线照样被删）。
;; 现在「剪不动」必须同时满足：数据没变 + 与边界曲线真的一个交点都没有（ze:isolated）——
;; 这也正是 AutoCAD「无法修剪」的含义。原生命令失败时，相交的对象一个都不会被删。
;; IntersectWith 回来的交点表 (x y z x y z ...) 里，有没有「不在 e 自己端点上」的交点。
;; 2026-09-24 用户实测：一段线两头正好搭在别的线的端点上（中间没有任何交点），快速模式点它剪不掉。
;; AutoCAD 这时整段删掉；原生 TRIM 剪不动，而这里原来把「端点相接」也算成有交点，于是既不剪也不删。
;; 端点上的相接不算：只有线身中间真有交点（原生本该剪得动却没剪）才不删。tol 取与外框余量同一量级。
;; 闭合曲线（圆、闭合多段线）没有端点：起点 = 终点只是参数起点，交点一律都算。
(defun ze:inner-hit (e r tol / c ps pe q hit)
  (setq c (vl-catch-all-apply 'vlax-curve-isClosed (list e))
        ps (vl-catch-all-apply 'vlax-curve-getStartPoint (list e))
        pe (vl-catch-all-apply 'vlax-curve-getEndPoint (list e))
        hit nil)
  (if (or (vl-catch-all-error-p c) c
          (vl-catch-all-error-p ps) (vl-catch-all-error-p pe) (null ps) (null pe))
    (setq hit (if r T nil))            ;; 闭合或取不到端点就照旧：有交点即算
    (while (and r (not hit))
      (setq q (list (car r) (cadr r) (caddr r)) r (cdddr r))
      (if (not (or (< (distance q ps) tol) (< (distance q pe) tol)))
        (setq hit T))))
  hit)

(defun ze:isolated (e / o r mn mx p1 p2 d ss i e2 o2 hit)
  (setq hit nil o (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (vl-catch-all-error-p o)
    nil
    (progn
      (setq r (vl-catch-all-apply 'vla-getboundingbox (list o 'mn 'mx)))
      (if (or (vl-catch-all-error-p r) (null mn) (null mx))
        nil
        (progn
          (setq p1 (vlax-safearray->list mn) p2 (vlax-safearray->list mx)
                d (* 0.001 (+ 1.0 (distance p1 p2))))
          (setq p1 (trans (list (- (car p1) d) (- (cadr p1) d) (caddr p1)) 0 1)
                p2 (trans (list (+ (car p2) d) (+ (cadr p2) d) (caddr p2)) 0 1))
          (setq ss (ssget "_C" p1 p2 (ze:skip)))
          (vl-catch-all-apply 'sssetfirst (list nil nil))
          (setq i 0)
          (if ss
            (while (and (not hit) (< i (sslength ss)))
              (setq e2 (ssname ss i) i (1+ i))
              (if (and (not (eq e2 e))
                       (or (null ze:bounds) (ssmemb e2 ze:bounds)))
                (progn
                  (setq o2 (vl-catch-all-apply 'vlax-ename->vla-object (list e2)))
                  ;; 出任何错都按「有交点」算（宁可不删）：0 = acExtendNone（ZWCAD 未必预定义该常量）
                  (if (vl-catch-all-error-p o2)
                    (setq hit T)
                    (progn
                      (setq r (vl-catch-all-apply 'vlax-invoke (list o 'IntersectWith o2 0)))
                      (if (or (vl-catch-all-error-p r) (ze:inner-hit e r d)) (setq hit T))))))))
          (not hit))))))

(defun ze:trim-del (snap single / n changed near)
  (setq n 0 changed nil)
  (if single
    (progn
      (foreach it snap
        (if (not (equal (cadr it) (ze:state (car it)))) (setq changed T)))
      (if (and (not changed) (setq near (ze:nearest (mapcar 'car snap) single)))
        (if (and (ze:unlocked near) (ze:isolated near) (entdel near)) (setq n 1))))
    (foreach it snap
      (if (and (ze:unlocked (car it)) (equal (cadr it) (ze:state (car it))) (ze:isolated (car it)))
        (if (entdel (car it)) (setq n (1+ n))))))
  n)

;; 把 new 并进 ss，并照 AutoCAD 选择对象的口径报数：
;; 第一次「找到 3 个」，之后「找到 1 个，总计 4 个」，选到已选的「找到 1 个 (1 个重复)，总计 4 个」。
;; 返回合并后的选择集（可能是空集）。
(defun ze:add (ss new / t0 k d i e)
  (if (null ss) (setq ss (ssadd)))
  (setq t0 (sslength ss) k (if new (sslength new) 0) d 0 i 0)
  (while (< i k)
    (setq e (ssname new i) i (1+ i))
    (if (ssmemb e ss) (setq d (1+ d)) (ssadd e ss)))
  (princ (strcat "\n找到 " (itoa k) " 个"
                 (if (> d 0) (strcat " (" (itoa d) " 个重复)") "")
                 (if (or (> t0 0) (> d 0)) (strcat "，总计 " (itoa (sslength ss)) " 个") "")))
  ss)

;; 窗口 / 窗交按「屏幕上」从左往右还是从右往左拉来定（AutoCAD 的规则），
;; 不按 UCS 的 X 坐标比 —— 视图有转角（DVIEW 扭转）时两者不一致。
(defun ze:win (p1 p2 / m)
  (setq m (if (< (car (ze:frac p1)) (car (ze:frac p2))) "_W" "_C"))
  (ssget m p1 p2 (ze:skip)))

;; ============================================================ 几何层：委托原生
;; bnd 为 nil 表示「全部对象作边界」。
;; 注意：这里必须给原生命令一个「真实的选择集」，不能给 ""（回车=全选）——
;; 2026-09-21 在 ZWCAD 2020 上实测：传 "" 时 TRIM / EXTEND 会从头到尾走完、
;; 不报任何错、也不改图形（V1/V2/V3/V5 均无变化）；改成 (ssget "_X") 后
;; 栏选 / 窗交 / 修剪 / 延伸 四种组合全部生效（A1~A4）。
;; 全部对象作边界时同样排除标注类（ze:skip）：否则悬停预览（不把标注当边界）
;; 和实际结果（在尺寸线处断开）对不上。
(defun ze:cmd (isExt bnd mode plist tail / lst p s)
  (setq s (if bnd bnd (ssget "_X" (ze:skip))))
  (if (null s) (setq s ""))
  (setq lst (list (if isExt "_.EXTEND" "_.TRIM") s ""))
  (if mode (setq lst (append lst (list mode))))
  (foreach p plist (setq lst (append lst (list "_non" p))))
  (foreach p tail (setq lst (append lst (list p))))
  (ze:dbg (strcat "cmd=" (vl-princ-to-string (mapcar 'type lst))))
  (apply 'command lst))

;; ============================================================ 撤销
;; 与 AutoCAD 一致：命令里的「放弃(U)」一步步撤，退出后一次 U / Ctrl+Z 撤掉整条 TR / EX。
;; 2026-09-24 真机实验：ZWCAD 2020 的 UNDO 组不能嵌套 —— 内层组不会并进外层组（退出后要 U 好几次），
;; 在开着的外层组里做 (_.UNDO 1) 再结束外层组，撤销链会坏掉（一直提示「输入 Undo End Group」）。
;; 真机验证可行的写法：整条命令只开一个外层组，每步操作前打 UNDO 标记，命令里的放弃用 UNDO 后退(B)。
;; UNDO 后退找不到标记会一路撤到头，所以仍由 ze:ops 计数把关，只在确实打过标记时才后退。
(defun ze:group-begin (/ doc)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list (vlax-get-acad-object))))
  (if (not (vl-catch-all-error-p doc))
    (progn (vla-StartUndoMark doc) (setq ze:mark T))))

(defun ze:step-mark () (command "_.UNDO" "_M"))

;; ============================================================ 过期的 Shift 状态
;; 2026-09-24 真机查明：ZWCAD 记着「最后一条真实鼠标消息」里 Shift 是否按着，此后用 LISP 喂给原生命令的
;; 选择 / 拾取都按「按着 Shift」处理 —— 原生 TRIM 变成延伸、EXTEND 变成修剪，ERASE / MOVE 的选择变成反选
;; （找到 N 个、一个也没选上）；不按 Shift 真实地动一下鼠标才恢复。补发 WM_MOUSEMOVE 清不掉，
;; 选择前加「添加(A)」也没用。TR 里按住 Shift 单击时，调原生命令那一刻正是这个状态，
;; 于是「按住 Shift 延伸」原样不动（原来的老问题）。
;; 探测：新建一个临时点，让原生 SELECT 选它，看它进没进「上一个」选择集（新对象不可能在旧的集里）。
;; 临时点建在可写的图层上（冻结 / 锁定的图层上 SELECT 选不到，会误判成过期）；都不可写就不探测。
(defun ze:laywritable (name / d)
  (and name (setq d (tblsearch "LAYER" name)) (= 0 (logand 5 (cdr (assoc 70 d))))))

(defun ze:stale-shift (/ lay t0 ps r)
  (setq lay (cond ((ze:laywritable (getvar "CLAYER")) (getvar "CLAYER"))
                  ((ze:laywritable "0") "0")))
  (if (and lay (entmake (list '(0 . "POINT") (cons 8 lay) '(10 0.0 0.0 0.0))))
    (progn
      (setq t0 (entlast))
      (command "_.SELECT" t0 "")
      (setq ps (ssget "_P"))
      (entdel t0)
      (setq r (not (and ps (ssmemb t0 ps))))
      (ze:dbg (strcat "stale-shift=" (if r "T" "nil")))
      r)))

(defun ze:apply (isExt mode plist tail cand / snap sw)
  (ze:dbg (strcat "apply isExt=" (vl-princ-to-string isExt) " mode=" (vl-princ-to-string mode)
                  " plist=" (vl-princ-to-string plist) " cand=" (if cand (itoa (sslength cand)) "nil")))
  ;; 探测要在打 UNDO 标记「之前」：探测不执行时不能留下一个空标记（放弃(U) 会退到它上面白退一次）。
  (setq sw (ze:stale-shift))
  (if (and sw ze:bounds)
    ;; 过期的 Shift 状态下，传给原生命令的边界选择集会被当成反选丢掉（变成全部对象作边界），
    ;; 结果会剪 / 延到不该到的地方 —— 宁可这一下不执行。
    (princ (strcat "\n按住 Shift 时无法按指定的" (if ze:isext "边界边" "剪切边")
                   "执行，请松开 Shift、移动一下鼠标后再点。"))
    (progn
      ;; 快照必须在执行「之前」拍：操作后再拍就变成「自己跟自己比」，永远判成没修剪到，
      ;; 结果把修剪成功的对象也一起删掉（真机实测踩过）。
      (setq snap (ze:snap cand))
      (ze:step-mark)
      ;; 过期的 Shift 状态会让原生 TRIM 做延伸、EXTEND 做修剪 —— 反过来调，结果才是要的那个
      (ze:cmd (if sw (not isExt) isExt) ze:bounds mode plist tail)
  ;; 「删除无法修剪的对象」只属于快速模式；标准模式下 AutoCAD 不删（审查发现原来两种模式都删）。
  ;; mode 为 nil = 单个选择，把拾取点传进去（见 ze:trim-del）。
  ;; 原生命令没正常结束（还在命令里）就一个都不删：它根本没按预期执行
      (if (and (not isExt) (= (ze:mode) 1) (= (getvar "CMDACTIVE") 0))
        (ze:trim-del snap (if mode nil (car plist))))
      (setq ze:ops (1+ ze:ops)))))

(defun ze:close-mark (/ doc)
  (if ze:mark
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list (vlax-get-acad-object))))
      (if (not (vl-catch-all-error-p doc))
        (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
      (setq ze:mark nil))))

;; 放弃(U)：只撤本次命令里自己做的操作（ze:ops 计数），撤完就报无效选择，
;; 不会一路撤到 TR 之前别的命令（审查发现原来只记「有没有操作过」，连按会撤过头）。
(defun ze:undo ()
  (if (> ze:ops 0)
    (progn (command "_.UNDO" "_B") (setq ze:ops (1- ze:ops)) T)
    nil))

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
;; 每点一下只删一个对象（离拾取点最近的曲线；框里没有曲线就删第一个），
;; 原来是把拾取框里的所有对象一起删。每次删除前打一个 UNDO 标记，放弃(U) 能撤回。
(defun ze:opt-del (isExt / p b ss e n)
  (setq n 0)
  (while (setq p (getpoint "\n选择要删除的对象或 <退出>: "))
    (setq b (ze:box p) ss (ssget "_C" (car b) (cadr b)))
    (vl-catch-all-apply 'sssetfirst (list nil nil))
    (if ss
      (progn
        (setq e (ze:nearest (ze:names ss) p))
        (if (null e) (setq e (ssname ss 0)))
        (ze:step-mark)
        (if (entdel e) (setq n (1+ n) ze:ops (1+ ze:ops))))
      (ze:bad isExt)))
  (if (> n 0) (princ (strcat "\n已删除 " (itoa n) " 个对象。")))
  n)

;; 剪切边 / 边界边。start 为 T 表示命令起始的那一次（选择对象行带 [模式(O)]）。
;; 与 AutoCAD 选择对象一致：
;;   · 点到对象只选「一个」（离拾取点最近的那根），不是拾取框里的全部；
;;   · 点在空白处 = 拉矩形框（getcorner 画出框，不是一根橡皮线），从左往右窗口、从右往左窗交；
;;   · 报数按 AutoCAD 的「找到 1 个，总计 N 个」「(1 个重复)」口径（ze:add）；
;;   · 选的过程中按 O 切到快速模式 = 不再选剪切边，全部对象作边界（AutoCAD 切模式后直接进快速模式的提示）。
;; 返回：选择集 / nil（回车「全部选择」或切到了快速模式）。
(defun ze:pickbounds (isExt start / p hit e ss p2 go)
  (setq ss nil go T)
  (princ (strcat "\n" (ze:setline)))
  (princ (if isExt "\n选择边界边... " "\n选择剪切边..."))
  (while go
    (initget "O")
    (setq p (getpoint (strcat "\n选择对象或 " (if start "[模式(O)] " "") "<全部选择>: ")))
    (cond
      ((null p) (setq go nil))
      ((equal p "O")
        (ze:opt-mode isExt)
        (if (= (ze:mode) 1)
          (progn (setq ss nil go nil) (princ (strcat "\n" (ze:setline))))))
      ((setq hit (ze:hit p))
        (setq e (ze:nearest (ze:names hit) p))
        (if (null e) (setq e (ssname hit 0)))
        (setq ss (ze:add ss (ssadd e))))
      (T
        (setq p2 (getcorner p "\n指定对角点: "))
        (if p2
          (progn
            (setq ss (ze:add ss (ze:win p p2)))
            (vl-catch-all-apply 'sssetfirst (list nil nil)))))))
  ;; 一个都没选到 = 回车「全部选择」（nil），不要返回空选择集
  (if (and ss (> (sslength ss) 0)) ss nil))

;; 命令中途按 O 切模式：AutoCAD 切到标准模式后马上要你选剪切边 / 边界边，
;; 切回快速模式则所有对象重新都算边界。原来只改了变量，切到标准后照样按「全部对象」修剪。
(defun ze:switch-mode (isExt / old)
  (setq old (ze:mode))
  (ze:opt-mode isExt)
  (cond
    ((= (ze:mode) old) nil)
    ((= (ze:mode) 0)
      (setq ze:bounds (ze:pickbounds isExt T))
      (ze:bound-send))
    (T
      (setq ze:bounds nil)
      (ze:bound-send)
      (princ (strcat "\n" (ze:setline))))))

;; 栏选：只在标准模式下是命令选项（实机确认快速模式没有它）。
;; 第二个点起与快速模式空白处栏选同一句提示「指定下一个栏选点或 [放弃(U)]:」（AutoCAD 的 FENCE 就是这句），
;; 从上一点拉橡皮线，已定的各段用虚线画在屏幕上，U 撤掉上一点，回车执行。
(defun ze:fence-redraw (pts / a)
  (redraw)
  (setq a (car pts))
  (foreach b (cdr pts) (grdraw a b 7 1) (setq a b)))

(defun ze:opt-fence (isExt / p pts go)
  (setq pts nil go T)
  (while go
    (if pts (initget "U"))
    (setq p (if pts
              (getpoint (last pts) "\n指定下一个栏选点或 [放弃(U)]: ")
              (getpoint "\n指定第一个栏选点或拾取/拖动光标: ")))
    (cond
      ((null p) (setq go nil))
      ((equal p "U") (setq pts (reverse (cdr (reverse pts)))) (ze:fence-redraw pts))
      (T (setq pts (append pts (list p))) (ze:fence-redraw pts))))
  (redraw)
  (if (> (length pts) 1) (ze:apply isExt "_F" pts (list "" "") (ze:cand "_F" pts))))

;; 窗交：两个角点交给原生自己选，顺时针规则由原生保证。第二角用 getcorner，拖出来的是矩形框。
(defun ze:opt-cross (isExt / p1 p2)
  (setq p1 (getpoint "\n指定第一个角点: "))
  (if p1
    (progn
      (setq p2 (getcorner p1 "\n指定对角点: "))
      (if p2 (ze:apply isExt "_C" (list p1 p2) (list "") (ze:cand "_C" (list p1 p2)))))))

;; 主提示的按键分发。「是否已有可撤销操作」统一看 ze:ops（ze:apply / ze:opt-del 自己计数）。
;; 剪切边(T)/边界边(B)：选完的结果必须存进 ze:bounds（原来被丢掉，这个选项等于没用）。
(defun ze:key (isExt k / md)
  (setq md (ze:mode))
  (cond
    ((or (and (not isExt) (equal k "T")) (and isExt (equal k "B")))
      (setq ze:bounds (ze:pickbounds isExt nil))
      (ze:bound-send))
    ((equal k "C") (ze:opt-cross isExt))
    ((equal k "O") (ze:switch-mode isExt))
    ((equal k "P") (ze:opt-proj))
    ((equal k "E") (if (= md 0) (ze:opt-edge) (ze:bad isExt)))
    ((equal k "F") (if (= md 0) (ze:opt-fence isExt) (ze:bad isExt)))
    ((equal k "R") (if isExt (ze:bad isExt) (ze:opt-del isExt)))
    ((equal k "U") (if (not (ze:undo)) (ze:bad isExt)))
    (T (ze:bad isExt))))

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
  ;; 只认 "1"：setenv 成空串并不会删掉变量，原来「有值就写」会让日志关不掉
  (if (equal (getenv "ZWK_TRIMDEBUG") "1")
    (progn
      (setq f (open (strcat (getenv "TEMP") "/zwk-trim-debug.txt") "a"))
      (if f (progn (write-line s f) (close f))))))

;; A-Z 的字符（不依赖 chr）。
(defun ze:ch (n / base)
  (setq base "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (cond ((and (>= n 65) (<= n 90)) (substr base (- n 64) 1))
        ((and (>= n 97) (<= n 122)) (substr base (- n 96) 1))
        (T "")))

;; 键盘输入缓冲：与 AutoCAD 一样，选项字母要「回车 / 空格」确认才生效，打的字母回显在命令行上。
;; 原来按下字母立刻执行，照 AutoCAD 的习惯打「U 回车」时，U 撤了一步、紧跟的回车又把整条命令结束了；
;; 打「O 回车 S 回车」时回车被模式提示当成默认值吃掉、S 又报无效选择。
(setq ze:kbuf "")
(setq ze:lastp "")   ;; 当前这句提示（Backspace 后重打它的最后一行 + 已输入的字母）

;; 提示的最后一行（主提示是两行，Backspace 后只重打「 [选项]: 」那一行 + 已输入的字母；
;; ZWCAD 的命令行不认退格符，没法原地删字 —— 2026-09-24 真机确认）。
(defun ze:lastline (s / i)
  (setq i (strlen s))
  (while (and (> i 0) (/= (substr s i 1) "\n")) (setq i (1- i)))
  (substr s (1+ i)))

;; 读一个 grread 事件：("MOVE"/"PRESS"/"KEY"/"RIGHT"/"ENTER"/"CANCEL"/"OTHER") + 点或字符。
;; "KEY" 只在回车 / 空格确认时出现，带的是整串输入（大写，如 "U"）；打字过程中返回 "OTHER"。
(defun ze:next (/ k p c)
  (setq k (vl-catch-all-apply 'grread (list T 13 0)) ze:grk k)
  (cond
    ((vl-catch-all-error-p k)
      (ze:dbg (strcat "grread-err=" (vl-catch-all-error-message k)))
      (setq ze:kbuf "")
      (list "CANCEL" nil))
    ((not (listp k)) (list "OTHER" nil))
    ((= (car k) 5) (list "MOVE" (cadr k)))
    ;; 打了字又去点鼠标：按点处理，打了一半的字作废（AutoCAD 同样）
    ((= (car k) 3) (setq ze:kbuf "") (list "PRESS" (cadr k)))
    ((= (car k) 25) (setq ze:kbuf "") (list "RIGHT" (cadr k)))
    ((= (car k) 2)
      (setq p (cadr k))
      (cond ((not (numberp p)) (list "OTHER" nil))
            ((= p 27) (setq ze:kbuf "") (list "CANCEL" nil))
            ((or (= p 13) (= p 32))
              (if (= ze:kbuf "")
                (list "ENTER" nil)
                (progn (setq c ze:kbuf ze:kbuf "") (list "KEY" c))))
            ((= p 8)
              (if (> (strlen ze:kbuf) 0)
                (progn
                  (setq ze:kbuf (substr ze:kbuf 1 (1- (strlen ze:kbuf))))
                  (princ (strcat "\n" (ze:lastline ze:lastp) ze:kbuf))))
              (list "OTHER" nil))
            ((/= (setq c (ze:ch p)) "")
              (setq ze:kbuf (strcat ze:kbuf c))
              (princ c)
              (list "OTHER" nil))
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
(defun zwk:hover (f red) (ze:dll 'ZWK_HOVER_101 (append f (list red))))
(defun zwk:bound (s) (ze:dll 'ZWK_BOUND_101 (list s)))
(defun zwk:hover-end () (ze:dll 'ZWK_HOVER_END_101 nil))
(defun zwk:press (f ms) (ze:dll 'ZWK_PRESS_101 (append f (list ms))))
(defun zwk:ink (f red mode) (ze:dll 'ZWK_INK_101 (append f (list red mode))))
;; 栏选橡皮筋 + 实时预览：fl 是展开的比例坐标 (fx1 fy1 fx2 fy2 ...)，mode 0 = 修剪 / 1 = 延伸
(defun zwk:fence (mode fl) (ze:dll 'ZWK_FENCE_101 (cons mode fl)))
(defun zwk:ink-end () (ze:dll 'ZWK_INK_END_101 nil))

;; 把当前的修剪边界集合告诉 DLL：悬停预览要按它算「点下去会被剪掉的那一段」。
;; 空串 = 全部对象（快速模式的默认边界）。边界一变（T/B 选项）就得重新送一次。
(defun ze:bound-send (/ s i n e h)
  (setq s "" i 0 n (if ze:bounds (sslength ze:bounds) 0))
  (while (< i n)
    (setq e (ssname ze:bounds i))
    (if e
      (progn
        (setq h (cdr (assoc 5 (entget e))))
        (if h (setq s (strcat s (if (= s "") "" ",") h)))))
    (setq i (1+ i)))
  (zwk:bound s))

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

;; 悬停高亮：只亮「点下去会被剪掉的那一段」（AutoCAD 快速模式的预览就是这样，不是整条线）。
;; 按住 Shift 是延伸模式，不做那一段的预览（交给 DLL 整条亮）。
;; 当前该预览「延伸」吗：EX 不按 Shift、或 TR 按住 Shift（与 AutoCAD 的 Shift 互换一致）。
(defun ze:extp (shift) (if ze:isext (not shift) shift))
(defun ze:emode (shift) (if (ze:extp shift) 1 0))

(defun ze:hover (p / r)
  (setq r (zwk:hover (ze:frac p) (ze:emode (equal (zwk:shift) "1")))
        ze:hovst r)
  ;; "OFF" 是「光标不在绘图区」的正常回执（比如移到命令行上），不算故障，不报警。
  (if (and (not ze:hwarn) (or (null r) (and (/= (substr r 1 2) "OK") (/= r "OFF"))))
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
(defun ze:gesture (p / st r q pts e kind done last)
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
          ;; 拖动：笔画跟着鼠标走，松开时结算成一笔栏选。
          ;; 输入循环必须是 grread：CAD 主线程一旦忙着轮询（不处理消息），整个系统的鼠标移动都会
          ;; 卡住（2026-09-22 真机实测：轮询期间光标位置一直不变）。grread 不报告「松开左键」，
          ;; 由 DLL 的底层鼠标钩子在松手后补发一次移动消息把 grread 叫醒（见 Mouse.cs PostCursorMove）。
          (setq pts (list p))
          (zwk:ink (ze:frac p) (if st 1 0) (ze:emode st))
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
                      (progn (setq pts (cons q pts)) (zwk:ink (ze:frac q) (if st 1 0) (ze:emode st))))
                    (if (equal (zwk:shift) "1") (setq st T)))))
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
;; TR / EX 期间把十字缩到最小，让 DLL 贴的那个「拾取框方块」成为唯一显眼的东西 ——
;; 就是用户要的「进了 TR / EX 才变成方块，其余时间不变」（2026-09-21）。
;; 触发范围与十字大小无关：光圈 = PICKBOX（见 ze:pick），方块画多大、触发就多大。
;; CURSORSIZE 是全局设置，退出时必须原样还原，所以挂在 ze:restore 上，
;; 正常结束和出错（*error*）两条路都会经过它；ZWCAD 不支持该变量就自动跳过。
;;
;; 2026-09-22 真机复现的漏洞：TR 运行中直接关掉 / 切走这张图，这张图的 LISP 被整个丢弃，
;; *error* 根本不会执行 —— CURSORSIZE 是全局设置，于是一直停在 1（别的图也是 1）。
;; 更糟的是下次再进 TR 会把「1」当成原值存下来，退出时「还原」成 1，从此回不去。
;; 所以现在：
;;   1) 原值另存一份到 ZWK_CURSOR_ORIG（注册表环境变量，跨图纸、跨重启）；进 TR 时如果
;;      当前已经是 1，就用存下的原值，不再把 1 当原值；
;;   2) 用黑板变量 zwk:trdoc（所有图纸共享）记「哪张图正在跑 TR」，退出时清掉；
;;   3) ze:cursor-heal：光标是 1、有原值、且当前图纸没在跑 TR 时自动还原 ——
;;      每张图加载本文件时调一次，另挂命令反应器：任何命令开始时都检查一次。
(setq ze:cur nil)

(defun ze:cursor-orig (/ s n)
  (setq s (vl-catch-all-apply 'getenv (list "ZWK_CURSOR_ORIG")))
  (if (and (not (vl-catch-all-error-p s)) (= (type s) 'STR)) (setq n (atoi s)))
  (if (and n (> n 1) (<= n 100)) n nil))

(defun ze:cursor-on (/ v o)
  (setq v (vl-catch-all-apply 'getvar (list "CURSORSIZE")))
  (if (vl-catch-all-error-p v)
    nil
    (progn
      (setq o (ze:cursor-orig))
      (cond
        ((and (numberp v) (> v 1))
          (vl-catch-all-apply 'setenv (list "ZWK_CURSOR_ORIG" (itoa (fix v)))))
        (o (setq v o)))                 ;; 已经是 1：上次没还原，用存下的原值
      (setq ze:cur v)
      (vl-catch-all-apply 'vl-bb-set (list 'zwk:trdoc (getvar "DWGNAME")))
      (vl-catch-all-apply 'setvar (list "CURSORSIZE" 1)))))

(defun ze:cursor-off ()
  (if ze:cur
    (progn
      (vl-catch-all-apply 'setvar (list "CURSORSIZE" (fix ze:cur)))
      (setq ze:cur nil)))
  (vl-catch-all-apply 'vl-bb-set (list 'zwk:trdoc nil)))

;; 光标卡在 1、而当前图纸并没有在跑 TR / EX 时，还原成存下的原值。
;; force = T：调用方已确定本图的 TR 不在运行（例如本图开始了别的命令），
;; 把残留的 ze:cur / 黑板标记一并清掉（收尾被打断时会残留）。
(defun ze:cursor-heal (force / o t1 busy)
  (if force
    (progn
      (setq ze:cur nil)
      (if (equal (vl-catch-all-apply 'vl-bb-ref (list 'zwk:trdoc)) (getvar "DWGNAME"))
        (vl-catch-all-apply 'vl-bb-set (list 'zwk:trdoc nil)))))
  (setq o (ze:cursor-orig)
        t1 (vl-catch-all-apply 'vl-bb-ref (list 'zwk:trdoc)))
  (if (vl-catch-all-error-p t1) (setq t1 nil))
  ;; 本图正在跑 TR（ze:cur 有值且黑板记的就是本图）时不动它
  (setq busy (and ze:cur t1 (= t1 (getvar "DWGNAME"))))
  (if (and o (not busy) (equal (getvar "CURSORSIZE") 1))
    (vl-catch-all-apply 'setvar (list "CURSORSIZE" o)))
  (princ))

;; 命令反应器回调：本图开始了 TRIM / EXTEND / UNDO 以外的命令 = 本图的 TR 肯定不在运行
;; （TR 运行期间只会自己调这三个命令），可以强制自检。
(defun ze:cmd-react (reactor args / c)
  (setq c (strcase (if (car args) (car args) "")))
  (if (not (member c '("TRIM" "EXTEND" "UNDO" "U")))
    (vl-catch-all-apply 'ze:cursor-heal (list T))))

(defun ze:restore (echo snap)
  (ze:cursor-off)
  (if echo (setvar "CMDECHO" echo))
  (if snap (setvar "OSMODE" snap))
  (redraw))

;; ============================================================ 主循环
;; ============================================================ 栏选（在空白处单击开始）
;; 与 AutoCAD 2025 快速模式一致：空白处单击 = 栏选第一点；之后橡皮筋虚线跟着光标（起点红 ×），
;; 被穿过的对象实时预览，提示「指定下一个栏选点或 [放弃(U)]:」；点第二下立即按这条线执行。
;; U（回车确认）/ 回车 / 空格 / 右键 = 放弃这次栏选、回到主提示；Esc 取消整个命令。
;; 返回：点表（2 个点）/ nil（没形成栏选）/ 'cancel。
(defun ze:fence-show (pl / fl)
  (setq fl nil)
  (foreach p pl (setq fl (append fl (ze:frac p))))
  (zwk:fence (ze:emode (equal (zwk:shift) "1")) fl))

(defun ze:fence (start / pts e kind q done res)
  (setq pts (list start) done nil res nil)
  (princ (setq ze:lastp "\n指定下一个栏选点或 [放弃(U)]: "))
  (while (not done)
    (setq e (ze:next) kind (car e))
    (cond
      ((equal kind "MOVE")
        (ze:fence-show (append pts (list (cadr e)))))
      ((equal kind "PRESS")
        (setq q (cadr e))
        (zwk:press (ze:frac q) 400)                 ;; 等松开：按住不放也只算一个点
        ;; 与 AutoCAD 快速模式一致：点第二下就按这条两点栏选线立即修剪 / 延伸，不用再按空格
        ;; （2026-09-22 用户：点一下、再点一下就执行）
        (setq pts (append pts (list q)) done T res pts))
      ((and (equal kind "KEY") (equal (cadr e) "U"))
        (setq done T res nil))                      ;; 只有起点，放弃 = 退出栏选
      ((equal kind "KEY")                           ;; 别的字母：getpoint 同款报错，再问一遍
        (princ "\n需要点或选项关键字。")
        (princ ze:lastp))
      ((or (equal kind "ENTER") (equal kind "RIGHT"))
        (setq done T res nil))
      ((equal kind "CANCEL") (setq done T res 'cancel))))
  (zwk:ink-end)
  (zwk:hover-end)
  res)

(defun ze:loop (isExt / res kind data pts act go hit fp)
  (setq go T ze:ops 0 ze:isext isExt ze:kbuf "")
  (ze:bound-send)          ;; 把边界集合告诉 DLL（悬停预览要算「会被剪掉的那一段」）
  (princ (setq ze:lastp (ze:prompt isExt nil)))
  (while go
    (setq res (ze:capture) kind (car res) data (cadr res))
    (ze:dbg (strcat "loop kind=" kind " n=" (if (listp data) (vl-princ-to-string (length data)) (vl-princ-to-string data))))
    (cond
      ((equal kind "KEY")
        (ze:key isExt data))
      ((equal kind "STROKE")
        ;; data 已经是 UCS 点（grread 给的就是 UCS），不用再过 ze:wpts（那是比例坐标用的）。
        (setq pts data
              act (if ze:shift (not isExt) isExt))
        (cond
          ((= (length pts) 1)
            (if (setq hit (ze:hit (car pts)))
              (ze:apply act nil (list (car pts)) (list "") hit)
              ;; 空白处单击：进入栏选
              (progn
                (setq fp (ze:fence (car pts)))
                (cond
                  ((equal fp 'cancel) (setq go nil))
                  (fp
                    ;; 结束栏选那一刻按着 Shift 就互换修剪 / 延伸
                    (setq act (ze:extp (equal (zwk:shift) "1")))
                    (ze:apply act "_F" fp (list "" "") (ze:cand "_F" fp)))))))
          (T (ze:apply act "_F" pts (list "" "") (ze:cand "_F" pts)))))
      (T (setq go nil)))
    (if go (princ (setq ze:lastp (ze:prompt isExt (> ze:ops 0))))))
  (princ))

(defun ze:run (isExt / *error* echo snap)
  (setq echo (getvar "CMDECHO") snap (getvar "OSMODE"))
  (defun *error* (msg)
    ;; 光标第一个还原：后面的步骤可能被再一次取消打断（工具栏按钮宏自带 ^C^C）
    (ze:cursor-off)
    ;; 预览 / 栏选虚线是 CAD 的临时图形：出错退出时必须撤掉，否则会一直留在图上
    (vl-catch-all-apply 'zwk:ink-end nil)
    (vl-catch-all-apply 'zwk:hover-end nil)
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
      ;; 光标只在真正进入交互时才缩小：放在检查之前的话，DLL 未就绪 / 非平面视图
      ;; 两个分支直接返回，CURSORSIZE 会永久停在 1（它存注册表）。
      (ze:cursor-on)
      (setq ze:bounds nil)
      (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
      (if (= (ze:mode) 0)
        (setq ze:bounds (ze:pickbounds isExt T))
        (princ (strcat "\n" (ze:setline))))
      (ze:group-begin)
      (ze:loop isExt)
      (ze:close-mark)
      (ze:restore echo snap)))
  (princ))

;; ============================================================ 命令
;; TR / EX 是完整复刻版；拖动版仍是 ZT / TRD / EXD（trim.lsp / extend.lsp），
;; TRC / EXC 作为 TR / EX 的别名。
(defun c:TR () (ze:run nil))
(defun c:TRC () (ze:run nil))
(defun c:EX () (ze:run T))
(defun c:EXC () (ze:run T))
;; 光标自愈：本图加载时先检查一次（上一张图 TR 中途被关时，光标会停在 1），
;; 再挂命令反应器。重复加载本文件时先摘掉旧的反应器，避免叠加。
(if (and (not (ze:cursor-orig)) (numberp (getvar "CURSORSIZE")) (> (getvar "CURSORSIZE") 1))
  (vl-catch-all-apply 'setenv (list "ZWK_CURSOR_ORIG" (itoa (getvar "CURSORSIZE")))))
(vl-catch-all-apply 'ze:cursor-heal (list nil))
(if (and ze:react (= (type ze:react) 'VLR-Command-Reactor))
  (vl-catch-all-apply 'vlr-remove (list ze:react)))
(setq ze:react
  (vl-catch-all-apply 'vlr-command-reactor
    (list nil '((:vlr-commandWillStart . ze:cmd-react)))))
(if (vl-catch-all-error-p ze:react) (setq ze:react nil))

(princ "\nTR / EX 已按 AutoCAD 2025 复刻：提示与选项逐字一致，按 TRIMEXTENDMODE 决定快速或标准模式。")
(princ)