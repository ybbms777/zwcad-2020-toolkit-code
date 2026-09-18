;;; TK: 标题框缩放。GBK 编码。
;;; 图框自动缩放并套到零件上（零件不动，图框跟着走）。
;;;
;;; 前几版踩过的坑，别改回去：
;;;  1) vla-getboundingbox 返回 WCS，command 按 UCS 解释点，传 command 前必须 (trans p 0 1)。
;;;  2) grdraw 画的辅助线会被重绘冲掉，看不见；必须用真实实体（画完删掉）。
;;;  3) 「矩形」是点的点对 ((x1 y1 z) (x2 y2 z))，(car 矩形) 拿到的是「点」不是数字，
;;;     拿去 min/max/vl-sort 会报「函数参数类型不正确」。取坐标一律走 tk:rx1/ry1/rx2/ry2。
;;;  4) 最大空矩形必须先把遮挡物裁到内框里，否则遮挡物在内框外面时会算出跑到框外的可用区。
;;;  5) 线这类对象包围盒有一边是 0 宽/高，要先撑开一点，否则等于没有遮挡。
;;; 注意：不要用 let —— 标准 AutoLISP 没有这个宏，只用 setq + defun 局部变量。
(vl-load-com)

(setq tk:boxents nil)
(setq tk:mL 0.0 tk:mR 0.0 tk:mB 0.0 tk:mT 0.0)
;; 命令开始时保存的系统变量原值，出错或结束时一定恢复
(setq tk:os0 nil tk:ce0 nil)

(defun tk:save-vars ()
  (setq tk:os0 (getvar "OSMODE") tk:ce0 (getvar "CMDECHO")))

(defun tk:restore-vars ()
  (if tk:os0 (vl-catch-all-apply 'setvar (list "OSMODE" tk:os0)))
  (if tk:ce0 (vl-catch-all-apply 'setvar (list "CMDECHO" tk:ce0)))
  (setq tk:os0 nil tk:ce0 nil))

;; ---------- 坐标系：内部一律用 WCS ----------
;; vla-getboundingbox 返回 WCS，而 getpoint / getcorner 返回 UCS，command 又按 UCS 解释点。
;; 所以：取点后先 tk:u2w 转成 WCS 再算；传给 command 前再 tk:w2u 转回 UCS。
;; 混用会导致辅助框画到十万八千里外、遮挡物被误判到框外（踩过这个坑）。
(defun tk:w2u (p) (trans p 0 1))
(defun tk:u2w (p) (trans p 1 0))

;; ---------- 包围盒 ----------
(defun tk:ebbox (e / o mn mx)
  (setq o (vlax-ename->vla-object e))
  (vla-getboundingbox o 'mn 'mx)
  (list (vlax-safearray->list mn) (vlax-safearray->list mx)))

(defun tk:ssbox (ss / i e bb mn mx a b)
  (setq i 0 mn nil mx nil)
  (while (< i (sslength ss))
    (setq e (ssname ss i) i (1+ i))
    (setq bb (tk:ebbox e))
    (if bb
      (progn
        (setq a (car bb) b (cadr bb))
        (if (null mn)
          (setq mn (list (car a) (cadr a) 0.0) mx (list (car b) (cadr b) 0.0))
          (setq mn (list (min (car mn) (car a)) (min (cadr mn) (cadr a)) 0.0)
                mx (list (max (car mx) (car b)) (max (cadr mx) (cadr b)) 0.0))))))
  (if mn (list mn mx) nil))

;; ---------- 矩形取值：一律走这四个，别直接 (car 矩形) ----------
(defun tk:rx1 (r) (car (car r)))
(defun tk:ry1 (r) (cadr (car r)))
(defun tk:rx2 (r) (car (cadr r)))
(defun tk:ry2 (r) (cadr (cadr r)))

;; 每个对象各自的包围盒；线这类一边为 0 的先撑开，否则挡不住东西
(defun tk:allbox (ss / i e bb out eps)
  (setq i 0 out nil eps 0.01)
  (while (< i (sslength ss))
    (setq e (ssname ss i) i (1+ i))
    (setq bb (tk:ebbox e))
    (if bb
      (progn
        (if (<= (tk:rx2 bb) (tk:rx1 bb))
          (setq bb (list (list (- (tk:rx1 bb) eps) (tk:ry1 bb) 0.0)
                         (list (+ (tk:rx2 bb) eps) (tk:ry2 bb) 0.0))))
        (if (<= (tk:ry2 bb) (tk:ry1 bb))
          (setq bb (list (list (tk:rx1 bb) (- (tk:ry1 bb) eps) 0.0)
                         (list (tk:rx2 bb) (+ (tk:ry2 bb) eps) 0.0))))
        (setq out (cons bb out)))))
  out)

;; 把矩形裁到 R 里面；完全不相交返回 nil
;; 注意：AutoLISP 符号名不区分大小写，(r R ...) 会被当成重名报「参数名重复」
(defun tk:clip (rect box / x1 y1 x2 y2)
  (setq x1 (max (tk:rx1 rect) (tk:rx1 box)) y1 (max (tk:ry1 rect) (tk:ry1 box)))
  (setq x2 (min (tk:rx2 rect) (tk:rx2 box)) y2 (min (tk:ry2 rect) (tk:ry2 box)))
  (if (or (>= x1 x2) (>= y1 y2)) nil (list (list x1 y1 0.0) (list x2 y2 0.0))))

(defun tk:boxinfo (bb / a b)
  (if bb
    (progn
      (setq a (car bb) b (cadr bb))
      (list (- (car b) (car a))
            (- (cadr b) (cadr a))
            (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0) 0.0)))
    nil))

(defun tk:num (x) (rtos x 2 3))

;; 守卫：这里要的是 (宽 高 中心点)，不是矩形 ((左下点) (右上点))。
;; 传错的话 tk:num 会收到「点」，报「类型不正确 - (x y 0.0)」，很难定位。
(defun tk:boxok (b what)
  (if (and b (numberp (car b)) (numberp (cadr b)))
    T
    (progn
      (princ (strcat "\n      内部错误：" what " 数据格式不对（应为 宽 高 中心点），已取消"))
      nil)))

;; 每次 ssget 之后清掉选择集：否则对象一直处于选中（带夹点）状态，
;; 会干扰下一步的框选，用户也会看到整个图框一直高亮。
(defun tk:clear-sel () (vl-catch-all-apply 'sssetfirst (list nil nil)))

;; ---------- 取点：临时关掉对象捕捉，用准星原始位置；返回 WCS ----------
;; 恢复用命令开始时保存的 tk:os0，不是就地读——否则中途按 Esc 会让 OSMODE 永远停在 0，
;; 整个 CAD 会话都没有捕捉了（踩过这个坑）。Esc 时由 *error* 兜底恢复。
(defun tk:getpt (msg / p)
  (setvar "OSMODE" 0)
  (setq p (getpoint msg))
  (setvar "OSMODE" (if tk:os0 tk:os0 4133))
  (if p (tk:u2w p) nil))

;; ---------- 辅助显示：洋红实体（用完删除） ----------
(defun tk:mag (e col / d c)
  (setq c (if col col 6))
  (if e
    (progn
      (setq d (entget e))
      (entmod (if (assoc 62 d) (subst (cons 62 c) (assoc 62 d) d) (append d (list (cons 62 c)))))
      (setq tk:boxents (cons e tk:boxents))))
  nil)

(defun tk:erase-box ()
  (if tk:boxents
    (foreach e tk:boxents (if (entget e) (entdel e))))
  (setq tk:boxents nil))

;; 画辅助矩形。颜色：2=黄（圈出的遮挡物）5=蓝（可用区）6=洋红（零件范围）
(defun tk:rectc (p1 p2 col / old e)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.RECTANG" (tk:w2u p1) (tk:w2u p2))
  (setvar "CMDECHO" old)
  (setq e (entlast))
  (if (and e (= (cdr (assoc 0 (entget e))) "LWPOLYLINE")) (tk:mag e col))
  nil)

(defun tk:rect (p1 p2) (tk:rectc p1 p2 6))

(defun tk:dot (p r / old e)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.CIRCLE" (tk:w2u p) r)
  (setvar "CMDECHO" old)
  (setq e (entlast))
  (if (and e (= (cdr (assoc 0 (entget e))) "CIRCLE")) (tk:mag e))
  nil)

(defun tk:mark-edge (mn mx r / x1 y1 x2 y2 xm ym)
  (setq x1 (car mn) y1 (cadr mn) x2 (car mx) y2 (cadr mx))
  (setq xm (/ (+ x1 x2) 2.0) ym (/ (+ y1 y2) 2.0))
  (tk:dot (list x1 ym 0.0) r)
  (tk:dot (list x2 ym 0.0) r)
  (tk:dot (list xm y1 0.0) r)
  (tk:dot (list xm y2 0.0) r))

;; ---------- 矩形合并 / 最大空矩形 ----------
(defun tk:rtouch (a b)
  (and (<= (tk:rx1 a) (tk:rx2 b)) (>= (tk:rx2 a) (tk:rx1 b))
       (<= (tk:ry1 a) (tk:ry2 b)) (>= (tk:ry2 a) (tk:ry1 b))))

(defun tk:runion (a b)
  (list (list (min (tk:rx1 a) (tk:rx1 b)) (min (tk:ry1 a) (tk:ry1 b)) 0.0)
        (list (max (tk:rx2 a) (tk:rx2 b)) (max (tk:ry2 a) (tk:ry2 b)) 0.0)))

(defun tk:merge-rects (rects / changed out a rest b)
  (setq changed T)
  (while changed
    (setq changed nil out nil)
    (while rects
      (setq a (car rects) rects (cdr rects) rest nil)
      (foreach b rects
        (if (tk:rtouch a b)
          (progn (setq a (tk:runion a b)) (setq changed T))
          (setq rest (cons b rest))))
      (setq rects (reverse rest))
      (setq out (cons a out)))
    (setq rects (reverse out)))
  rects)

(defun tk:usort (lst / out)
  (setq out nil)
  (foreach v (vl-sort lst '<) (if (not (member v out)) (setq out (append out (list v)))))
  out)

(defun tk:cellused (obs xa xb ya yb / hit)
  (setq hit nil)
  (foreach o obs
    (if (and (< xa (tk:rx2 o)) (> xb (tk:rx1 o))
             (< ya (tk:ry2 o)) (> yb (tk:ry1 o)))
      (setq hit T)))
  hit)

;; 在 R 里、避开 obs 的最大空矩形。obs 必须已经裁进 R，否则会算出跑到 R 外面的框。
(defun tk:maxrect (R obs / xs ys nx ny best bar i1 i2 j1 j2 i j xa xb ya yb ok ar)
  (setq xs (tk:usort (append (list (tk:rx1 R) (tk:rx2 R))
                             (mapcar 'tk:rx1 obs) (mapcar 'tk:rx2 obs))))
  (setq ys (tk:usort (append (list (tk:ry1 R) (tk:ry2 R))
                             (mapcar 'tk:ry1 obs) (mapcar 'tk:ry2 obs))))
  (setq nx (1- (length xs)) ny (1- (length ys)))
  (setq best nil bar -1.0)
  (setq i1 0)
  (while (< i1 nx)
    (setq j1 0)
    (while (< j1 ny)
      (setq i2 i1)
      (while (< i2 nx)
        (setq j2 j1)
        (while (< j2 ny)
          (setq xa (nth i1 xs) xb (nth (1+ i2) xs) ya (nth j1 ys) yb (nth (1+ j2) ys))
          (setq ok T i i1)
          (while (and ok (< i (1+ i2)))
            (setq j j1)
            (while (and ok (< j (1+ j2)))
              (if (tk:cellused obs (nth i xs) (nth (1+ i) xs) (nth j ys) (nth (1+ j) ys))
                (setq ok nil))
              (setq j (1+ j)))
            (setq i (1+ i)))
          (if ok
            (progn
              (setq ar (* (- xb xa) (- yb ya)))
              (if (> ar bar)
                (setq bar ar best (list (list xa ya 0.0) (list xb yb 0.0))))))
          (setq j2 (1+ j2)))
        (setq i2 (1+ i2)))
      (setq j1 (1+ j1)))
    (setq i1 (1+ i1)))
  best)

;; ---------- 解析 "105.95,117.9" ----------
(defun tk:parse2 (s / i c buf res)
  (setq res nil buf "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (member c '("," "，" " " "x" "X" "*" "×"))
      (progn (if (/= buf "") (setq res (cons (atof buf) res))) (setq buf ""))
      (setq buf (strcat buf c)))
    (setq i (1+ i)))
  (if (/= buf "") (setq res (cons (atof buf) res)))
  (reverse res))

;; ---------- 标准比例 ----------
(setq tk:std '(0.01 0.02 0.05 0.1 0.2 0.25 0.5 0.75 1.0 1.5 2.0 2.5 3.0 4.0 5.0 10.0))
(defun tk:snap (f / r)
  (setq r nil)
  (foreach v tk:std (if (and (null r) (>= v f)) (setq r v)))
  (if r r f))

;; 缩放系数：可用区要装下「零件 + 四边余量」
(defun tk:calc (uw uh pw ph mode / s)
  (setq s (max (/ (+ pw tk:mL tk:mR) uw) (/ (+ ph tk:mB tk:mT) uh)))
  (if (= mode "std") (setq s (tk:snap s)))
  s)

;; ---------- [1/5] 选图框 ----------
(defun tk:frame (/ ss r i)
  (princ "\n[1/5] 框选【整个图框】（边框 + 标题栏一起），回车结束选择: ")
  (setq ss (ssget))
  (tk:clear-sel)
  (if (null ss)
    (progn (princ "\n      没有选到对象。") nil)
    (progn
      (setq r (vl-catch-all-apply 'tk:ssbox (list ss)))
      (if (or (vl-catch-all-error-p r) (null r))
        (progn (princ "\n      算不出图框范围，请确认框选的是图框本身。") nil)
        (progn
          (setq i (tk:boxinfo r))
          (princ (strcat "\n      图框外框 = " (tk:num (car i)) " x " (tk:num (cadr i))))
          (list ss r))))))

;; ---------- [2/5] 内边框 ----------
(defun tk:inner (bb / p1 p2 mn mx i)
  (setq p1 (getpoint "\n[2/5] 点【图框内边框】第一个角点（回车=改用图框外框，快）: "))
  (if (null p1)
    (progn
      (setq i (tk:boxinfo bb))
      (princ (strcat "\n      内边框 = 图框外框 " (tk:num (car i)) " x " (tk:num (cadr i))))
      (list (car bb) (cadr bb)))
    (progn
      (setq p2 (getcorner p1 "\n      点对角: "))
      (if (null p2)
        nil
        (progn
          ;; 取到的点是 UCS，先换成 WCS 再算范围
          (setq p1 (tk:u2w p1) p2 (tk:u2w p2))
          (setq mn (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)) 0.0))
          (setq mx (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)) 0.0))
          (setq i (tk:boxinfo (list mn mx)))
          (princ (strcat "\n      内边框 = " (tk:num (car i)) " x " (tk:num (cadr i))))
          (list mn mx))))))

;; ---------- [3/5] 可用区：圈出遮挡物（点两个对角点，可圈多块） ----------
;; 不用 ssget：整个图框常常是一个块，里面的标题栏/变更表根本选不上
;; （选到的永远是整个图框块，裁进内框就把内框全盖住，等于没减）。
;; 改成让用户直接圈区域，块 / 锁定图层 / 外部参照都不影响。
;; 每圈一块就画黄框，圈完画蓝框（可用区）并让用户确认——看不到就判断不了圈对没有。
(defun tk:usable (inner / p1 p2 mn mx obs obs2 u i ii n done again)
  (setq again T)
  (while again
    (tk:erase-box)
    (princ "\n[3/5] 圈出【图框内遮挡物】（标题栏、变更记录表等）")
    (princ "\n      点遮挡物的两个对角点；可以圈多块；没有遮挡物就直接回车")
    (setq obs nil n 0 done nil)
    (while (not done)
      (setq p1 (getpoint (if (= n 0)
                           "\n      点第 1 块遮挡物的第一个角点（回车=没有遮挡物）: "
                           "\n      继续圈下一块（回车=圈完）: ")))
      (if (null p1)
        (setq done T)
        (progn
          (setq p2 (getcorner p1 "\n      点对角: "))
          (if (null p2)
            (setq done T)
            (progn
              (setq p1 (tk:u2w p1) p2 (tk:u2w p2))
              (setq mn (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)) 0.0))
              (setq mx (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)) 0.0))
              (setq obs (cons (list mn mx) obs))
              (setq n (1+ n))
              (tk:rectc mn mx 2)
              (princ (strcat "\n      已圈第 " (itoa n) " 块（黄框，继续圈或回车结束）")))))))
    ;; 裁进内框：遮挡物在内框外面时，不裁会算出跑到框外的可用区
    (setq obs2 nil)
    (foreach r obs
      (if (tk:clip r inner) (setq obs2 (cons (tk:clip r inner) obs2))))
    (setq obs obs2)
    (if (> n 0)
      (princ (strcat "\n      圈了 " (itoa n) " 块，落在内边框里的有 " (itoa (length obs)) " 块")))
    (if obs
      (setq u (tk:maxrect inner obs))
      (progn
        (princ "\n      无遮挡物，可用区 = 内边框")
        (setq u inner)))
    ;; 兜底：可用区必须完全在内边框里
    (if (or (null u)
            (< (tk:rx1 u) (tk:rx1 inner)) (< (tk:ry1 u) (tk:ry1 inner))
            (> (tk:rx2 u) (tk:rx2 inner)) (> (tk:ry2 u) (tk:ry2 inner)))
      (progn
        (princ "\n      警告：可用区算出异常，已回退为整个内边框")
        (setq u inner)))
    (setq ii (tk:boxinfo inner))
    (setq i (tk:boxinfo u))
    (princ (strcat "\n      内边框 = " (tk:num (car ii)) " x " (tk:num (cadr ii))))
    (princ (strcat "\n      可用区 = " (tk:num (car i)) " x " (tk:num (cadr i))))
    (princ "\n      黄框 = 你圈的遮挡物，蓝框 = 算出的可用区（零件要装进这个框）")
    (tk:rectc (car u) (cadr u) 5)
    (initget "R")
    (if (= (getkword "\n      可用区对吗？[回车=是 / R=重新圈]: ") "R")
      (princ "\n      重新圈遮挡物。")
      (setq again nil)))
  (setq ii (tk:boxinfo inner))
  (setq i (tk:boxinfo u))
  (princ (strcat "\n      内边框 = " (tk:num (car ii)) " x " (tk:num (cadr ii))
                 "   可用区 = " (tk:num (car i)) " x " (tk:num (cadr i))))
  i)

;; ---------- [4/5] 点零件 4 个极值（关捕捉，用准星原始位置） ----------
(defun tk:ext (msg / p)
  (setq p (tk:getpt msg))
  (if p
    (princ (strcat "\n        已记录 " (tk:num (car p)) " , " (tk:num (cadr p)))))
  p)

(defun tk:part-points (pL / pR pB pT mn mx i r)
  (setq r nil)
  (while (null r)
    (if (null pL) (setq pL (tk:ext "\n      点【零件最左】位置（关捕捉，用准星）: ")))
    (if (null pL)
      (setq r 'cancel)
      (progn
        (setq pR (tk:ext "\n      点【零件最右】位置: "))
        (if (null pR)
          (setq r 'cancel)
          (progn
            (setq pB (tk:ext "\n      点【零件最下】位置: "))
            (if (null pB)
              (setq r 'cancel)
              (progn
                (setq pT (tk:ext "\n      点【零件最上】位置: "))
                (if (null pT)
                  (setq r 'cancel)
                  (progn
                    (setq mn (list (min (car pL) (car pR)) (min (cadr pB) (cadr pT)) 0.0))
                    (setq mx (list (max (car pL) (car pR)) (max (cadr pB) (cadr pT)) 0.0))
                    (setq i (tk:boxinfo (list mn mx)))
                    (tk:rect mn mx)
                    (tk:mark-edge mn mx (/ (max (car i) (cadr i)) 80.0))
                    (princ "\n")
                    (princ "\n      -- 零件范围已用洋红框标出（4 个圆点在四条边中点上）--")
                    (princ "\n        你点的 4 个位置：")
                    (princ (strcat "\n          最左点  " (tk:num (car pL)) " , " (tk:num (cadr pL))))
                    (princ (strcat "\n          最右点  " (tk:num (car pR)) " , " (tk:num (cadr pR))))
                    (princ (strcat "\n          最下点  " (tk:num (car pB)) " , " (tk:num (cadr pB))))
                    (princ (strcat "\n          最上点  " (tk:num (car pT)) " , " (tk:num (cadr pT))))
                    (princ "\n        合成后的包围盒（取 4 个点的最小/最大）：")
                    (princ (strcat "\n          左 X = " (tk:num (car mn)) "   右 X = " (tk:num (car mx))))
                    (princ (strcat "\n          下 Y = " (tk:num (cadr mn)) "   上 Y = " (tk:num (cadr mx))))
                    (princ (strcat "\n          宽 x 高 = " (tk:num (car i)) " x " (tk:num (cadr i))))
                    (initget "R")
                    (if (= (getkword "\n      贴住零件最外沿了吗？[回车=是 / R=重新点]: ") "R")
                      (progn (tk:erase-box) (setq pL nil) (princ "\n      重新点 4 个极值。"))
                      (setq r i)))))))))))
  (if (eq r 'cancel) nil r))

(defun tk:part-select (/ ss r info done)
  (setq info nil done nil)
  (while (not done)
    (princ "\n      框选零件（把最左/最右/最上/最下都框进去），回车结束: ")
    (setq ss (ssget))
    (tk:clear-sel)
    (if (null ss)
      (setq done T)
      (progn
        (setq r (vl-catch-all-apply 'tk:ssbox (list ss)))
        (if (or (vl-catch-all-error-p r) (null r))
          (princ "\n      算不出范围，请重新框选。")
          (progn
            (setq info (tk:boxinfo r))
            (tk:rect (car r) (cadr r))
            (princ "\n")
            (princ "\n      -- 零件范围已用洋红框标出，请核对 --")
            (princ (strcat "\n        最左 X = " (tk:num (car (car r)))))
            (princ (strcat "\n        最右 X = " (tk:num (car (cadr r)))))
            (princ (strcat "\n        最下 Y = " (tk:num (cadr (car r)))))
            (princ (strcat "\n        最上 Y = " (tk:num (cadr (cadr r)))))
            (princ (strcat "\n        宽 x 高 = " (tk:num (car info)) " x " (tk:num (cadr info))))
            (initget "R")
            (if (= (getkword "\n      贴住零件最外沿了吗？[回车=是 / R=重新框选]: ") "R")
              (progn (tk:erase-box) (setq info nil) (princ "\n      重新框选。"))
              (setq done T)))))))
  info)

(defun tk:part (/ pL)
  ;; 只保留一个「最左」提示：原来提示出现两次，用户会以为第一次点击没被记录
  (princ "\n[4/5] 依次点零件的最左 / 最右 / 最下 / 最上 四个位置（关捕捉，用准星）")
  (setq pL (tk:getpt "\n      点【零件最左】位置（回车=改为框选零件自动量）: "))
  ;; 最左点也要打印已记录：tk:getpt 不打印，只有 tk:ext 打印，
  ;; 漏掉这一行用户会以为最左没被记录（其它三个都有）
  (if pL (princ (strcat "\n        已记录 " (tk:num (car pL)) " , " (tk:num (cadr pL)))))
  (if (null pL) (tk:part-select) (tk:part-points pL)))

;; ---------- [5/5] 尺寸 -> 余量 -> 预览确认 ----------
(defun tk:ask-mm (msg dflt / s)
  (setq s (getstring msg))
  (if (and s (/= s "")) (atof s) dflt))

(defun tk:ask-margins (/ s)
  (setq s (getstring "\n      要留余量吗？[回车=不留（零件贴边）/ Y=留（按 mm 分上下左右设）]: "))
  (if (and s (member (strcase s) '("Y" "YES" "是" "1")))
    (progn
      (princ "\n      输入四边余量，单位 mm（直接回车用默认 5）:")
      (setq tk:mT (tk:ask-mm "\n        上余量 mm <5>: " 5.0))
      (setq tk:mB (tk:ask-mm "\n        下余量 mm <5>: " 5.0))
      (setq tk:mL (tk:ask-mm "\n        左余量 mm <5>: " 5.0))
      (setq tk:mR (tk:ask-mm "\n        右余量 mm <5>: " 5.0))
      (princ (strcat "\n      余量：上 " (tk:num tk:mT) " / 下 " (tk:num tk:mB)
                     " / 左 " (tk:num tk:mL) " / 右 " (tk:num tk:mR) " mm"))
      T)
    (progn
      (setq tk:mL 0.0 tk:mR 0.0 tk:mB 0.0 tk:mT 0.0)
      (princ "\n      不留余量，零件贴边")
      nil)))

(defun tk:confirm (u p / uw uh pw ph mode s kw txt v done)
  (setq uw (car u) uh (cadr u))
  (setq pw (car p) ph (cadr p))
  (setq mode "exact" done nil s nil)
  (setq txt (getstring (strcat "\n[5/5] 零件尺寸 <" (tk:num pw) " x " (tk:num ph)
                               "> [回车=用这个 / 或输入 宽,高 覆盖]: ")))
  (if (and txt (/= txt ""))
    (progn
      (setq v (tk:parse2 txt))
      (if (and v (car v) (cadr v) (> (car v) 0.0) (> (cadr v) 0.0))
        (progn
          (setq pw (car v) ph (cadr v))
          (princ (strcat "\n      已改为手输尺寸 " (tk:num pw) " x " (tk:num ph))))
        (princ "\n      尺寸格式不对，用原值。"))))
  (tk:ask-margins)
  (while (not done)
    (setq s (tk:calc uw uh pw ph mode))
    (princ "\n")
    (princ (strcat "\n      可用区    " (tk:num uw) " x " (tk:num uh)))
    (princ (strcat "\n      零件尺寸  " (tk:num pw) " x " (tk:num ph)))
    (princ (strcat "\n      余量 mm   上 " (tk:num tk:mT) " 下 " (tk:num tk:mB)
                   " 左 " (tk:num tk:mL) " 右 " (tk:num tk:mR)))
    (princ (strcat "\n      缩放系数  " (tk:num s) "   [" (if (= mode "std") "标准比例" "精确贴合") "]"))
    (initget "S M")
    (setq kw (getkword "\n      [回车=执行 / S=切标准比例 / M=重设余量 / Esc=取消]: "))
    (cond
      ((= kw "S") (setq mode (if (= mode "exact") "std" "exact")))
      ((= kw "M") (tk:ask-margins))
      (T (setq done T))))
  s)

;; ---------- 执行：缩放 + 按四边余量精确落位 ----------
(defun tk:apply (ss u p s / uw uh uctr pw ph pctr base uc2 uw2 uh2 plx pby tx ty delta old)
  (tk:erase-box)
  (setq uw (car u) uh (cadr u) uctr (caddr u))
  (setq pw (car p) ph (cadr p) pctr (caddr p))
  (if (or (< s 0.0001) (> s 10000.0))
    (progn (princ "\n      缩放系数异常，已取消，请检查尺寸。") nil)
    (progn
      (setq base uctr)
      (setq old (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.SCALE" ss "" (tk:w2u base) s)
      ;; 缩放后可绘图区中心 = base + s*(uctr - base)
      (setq uc2 (list (+ (car base) (* s (- (car uctr) (car base))))
                      (+ (cadr base) (* s (- (cadr uctr) (cadr base))))
                      0.0))
      (if pctr
        (progn
          (setq uw2 (* s uw) uh2 (* s uh))
          ;; 零件左下角
          (setq plx (- (car pctr) (/ pw 2.0)) pby (- (cadr pctr) (/ ph 2.0)))
          ;; 目标：可用区左边 = 零件左边 - 左余量；可用区下边 = 零件下边 - 下余量
          (setq tx (+ (- plx tk:mL) (/ uw2 2.0)))
          (setq ty (+ (- pby tk:mB) (/ uh2 2.0)))
          (setq delta (list (- tx (car uc2)) (- ty (cadr uc2)) 0.0))
          (command "_.MOVE" ss "" (tk:w2u (list 0.0 0.0 0.0)) (tk:w2u delta))))
      (setvar "CMDECHO" old)
      (princ (strcat "\n      完成：图框缩放 " (tk:num s) " 倍，已按余量套到零件上（零件未动）。可 U 撤销。"))
      T)))

;; ---------- 主命令 ----------
(defun tk:run (/ fr ss bb inner u p s)
  (defun *error* (msg)
    (vl-catch-all-apply 'tk:erase-box nil)
    ;; 中途 Esc / 出错时把 OSMODE、CMDECHO 还原，否则会永久停在 0
    (tk:restore-vars)
    (if msg
      (if (not (wcmatch (strcase (vl-princ-to-string msg))
                        "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*"))
        (princ (strcat "\n提示：" (vl-princ-to-string msg)))))
    (princ))
  (tk:save-vars)
  (princ "\n")
  (princ "\n  ================= 标题框缩放 =================")
  (princ "\n  [1] 选图框  [2] 点内边框  [3] 框选遮挡物")
  (princ "\n  [4] 点零件 4 个极值  [5] 设余量并执行")
  (princ "\n  零件不用动，图框会自己缩放并套到零件上。Esc 随时取消。")
  (princ "\n  =============================================")
  (if (and (setq fr (tk:frame))
           (setq ss (car fr) bb (cadr fr))
           (setq inner (tk:inner bb))
           (setq u (tk:usable inner))
           (setq p (tk:part))
           (tk:boxok u "可用区")
           (tk:boxok p "零件范围")
           (setq s (tk:confirm u p)))
    (tk:apply ss u p s)
    (progn (tk:erase-box) (princ "\n已取消，未做任何修改。")))
  (tk:restore-vars)
  (princ))

(defun c:TK () (tk:run))
(defun c:BTK () (tk:run))

(princ "\nTK 标题框缩放：选图框 → 点内边框 → 框选遮挡物 → 点零件极值 → 设余量，图框自动套上去。")
(princ)
