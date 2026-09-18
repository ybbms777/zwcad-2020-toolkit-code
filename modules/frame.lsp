;;; TK: 标题框缩放。GBK 编码。
;;; 图框自动缩放并对齐到零件上（零件不动，图框跟着走）。
;;;
;;; 前几版踩过的坑，别改回去：
;;;  1) vla-getboundingbox 返回 WCS 坐标，而 command 按 UCS 解释点，
;;;     所以所有传给 command 的点必须先 (trans p 0 1) 转成 UCS。
;;;  2) 用 grdraw 画的辅助框会被重绘冲掉，看不见；必须用真实实体（画完删掉）。
;;;  3) 标极值时用 nentselp 判断点没点在对象上：点在对象上就取该对象的包围盒边缘。
;;;     文字、标注这类对象点不准边缘（用户实测反馈），必须靠这一条。
;;;  4) 图框内还有标题栏、变更记录表等遮挡物，可绘图区要用「最大空矩形」算，
;;;     不能简单取内边框或遮挡物并集（并集往往是整个内框，等于没减）。
;;; 注意：不要用 let —— 标准 AutoLISP 没有这个宏，只用 setq + defun 局部变量。
(vl-load-com)

(setq tk:boxents nil)

;; ---------- WCS -> UCS（传给 command 的点都要过这一步） ----------
(defun tk:w2u (p) (trans p 0 1))

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

;; 每个对象各自的包围盒（遮挡物要单独算，不能并成一个）
(defun tk:allbox (ss / i e bb out)
  (setq i 0 out nil)
  (while (< i (sslength ss))
    (setq e (ssname ss i) i (1+ i))
    (setq bb (tk:ebbox e))
    (if bb (setq out (cons bb out))))
  out)

(defun tk:boxinfo (bb / a b)
  (if bb
    (progn
      (setq a (car bb) b (cadr bb))
      (list (- (car b) (car a))
            (- (cadr b) (cadr a))
            (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0) 0.0)))
    nil))

(defun tk:num (x) (rtos x 2 3))

;; ---------- 辅助显示：洋红实体（用完删除） ----------
(defun tk:mag (e / d)
  (if e
    (progn
      (setq d (entget e))
      (entmod (if (assoc 62 d) (subst (cons 62 6) (assoc 62 d) d) (append d (list (cons 62 6)))))
      (setq tk:boxents (cons e tk:boxents))))
  nil)

(defun tk:erase-box ()
  (if tk:boxents
    (foreach e tk:boxents (if (entget e) (entdel e))))
  (setq tk:boxents nil))

;; 画一个洋红矩形（不擦旧的）
(defun tk:rect (p1 p2 / old e)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.RECTANG" (tk:w2u p1) (tk:w2u p2))
  (setvar "CMDECHO" old)
  (setq e (entlast))
  (if (and e (= (cdr (assoc 0 (entget e))) "LWPOLYLINE")) (tk:mag e))
  nil)

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

;; ---------- 矩形工具 ----------
(defun tk:rtouch (a b)
  (and (<= (car a) (car (cadr b))) (>= (car (cadr a)) (car b))
       (<= (cadr a) (cadr (cadr b))) (>= (cadr (cadr a)) (cadr b))))

(defun tk:runion (a b)
  (list (list (min (car a) (car b)) (min (cadr a) (cadr b)) 0.0)
        (list (max (car (cadr a)) (car (cadr b))) (max (cadr (cadr a)) (cadr (cadr b))) 0.0)))

;; 把相接/相交的矩形合并，避免几十个小矩形把网格撑爆
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
    (if (and (< xa (car (cadr o))) (> xb (car (car o)))
             (< ya (cadr (cadr o))) (> yb (cadr (car o))))
      (setq hit T)))
  hit)

;; 在内框 R 里、避开 obs 的最大空矩形
(defun tk:maxrect (R obs / xs ys nx ny best bar i1 i2 j1 j2 i j xa xb ya yb ok ar)
  (setq xs (cons (car R) (cons (car (cadr R)) (mapcar '(lambda (o) (car o)) obs))))
  (setq xs (append xs (mapcar '(lambda (o) (car (cadr o))) obs)))
  (setq xs (tk:usort xs))
  (setq ys (cons (cadr R) (cons (cadr (cadr R)) (mapcar '(lambda (o) (cadr o)) obs))))
  (setq ys (append ys (mapcar '(lambda (o) (cadr (cadr o))) obs)))
  (setq ys (tk:usort ys))
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

(defun tk:calc (uw uh pw ph marg mode / s)
  (setq s (max (/ (* pw (+ 1.0 marg)) uw) (/ (* ph (+ 1.0 marg)) uh)))
  (if (= mode "std") (setq s (tk:snap s)))
  s)

;; ---------- [1/5] 选图框 ----------
(defun tk:frame (/ ss r i)
  (princ "\n[1/5] 框选【整个图框】（边框 + 标题栏一起），回车结束选择: ")
  (setq ss (ssget))
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
          (setq mn (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)) 0.0))
          (setq mx (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)) 0.0))
          (setq i (tk:boxinfo (list mn mx)))
          (princ (strcat "\n      内边框 = " (tk:num (car i)) " x " (tk:num (cadr i))))
          (list mn mx))))))

;; ---------- [3/5] 框选遮挡物，算最大可用矩形 ----------
(defun tk:usable (inner / ss obs u i)
  (princ "\n[3/5] 框选【图框内的遮挡物】（标题栏、变更记录表等），没有就回车跳过: ")
  (setq ss (ssget))
  (setq obs nil)
  (if ss
    (progn
      (setq obs (vl-catch-all-apply 'tk:allbox (list ss)))
      (if (vl-catch-all-error-p obs) (setq obs nil))
      (if obs (setq obs (tk:merge-rects obs)))))
  (if obs
    (progn
      (princ (strcat "\n      遮挡物合并成 " (itoa (length obs)) " 块"))
      (setq u (tk:maxrect inner obs)))
    (princ "\n      无遮挡物，可用区 = 内边框"))
  (if (null u) (setq u inner))
  (setq i (tk:boxinfo u))
  (princ (strcat "\n      可用区 = " (tk:num (car i)) " x " (tk:num (cadr i))))
  (tk:erase-box)
  (tk:rect (car u) (cadr u))
  (princ "\n      已用洋红框画出可用区（零件要装在这个框里）")
  u)

;; ---------- 取一个极值：点空白=用点坐标；点在对象上=取该对象包围盒的边缘 ----------
;; dir: "L" 最小X  "R" 最大X  "B" 最小Y  "T" 最大Y
(defun tk:ext (msg dir / p e bb v fromobj)
  (setq p (getpoint msg))
  (if (null p)
    nil
    (progn
      (setq e (car (nentselp p)) fromobj nil)
      (if e
        (progn
          (setq bb (tk:ebbox e))
          (if bb
            (progn
              (setq v (cond ((= dir "L") (car (car bb)))
                            ((= dir "R") (car (cadr bb)))
                            ((= dir "B") (cadr (car bb)))
                            (T (cadr (cadr bb)))))
              (setq fromobj T)))))
      (if (not fromobj)
        (setq v (if (or (= dir "L") (= dir "R")) (car p) (cadr p))))
      (princ (strcat "\n        取到 " (if fromobj "对象边缘" "点坐标") " = " (tk:num v)))
      v)))

;; ---------- [4/5] 零件范围：点 4 个极值 ----------
(defun tk:part-points (xL / xR yB yT mn mx i r)
  (setq r nil)
  (while (null r)
    (if (null xL)
      (setq xL (tk:ext "\n      点【零件最左】位置（点在对象上=取该对象最左边缘）: " "L")))
    (if (null xL)
      (setq r 'cancel)
      (progn
        (setq xR (tk:ext "\n      点【零件最右】位置（点在对象上=取该对象最右边缘）: " "R"))
        (if (null xR)
          (setq r 'cancel)
          (progn
            (setq yB (tk:ext "\n      点【零件最下】位置（点在对象上=取该对象最下边缘）: " "B"))
            (if (null yB)
              (setq r 'cancel)
              (progn
                (setq yT (tk:ext "\n      点【零件最上】位置（点在对象上=取该对象最上边缘）: " "T"))
                (if (null yT)
                  (setq r 'cancel)
                  (progn
                    (setq mn (list (min xL xR) (min yB yT) 0.0))
                    (setq mx (list (max xL xR) (max yB yT) 0.0))
                    (setq i (tk:boxinfo (list mn mx)))
                    (tk:rect mn mx)
                    (tk:mark-edge mn mx (/ (max (car i) (cadr i)) 80.0))
                    (princ "\n")
                    (princ "\n      -- 零件范围已用洋红框标出（4 个圆点在四条边中点上）--")
                    (princ (strcat "\n        最左 X = " (tk:num (car mn))))
                    (princ (strcat "\n        最右 X = " (tk:num (car mx))))
                    (princ (strcat "\n        最下 Y = " (tk:num (cadr mn))))
                    (princ (strcat "\n        最上 Y = " (tk:num (cadr mx))))
                    (princ (strcat "\n        宽 x 高 = " (tk:num (car i)) " x " (tk:num (cadr i))))
                    (initget "R")
                    (if (= (getkword "\n      贴住零件最外沿了吗？[回车=是 / R=重新点]: ") "R")
                      (progn (tk:erase-box) (setq xL nil) (princ "\n      重新点 4 个极值。"))
                      (setq r i)))))))))))
  (if (eq r 'cancel) nil r))

(defun tk:part-select (/ ss r info done)
  (setq info nil done nil)
  (while (not done)
    (princ "\n      框选零件（把最左/最右/最上/最下都框进去），回车结束: ")
    (setq ss (ssget))
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

(defun tk:part (/ xL)
  (setq xL (tk:ext "\n[4/5] 点【零件最左】位置（回车=改为框选零件自动量；点在对象上=取该对象最左边缘）: " "L"))
  (if (null xL) (tk:part-select) (tk:part-points xL)))

;; ---------- [5/5] 尺寸可覆盖 + 预览确认 ----------
(defun tk:confirm (u p / uw uh pw ph marg mode s kw txt v done)
  (setq uw (car u) uh (cadr u))
  (setq pw (car p) ph (cadr p))
  (setq marg 0.05 mode "exact" done nil s nil)
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
  (while (not done)
    (setq s (tk:calc uw uh pw ph marg mode))
    (princ "\n")
    (princ (strcat "\n      可用区    " (tk:num uw) " x " (tk:num uh)))
    (princ (strcat "\n      零件尺寸  " (tk:num pw) " x " (tk:num ph)))
    (princ (strcat "\n      留边余量  " (tk:num (* marg 100.0)) " %"))
    (princ (strcat "\n      缩放系数  " (tk:num s) "   [" (if (= mode "std") "标准比例" "精确贴合") "]"))
    (initget "S M")
    (setq kw (getkword "\n      [回车=执行 / S=切标准比例 / M=改余量 / Esc=取消]: "))
    (cond
      ((= kw "S") (setq mode (if (= mode "exact") "std" "exact")))
      ((= kw "M") (progn
                    (setq txt (getstring (strcat "\n      新余量百分比 <" (tk:num (* marg 100.0)) ">): ")))
                    (if (and txt (/= txt "")) (setq marg (/ (atof txt) 100.0)))))
      (T (setq done T))))
  s)

;; ---------- 执行：缩放 + 用位移量精确对齐到零件中心 ----------
(defun tk:apply (ss u p s / uw uh uctr pw ph pctr base uc2 delta old)
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
      (setq uc2 (list (+ (car base) (* s (- (car uctr) (car base))))
                      (+ (cadr base) (* s (- (cadr uctr) (cadr base))))
                      0.0))
      (if pctr
        (progn
          (setq delta (list (- (car pctr) (car uc2)) (- (cadr pctr) (cadr uc2)) 0.0))
          (command "_.MOVE" ss "" (tk:w2u (list 0.0 0.0 0.0)) (tk:w2u delta))))
      (setvar "CMDECHO" old)
      (princ (strcat "\n      完成：图框缩放 " (tk:num s) " 倍，已对齐到零件中心（零件未动）。可 U 撤销。"))
      T)))

;; ---------- 主命令 ----------
(defun tk:run (/ fr ss bb inner u p s)
  (defun *error* (msg)
    (tk:erase-box)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n提示：" msg)))
    (princ))
  (princ "\n")
  (princ "\n  ================= 标题框缩放 =================")
  (princ "\n  [1] 选图框  [2] 点内边框  [3] 框选遮挡物")
  (princ "\n  [4] 点零件 4 个极值  [5] 确认执行")
  (princ "\n  零件不用动，图框会自己缩放到零件正中。Esc 随时取消。")
  (princ "\n  =============================================")
  (if (and (setq fr (tk:frame))
           (setq ss (car fr) bb (cadr fr))
           (setq inner (tk:inner bb))
           (setq u (tk:usable inner))
           (setq p (tk:part))
           (setq s (tk:confirm u p)))
    (tk:apply ss u p s)
    (progn (tk:erase-box) (princ "\n已取消，未做任何修改。")))
  (princ))

(defun c:TK () (tk:run))
(defun c:BTK () (tk:run))

(princ "\nTK 标题框缩放：选图框 → 点内边框 → 框选遮挡物 → 点零件极值，图框自动缩放并对齐。")
(princ)
