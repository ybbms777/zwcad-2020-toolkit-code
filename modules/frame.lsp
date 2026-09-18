;;; TK: 标题框缩放。GBK 编码。
;;; 图框自动缩放并包裹到零件上（零件不动，图框跟着走）。
;;;
;;; 两个关键点（前两版踩过的坑，别改回去）：
;;;  1) vla-getboundingbox 返回 WCS 坐标，而 command 按 UCS 解释点，
;;;     所以所有传给 command 的点必须先 (trans p 0 1) 转成 UCS。
;;;  2) 用 grdraw 画的辅助框会被重绘冲掉，看不见；必须用真实实体（画完删掉）。
;;;  3) 标极值时用 nentselp 判断点没点在对象上：点在对象上就取该对象的包围盒边缘。
;;;     文字、标注这类对象点不准边缘（用户实测反馈），必须靠这一条。
(vl-load-com)

(setq tk:boxents nil)

;; ---------- WCS -> UCS（传给 command 的点都要过这一步） ----------
(defun tk:w2u (p) (trans p 0 1))

;; ---------- 取单个实体的包围盒 ----------
(defun tk:ebbox (e / o mn mx)
  (setq o (vlax-ename->vla-object e))
  (vla-getboundingbox o 'mn 'mx)
  (list (vlax-safearray->list mn) (vlax-safearray->list mx)))

;; ---------- 取选择集整体包围盒 ----------
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

;; ---------- 包围盒 -> (宽 高 中心点) ----------
(defun tk:boxinfo (bb / a b)
  (if bb
    (progn
      (setq a (car bb) b (cadr bb))
      (list (- (car b) (car a))
            (- (cadr b) (cadr a))
            (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0) 0.0)))
    nil))

(defun tk:num (x) (rtos x 2 3))

;; ---------- 辅助显示：洋红矩形 + 圆点标记（真实实体，用完删除） ----------
(defun tk:set-magenta (e)
  (if e
    (let ((d (entget e)))
      (if (assoc 62 d)
        (entmod (subst (cons 62 6) (assoc 62 d) d))
        (entmod (append d (list (cons 62 6)))))
      (setq tk:boxents (cons e tk:boxents)))
    nil))

(defun tk:erase-box ()
  (if tk:boxents
    (foreach e tk:boxents (if (entget e) (entdel e))))
  (setq tk:boxents nil))

(defun tk:draw-box (p1 p2 / old)
  (tk:erase-box)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.RECTANG" (tk:w2u p1) (tk:w2u p2))
  (setvar "CMDECHO" old)
  (tk:set-magenta (entlast)))

(defun tk:mark (pts r / old)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (foreach p pts
    (command "_.CIRCLE" (tk:w2u p) r)
    (tk:set-magenta (entlast)))
  (setvar "CMDECHO" old))

;; 在包围盒四条边的中点画圆点（标出 4 个极值位置）
(defun tk:mark-edge (mn mx r / x1 y1 x2 y2 xm ym)
  (setq x1 (car mn) y1 (cadr mn) x2 (car mx) y2 (cadr mx))
  (setq xm (/ (+ x1 x2) 2.0) ym (/ (+ y1 y2) 2.0))
  (tk:mark (list (list x1 ym 0.0) (list x2 ym 0.0) (list xm y1 0.0) (list xm y2 0.0)) r))

;; ---------- 取一个极值 ----------
;; 点空白处 -> 用点的坐标；
;; 点在对象上 -> 用该对象包围盒在指定方向的边缘（文字、标注这类点不准的对象就靠这个）
;; dir: "L" 最小X  "R" 最大X  "B" 最小Y  "T" 最大Y
(defun tk:ext (msg dir / p e bb v fromobj)
  (setq p (getpoint msg))
  (if (null p)
    nil
    (progn
      (setq e (car (nentselp p)))
      (setq fromobj nil)
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
      (princ (strcat "\n        取到 " (if fromobj "对象边缘" "点坐标")
                     " = " (tk:num v)))
      v)))

;; ---------- 解析 "105.95,117.9" / "105.95x117.9" / "105.95 117.9" ----------
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

;; ---------- 标准比例系列 ----------
(setq tk:std '(0.01 0.02 0.05 0.1 0.2 0.25 0.5 0.75 1.0 1.5 2.0 2.5 3.0 4.0 5.0 10.0))
(defun tk:snap (f / r)
  (setq r nil)
  (foreach v tk:std (if (and (null r) (>= v f)) (setq r v)))
  (if r r f))

(defun tk:calc (uw uh pw ph marg mode / s)
  (setq s (max (/ (* pw (+ 1.0 marg)) uw) (/ (* ph (+ 1.0 marg)) uh)))
  (if (= mode "std") (setq s (tk:snap s)))
  s)

;; ---------- [1/4] 选图框 ----------
(defun tk:frame (/ ss r i)
  (princ "\n[1/4] 框选【整个图框】（边框 + 标题栏一起），回车结束选择: ")
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

;; ---------- [2/4] 可绘图区：直接点第一个角点即可，回车才用外框 ----------
(defun tk:area (bb / p1 p2 mn mx i)
  (setq p1 (getpoint "\n[2/4] 点【图框内边框】第一个角点（回车=改用图框外框，快）: "))
  (if (null p1)
    (progn
      (setq i (tk:boxinfo bb))
      (princ (strcat "\n      可绘图区 = 图框外框 " (tk:num (car i)) " x " (tk:num (cadr i))))
      i)
    (progn
      (setq p2 (getcorner p1 "\n      点对角: "))
      (if (null p2)
        nil
        (progn
          (setq mn (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)) 0.0))
          (setq mx (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)) 0.0))
          (setq i (tk:boxinfo (list mn mx)))
          (princ (strcat "\n      可绘图区 = " (tk:num (car i)) " x " (tk:num (cadr i))))
          i)))))

;; ---------- [3/4] 零件范围：点 4 个极值（回车改为框选自动量） ----------
;; xL 可由调用方先给（用户在第 3 步提示处直接点的那一下就是「最左」）
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
                    (tk:draw-box mn mx)
                    (tk:mark-edge mn mx (/ (max (car i) (cadr i)) 80.0))
                    (princ "\n")
                    (princ "\n      -- 已用洋红框标出范围，4 个圆点在四条边的中点上 --")
                    (princ (strcat "\n        最左 X = " (tk:num (car mn))))
                    (princ (strcat "\n        最右 X = " (tk:num (car mx))))
                    (princ (strcat "\n        最下 Y = " (tk:num (cadr mn))))
                    (princ (strcat "\n        最上 Y = " (tk:num (cadr mx))))
                    (princ (strcat "\n        宽 x 高 = " (tk:num (car i)) " x " (tk:num (cadr i))))
                    (initget "R")
                    (if (= (getkword "\n      洋红框贴住零件最外沿了吗？[回车=是 / R=重新点]: ") "R")
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
            (tk:draw-box (car r) (cadr r))
            (princ "\n")
            (princ "\n      -- 已用洋红框标出量到的范围，请核对 --")
            (princ (strcat "\n        最左 X = " (tk:num (car (car r)))))
            (princ (strcat "\n        最右 X = " (tk:num (car (cadr r)))))
            (princ (strcat "\n        最下 Y = " (tk:num (cadr (car r)))))
            (princ (strcat "\n        最上 Y = " (tk:num (cadr (cadr r)))))
            (princ (strcat "\n        宽 x 高 = " (tk:num (car info)) " x " (tk:num (cadr info))))
            (initget "R")
            (if (= (getkword "\n      洋红框贴住零件最外沿了吗？[回车=是 / R=重新框选]: ") "R")
              (progn (tk:erase-box) (setq info nil) (princ "\n      重新框选。"))
              (setq done T)))))))
  info)

(defun tk:part (/ xL)
  (setq xL (tk:ext "\n[3/4] 点【零件最左】位置（回车=改为框选零件自动量；点在对象上=取该对象最左边缘）: " "L"))
  (if (null xL) (tk:part-select) (tk:part-points xL)))

;; ---------- [4/4] 尺寸可覆盖 + 预览确认 ----------
(defun tk:confirm (u p / uw uh pw ph marg mode s kw txt v done)
  (setq uw (car u) uh (cadr u))
  (setq pw (car p) ph (cadr p))
  (setq marg 0.05 mode "exact" done nil s nil)
  (setq txt (getstring (strcat "\n[4/4] 零件尺寸 <" (tk:num pw) " x " (tk:num ph)
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
    (princ (strcat "\n      可绘图区  " (tk:num uw) " x " (tk:num uh)))
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
;; 不用 MOVE 的「基点->目标点」语义，改成算好位移量直接搬，
;; 免得受 SCALE 基点、UCS/WCS 差异影响。
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
      ;; 缩放后可绘图区中心的位置 = base + s*(uctr - base)
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
(defun tk:run (/ fr ss bb u p s)
  (defun *error* (msg)
    (tk:erase-box)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n提示：" msg)))
    (princ))
  (princ "\n")
  (princ "\n  ================ 标题框缩放 ================")
  (princ "\n  [1] 选图框  [2] 定可绘图区  [3] 标零件范围  [4] 确认")
  (princ "\n  [3] 推荐直接点 4 个极值点：最左 / 最右 / 最下 / 最上")
  (princ "\n  零件不用动，图框会自己缩放到零件正中。Esc 随时取消。")
  (princ "\n  ===========================================")
  (if (and (setq fr (tk:frame))
           (setq ss (car fr) bb (cadr fr))
           (setq u (tk:area bb))
           (setq p (tk:part))
           (setq s (tk:confirm u p)))
    (tk:apply ss u p s)
    (progn (tk:erase-box) (princ "\n已取消，未做任何修改。")))
  (princ))

(defun c:TK () (tk:run))
(defun c:BTK () (tk:run))

(princ "\nTK 标题框缩放：点 4 个极值点标出零件范围，图框自动缩放并对齐到零件中心。")
(princ)
