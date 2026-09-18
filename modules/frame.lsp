;;; TK: 标题框缩放。GBK 编码。
;;; 图框自动缩放并包裹到零件上（零件不动，图框跟着走）。
;;; 量完零件会用洋红虚线框标出范围 + 打印左右上下四个坐标，供人工确认。
(vl-load-com)

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

;; ---------- 洋红虚线框：grdraw 画，不产生实体、不污染撤销栈 ----------
(defun tk:draw-box (p1 p2 / x1 y1 x2 y2)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (grdraw (list x1 y1) (list x2 y1) 6)
  (grdraw (list x2 y1) (list x2 y2) 6)
  (grdraw (list x2 y2) (list x1 y2) 6)
  (grdraw (list x1 y2) (list x1 y1) 6))

(defun tk:erase-box () (redraw))

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

;; ---------- [2/4] 可绘图区 ----------
(defun tk:area (bb / kw p1 p2 mn mx i)
  (initget "I")
  (setq kw (getkword "\n[2/4] 图框可绘图区 [I=点内边框两个角点（准，推荐）/ 回车=直接用图框外框（快）]: "))
  (if (= kw "I")
    (progn
      (setq p1 (getpoint "\n      点【内边框】第一个角点: "))
      (if (null p1)
        nil
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
    (progn
      (setq i (tk:boxinfo bb))
      (princ (strcat "\n      可绘图区 = 图框外框 " (tk:num (car i)) " x " (tk:num (cadr i))))
      i)))

;; ---------- [3/4] 选零件（画框 + 报数 + 确认，不满意可重选） ----------
(defun tk:part (/ ss r info kw done)
  (setq info nil done nil)
  (while (not done)
    (princ "\n[3/4] 框选【零件】（把最左/最右/最上/最下都框进去），回车结束: ")
    (setq ss (ssget))
    (if (null ss)
      (setq done T)
      (progn
        (setq r (vl-catch-all-apply 'tk:ssbox (list ss)))
        (if (or (vl-catch-all-error-p r) (null r))
          (princ "\n      算不出零件范围，请重新框选。")
          (progn
            (setq info (tk:boxinfo r))
            (tk:draw-box (car r) (cadr r))
            (princ "\n")
            (princ "\n      -- 已用洋红虚线框标出量到的范围，请对照图纸核对 --")
            (princ (strcat "\n        最左 X = " (tk:num (car (car r)))))
            (princ (strcat "\n        最右 X = " (tk:num (car (cadr r)))))
            (princ (strcat "\n        最下 Y = " (tk:num (cadr (car r)))))
            (princ (strcat "\n        最上 Y = " (tk:num (cadr (cadr r)))))
            (princ (strcat "\n        宽 x 高 = " (tk:num (car info)) " x " (tk:num (cadr info))))
            (initget "R")
            (setq kw (getkword "\n      洋红框贴住零件最外沿了吗？[回车=是 / R=重新框选]: "))
            (if (= kw "R")
              (progn (tk:erase-box) (setq info nil) (princ "\n      重新框选零件。"))
              (setq done T)))))))
  info)

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
        (princ "\n      尺寸格式不对，用自动量到的值。"))))
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
                    (setq txt (getstring (strcat "\n      新余量百分比 <" (tk:num (* marg 100.0)) ">: ")))
                    (if (and txt (/= txt "")) (setq marg (/ (atof txt) 100.0)))))
      (T (setq done T))))
  s)

;; ---------- 执行：缩放 + 自动包裹（移到零件正中） ----------
(defun tk:apply (ss u p s / uctr pctr old)
  (tk:erase-box)
  (setq uctr (caddr u) pctr (caddr p))
  (if (or (< s 0.0001) (> s 10000.0))
    (progn (princ "\n      缩放系数异常，已取消，请检查尺寸。") nil)
    (progn
      (setq old (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.SCALE" ss "" uctr s)
      (if pctr (command "_.MOVE" ss "" uctr pctr))
      (setvar "CMDECHO" old)
      (princ (strcat "\n      完成：图框缩放 " (tk:num s) " 倍，已自动包裹到零件上（零件未动）。可 U 撤销。"))
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
  (princ "\n  把图框缩放并自动包裹到零件上。")
  (princ "\n  零件不用动，图框会自己缩放到零件正中。")
  (princ "\n  共 4 步，每步都有提示；中途按 Esc 可随时取消。")
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

(princ "\nTK 标题框缩放：图框自动缩放并包裹到零件上（可手输尺寸或框选零件自动量）。")
(princ)
