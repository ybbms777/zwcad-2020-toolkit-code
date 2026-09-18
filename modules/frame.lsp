;;; TK: 标题框智能缩放。GBK 编码。
;;; 把图框缩放并移动到刚好包住零件（零件不动，图框跟着走）。
;;; 尺寸可手输（宽,高）也可框选零件自动量；缩放可选精确贴合或标准比例。
(vl-load-com)

;; ---------- 取单个实体的包围盒，失败返回 nil ----------
(defun tk:ebbox (e / o mn mx)
  (setq o (vlax-ename->vla-object e))
  (vla-getboundingbox o 'mn 'mx)
  (list (vlax-safearray->list mn) (vlax-safearray->list mx)))

;; ---------- 取选择集的整体包围盒 ----------
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

;; ---------- 数字格式化 ----------
(defun tk:num (x) (rtos x 2 3))

;; ---------- 解析 "105.95,117.9" / "105.95x117.9" / "105.95 117.9" ----------
(defun tk:parse2 (s / i c buf res)
  (setq res nil buf "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (member c '("," "，" " " "x" "X" "*" "×"))
      (progn
        (if (/= buf "") (setq res (cons (atof buf) res)))
        (setq buf ""))
      (setq buf (strcat buf c)))
    (setq i (1+ i)))
  (if (/= buf "") (setq res (cons (atof buf) res)))
  (reverse res))

;; ---------- 标准比例系列（图框缩放系数）----------
(setq tk:std '(0.01 0.02 0.05 0.1 0.2 0.25 0.5 0.75 1.0 1.5 2.0 2.5 3.0 4.0 5.0 10.0))
(defun tk:snap (f / r)
  (setq r nil)
  (foreach v tk:std (if (and (null r) (>= v f)) (setq r v)))
  (if r r f))

;; ---------- 算缩放系数 ----------
(defun tk:calc (uw uh pw ph marg mode / s)
  (setq s (max (/ (* pw (+ 1.0 marg)) uw) (/ (* ph (+ 1.0 marg)) uh)))
  (if (= mode "std") (setq s (tk:snap s)))
  s)

;; ---------- 步骤 1：选图框 ----------
(defun tk:frame (/ ss bb r)
  (princ "\n选择图框对象（框选整个图框，回车结束选择）: ")
  (setq ss (ssget))
  (if (null ss)
    (progn (princ "\n未选择对象。") nil)
    (progn
      (setq r (vl-catch-all-apply 'tk:ssbox (list ss)))
      (if (or (vl-catch-all-error-p r) (null r))
        (progn (princ "\n无法计算图框包围盒，请确认选中的是图框本身。") nil)
        (list ss r)))))

;; ---------- 步骤 2：确定可绘图区 ----------
(defun tk:area (bb / kw p1 p2 mn mx)
  (initget "I")
  (setq kw (getkword "\n图框可绘图区 [I=手动点内边框两点 / 回车=直接用图框外框]: "))
  (if (= kw "I")
    (progn
      (setq p1 (getpoint "\n点图框内边框第一个角点: "))
      (if (null p1)
        nil
        (progn
          (setq p2 (getcorner p1 "\n点对角: "))
          (if (null p2)
            nil
            (progn
              (setq mn (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)) 0.0))
              (setq mx (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)) 0.0))
              (tk:boxinfo (list mn mx)))))))
    (tk:boxinfo bb)))

;; ---------- 步骤 3：零件尺寸 ----------
(defun tk:part (/ txt v ss r bb info ctr)
  (setq txt (getstring "\n零件尺寸 [回车=框选零件自动量 / 或输入 宽,高 如 105.95,117.9]: "))
  (if (and txt (/= txt ""))
    (progn
      (setq v (tk:parse2 txt))
      (if (and v (car v) (cadr v) (> (car v) 0.0) (> (cadr v) 0.0))
        (progn
          (setq ctr (getpoint "\n指定零件中心点 [回车=只缩放不移动图框]: "))
          (list (car v) (cadr v) ctr))
        (progn (princ "\n尺寸格式不对，请按 宽,高 输入，例如 105.95,117.9") nil)))
    (progn
      (princ "\n框选零件（把零件全部框进去）: ")
      (setq ss (ssget))
      (if (null ss)
        (progn (princ "\n未选择零件。") nil)
        (progn
          (setq r (vl-catch-all-apply 'tk:ssbox (list ss)))
          (if (or (vl-catch-all-error-p r) (null r))
            (progn (princ "\n无法计算零件包围盒。") nil)
            (progn
              (setq info (tk:boxinfo r))
              (princ (strcat "\n自动量得零件尺寸 " (tk:num (car info)) " x " (tk:num (cadr info))))
              info)))))))

;; ---------- 步骤 4：预览 + 确认 ----------
(defun tk:confirm (u p / uw uh uctr pw ph marg mode s kw txt done)
  (setq uw (car u) uh (cadr u) uctr (caddr u))
  (setq pw (car p) ph (cadr p))
  (setq marg 0.05 mode "exact" done nil s nil)
  (while (not done)
    (setq s (tk:calc uw uh pw ph marg mode))
    (princ "\n")
    (princ (strcat "\n  图框可绘图区  " (tk:num uw) " x " (tk:num uh)))
    (princ (strcat "\n  零件尺寸      " (tk:num pw) " x " (tk:num ph)))
    (princ (strcat "\n  留边余量      " (tk:num (* marg 100.0)) " %"))
    (princ (strcat "\n  缩放系数      " (tk:num s)
                   "   [" (if (= mode "std") "标准比例" "精确贴合") "]"))
    (initget "S M")
    (setq kw (getkword "\n[回车=执行 / S=切标准比例 / M=改余量 / Esc=取消]: "))
    (cond
      ((= kw "S") (setq mode (if (= mode "exact") "std" "exact")))
      ((= kw "M") (progn
                    (setq txt (getstring (strcat "\n新余量百分比 <" (tk:num (* marg 100.0)) ">: ")))
                    (if (and txt (/= txt "")) (setq marg (/ (atof txt) 100.0)))))
      (T (setq done T))))
  s)

;; ---------- 步骤 5：执行 ----------
(defun tk:apply (ss u p s / uctr pctr old)
  (setq uctr (caddr u) pctr (caddr p))
  (if (or (< s 0.0001) (> s 10000.0))
    (progn (princ "\n缩放系数异常，已取消，请检查输入的尺寸。") nil)
    (progn
      (setq old (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.SCALE" ss "" uctr s)
      (if pctr (command "_.MOVE" ss "" uctr pctr))
      (setvar "CMDECHO" old)
      (princ (strcat "\n完成：图框已缩放 " (tk:num s) " 倍"
                     (if pctr "，并移到零件正中。" "。位置未动。")
                     " 可撤销（U）。"))
      T)))

;; ---------- 主命令 ----------
(defun tk:run (/ fr ss bb u p s)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n提示：" msg)))
    (princ))
  (princ "\n标题框智能缩放：把图框缩放成刚好包住零件（零件不动，图框跟着走）。")
  (if (and (setq fr (tk:frame))
           (setq ss (car fr) bb (cadr fr))
           (setq u (tk:area bb))
           (setq p (tk:part))
           (setq s (tk:confirm u p)))
    (tk:apply ss u p s)
    (princ "\n已取消，未做任何修改。"))
  (princ))

(defun c:TK () (tk:run))
(defun c:BTK () (tk:run))

(princ "\nTK 标题框智能缩放：图框按零件尺寸缩放并居中（可手输尺寸或框选零件自动量）。")
(princ)
