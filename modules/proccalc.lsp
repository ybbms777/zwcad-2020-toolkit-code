;;; ============================================================
;;;  proccalc.lsp  工艺计算器    中望 CAD 2020
;;; ------------------------------------------------------------
;;;  命令:  JSQ    面板计算器（DCL 面板，输入即算）
;;;         JSQP   命令行计算（面板不可用时自动回退 / 脚本可调）
;;;         JSQN   纯计算测试入口（把结果写到 %TEMP%\zwk-proccalc-test.txt）
;;;  口径来源: Obsidian《SW/齿数计算公式》《SW/齿片台阶与角度计算公式》
;;;         · 齿根距 L = 第一片左外边到最后一片右外边 = (N-1)*P+t
;;;         · 标称片数 N0 = L/P 取整；排齿 N 取 (N-1)*P+t 落 L 公差带者
;;;         · 铲切台阶高 Hc = 实配齿高 * t / P
;;;         · 角度: 比较 片厚*2 与 步距 P（= 2t → 19~20°；> → 22~23°；< → 17~18°）
;;;         · 末片斜面: 水平 = Hc / tanθ，实长 = Hc / sinθ
;;;         · 毛坯: 底板厚 + Hc + 1.5（可大不可小）；下料加放 = 斜面水平 + 0.2 让刀
;;;  文件编码 GBK(936) 行尾 LF —— 改动必须用脚本读写
;;; ============================================================

;; ---------------- 默认值（会话内记忆） ----------------
(if (not zwkfc:t)    (setq zwkfc:t 0.05))
(if (not zwkfc:g)    (setq zwkfc:g 0.08))
(if (not zwkfc:p)    (setq zwkfc:p 0.0))
(if (not zwkfc:l)    (setq zwkfc:l 0.0))
(if (not zwkfc:w) (setq zwkfc:w 0.0))
(if (not zwkfc:tolerances) (setq zwkfc:tolerances nil))
(if (not zwkfc:h)    (setq zwkfc:h 0.8))
(if (not zwkfc:hb)   (setq zwkfc:hb 1.0))
(if (not zwkfc:hr)   (setq zwkfc:hr 0.15))
(if (not zwkfc:ang)  (setq zwkfc:ang 0.0))
(if (not zwkfc:incl) (setq zwkfc:incl 1))
(if (not zwkfc:res)  (setq zwkfc:res nil))
(if (not zwkfc:lines) (setq zwkfc:lines nil))

(defun zwkfc:reset ()
  (setq zwkfc:t 0.05 zwkfc:g 0.08 zwkfc:p 0.0 zwkfc:l 0.0 zwkfc:tol 0.1
        zwkfc:h 0.8 zwkfc:hb 1.0 zwkfc:hr 0.15 zwkfc:ang 0.0 zwkfc:incl 1 zwkfc:w 0.0 zwkfc:tolerances nil))

;; ---------------- 小工具 ----------------
;; 定点小数格式化：不依赖 DIMZIN（自己裁尾零），18.0 -> "18"，1.500 -> "1.5"
(defun zwkfc:f (v n / s)
  (if (null v) (setq v 0.0))
  (setq s (rtos v 2 n))
  (if (wcmatch s "*.*")
    (progn
      (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) "0"))
        (setq s (substr s 1 (1- (strlen s)))))
      (if (and (> (strlen s) 0) (= (substr s (strlen s) 1) "."))
        (setq s (substr s 1 (1- (strlen s)))))))
  s)

(defun zwkfc:rad (d) (* d (/ pi 180.0)))
(defun zwkfc:tan (d) (/ (sin (zwkfc:rad d)) (cos (zwkfc:rad d))))
(defun zwkfc:sin (d) (sin (zwkfc:rad d)))

;; 取有效步距（填 0 视为自动 = 片厚 + 间距）
(defun zwkfc:ep (t g p) (if (or (null p) (<= p 0.0)) (+ t g) p))

;; 字符串 -> 数值：逐字符校验（atof "abc" 会静默变 0.0），空串/非法则保留原值
(defun zwkfc:nv (s cur / ok i c dot digits)
  (if (or (null s) (= s ""))
    cur
    (progn
      (setq ok T i 1 dot nil digits 0)
      (while (and ok (<= i (strlen s)))
        (setq c (substr s i 1))
        (cond ((wcmatch c "#") (setq digits (1+ digits)))
              ((and (= c ".") (not dot)) (setq dot T))
              ((and (or (= c "-") (= c "+")) (= i 1)))
              (T (setq ok nil)))
        (setq i (1+ i)))
      (if (and ok (> digits 0)) (atof s) cur))))

;; ---------------- 角度推荐 ----------------
;; 返回 (区间字符串 建议角度 说明)
(defun zwkfc:angle (t p / d2 ad band rec txt)
  (setq d2 (- (* 2.0 t) p) ad (abs d2))
  (cond
    ((<= ad 0.005) (setq band "19~20" rec 19.5))
    ((> d2 0.0)    (setq band "22~23" rec (if (<= ad 0.035) 22.0 23.0)))
    (T             (setq band "17~18" rec (if (<= ad 0.035) 18.0 17.0))))
  (setq txt (strcat "角度: 片厚×2 = " (zwkfc:f (* 2.0 t) 3) "  步距 P = " (zwkfc:f p 3)
                    "  差 " (zwkfc:f d2 3) "  → 区间 " band "°，建议 "
                    (zwkfc:f rec 1) "°  (差得少取靠近标准端)"))
  (if (> ad 0.0501)
    (setq txt (strcat txt "  (注意: 差值超 0.05，已超该经验区间)")))
  (list band rec txt))

;; ---------------- 核心计算 ----------------
;; t 片厚 g 间距 p 步距 L 齿根距 tol 公差 h 齿高 hb 底板厚 hr 台阶补偿 ang 角度(0=自动) incl 齿高含台阶
;; 返回 alist: ((N0 . "...") (N . "...") ... (WARN . "..."))
(defun zwkfc:calc (t g p l tol h hb hr ang incl / pe n0 cands n d best bestn warn hfnet realh hc ab abr run slant blank lo hi checked)
  (setq checked (not (null tol)))
  (if (listp tol)
    (setq lo (if tol (car tol) 0.0) hi (if tol (cadr tol) 0.0))
    (setq lo (- tol) hi tol))
  (setq pe (zwkfc:ep t g p))
  (setq warn
    (cond ((<= t 0.0) "片厚必须大于 0")
          ((< g 0.0) "间距不能为负数")
          ((< p 0.0) "步距不能为负数；自动计算请填 0")
          ((<= pe 0.0) "步距必须大于 0")
          ((< pe t) "步距不能小于片厚")
          ((<= l 0.0) "请填写大于 0 的齿根距")
          ((or (> lo hi) (and (numberp tol) (< tol 0.0))) "下公差不能大于上公差")
          ((<= h 0.0) "齿高必须大于 0")
          ((< hb 0.0) "底板厚不能为负数")
          ((< hr 0.0) "台阶补偿不能为负数")
          ((and (= incl 1) (<= h hr)) "含台阶齿高必须大于台阶补偿")
          ((or (< ang 0.0) (>= ang 90.0)) "角度须大于 0 且小于 90 度；自动推荐请填 0")
          (T "")))
  (if (/= warn "")
    (list (cons 'VALID nil) (cons 'WARN warn))
    (progn
  (if (or (null t) (<= t 0.0))
    (setq warn "片厚必须大于 0"))
  (if (<= pe 0.0)
    (setq warn "步距必须大于 0"))
  (if (or (null l) (<= l 0.0))
    (setq warn "还没填齿根距 L"))
  ;; ---- 片数 ----
  (setq n0 0 bestn 0 best 0.0)
  (if (and (> pe 0.0) (> l 0.0))
    (progn
      (setq n0 (fix (+ (/ l pe) 0.5)))
      (setq cands (list (- n0 1) n0 (+ n0 1) (+ n0 2)))
      (setq bestn nil best nil)
      ;; 先挑落公差带内、且偏差最小的
      (foreach n cands
        (if (> n 1)
          (progn
            (setq d (- (+ (* (1- n) pe) t) l))
            (if (and (or (not checked) (and (>= d (- lo 0.000000001)) (<= d (+ hi 0.000000001)))) (or (null best) (< (abs d) (abs best))))
              (setq best d bestn n)))))
      ;; 都不在带里 → 取最接近的并告警
      (if (null bestn)
        (progn
          (foreach n cands
            (if (> n 1)
              (progn
                (setq d (- (+ (* (1- n) pe) t) l))
                (if (or (null best) (< (abs d) (abs best))) (setq best d bestn n)))))
          (setq warn (strcat "排 " (itoa bestn) " 片后的齿根距超出公差，请确认图纸。"))))))
  ;; ---- 实配齿高 / 铲切台阶高 ----
  (if (= incl 1)
    (progn (setq hfnet (- h hr)) (setq realh h))          ;; 图纸齿高包含工艺台阶
    (progn (setq hfnet h)        (setq realh (+ h hr))))  ;; 图纸齿高是净齿高
  (setq hc (if (> pe 0.0) (/ (* realh t) pe) 0.0))
  ;; ---- 角度 / 斜面 ----
  (if (> ang 0.0)
    (setq abr (list "手动" ang (strcat "角度: 手动指定 " (zwkfc:f ang 1) "°")))
    (setq abr (zwkfc:angle t pe)))
  (setq run (if (> (cos (zwkfc:rad (cadr abr))) 0.0) (/ hc (zwkfc:tan (cadr abr))) 0.0))
  (setq slant (if (> (cos (zwkfc:rad (cadr abr))) 0.0) (/ hc (zwkfc:sin (cadr abr))) 0.0))
  (setq blank (+ hb hc 1.5))
  (list
    (cons 'PE (zwkfc:f pe 4))
    (cons 'N0 (itoa n0))
    (cons 'N (itoa bestn))
    (cons 'ROOT (zwkfc:f (+ (* (1- bestn) pe) t) 3))
    (cons 'TOTAL (zwkfc:f (* (1- bestn) pe) 3))
    (cons 'DS (zwkfc:f best 3))
    (cons 'BW (zwkfc:f (+ (* (1- bestn) pe) t) 3))
    (cons 'PIN (if (> bestn 1) (zwkfc:f (/ (- l t) (1- bestn)) 6) "0"))
    (cons 'HNET (zwkfc:f hfnet 3))
    (cons 'REALH (zwkfc:f realh 3))
    (cons 'HC (zwkfc:f hc 4))
    (cons 'BAND (car abr))
    (cons 'ANG (zwkfc:f (cadr abr) 1))
    (cons 'ANGTXT (caddr abr))
    (cons 'RUN (zwkfc:f run 3))
    (cons 'SLANT (zwkfc:f slant 3))
    (cons 'BLANK (zwkfc:f blank 2))
    (cons 'REC (zwkfc:f (+ run 0.2) 2))
    (cons 'WARN warn)))))

;; ---------------- 结果文本 ----------------
(defun zwkfc:format-lines (r t g p l tol h hb hr / s)
  (if (assoc 'VALID r)
    (list (strcat "请检查输入：" (cdr (assoc 'WARN r))))
    (progn
      (setq s (list
        "片数与排齿"
        (strcat "图纸标注片数：" (cdr (assoc 'N0 r)) " ±1 片")
        (strcat "实际排齿片数：" (cdr (assoc 'N r)) " 片")
        (strcat "排齿后齿根距：" (cdr (assoc 'ROOT r)) " mm")
        (strcat "与图纸尺寸相差：" (cdr (assoc 'DS r)) " mm")
        (strcat "齿宽：" (zwkfc:f zwkfc:w 4) " mm")
        ""
        "CAD 阵列填写"
        (strcat "列数：" (cdr (assoc 'N r)))
        (strcat "介于（相邻齿的步距）：" (cdr (assoc 'PE r)) " mm")
        (strcat "总计（阵列移动距离）：" (cdr (assoc 'TOTAL r)) " mm")
        (strcat "若要保持图纸齿根距，介于填：" (cdr (assoc 'PIN r)) " mm")
        ""
        "加工尺寸"
        (strcat "成品净齿高：" (cdr (assoc 'HNET r)) " mm")
        (strcat "铲齿计算用齿高：" (cdr (assoc 'REALH r)) " mm")
        (strcat "铲切台阶高度：" (cdr (assoc 'HC r)) " mm")
        (strcat "铲切角度：" (cdr (assoc 'ANG r)) "°"
          (if (equal (cdr (assoc 'BAND r)) "手动") "（手动设置）" "（推荐）"))
        (strcat "末片斜面水平长度：" (cdr (assoc 'RUN r)) " mm")
        (strcat "末片斜面实际长度：" (cdr (assoc 'SLANT r)) " mm")
        (strcat "建议毛坯厚度：" (cdr (assoc 'BLANK r)) " mm（已留 1.5 mm 余量）")
        (strcat "建议下料长度加放：" (cdr (assoc 'REC r)) " mm")
        "741 件的下料加放，请按对应工艺确认。"))
      (if (> (abs (- (* 2.0 t) (zwkfc:ep t g p))) 0.0501)
        (setq s (append s (list "片厚与步距超出常用角度经验范围，请确认角度。"))))
      (if (/= "" (cdr (assoc 'WARN r)))
        (setq s (append s (list (strcat "排齿提醒：" (cdr (assoc 'WARN r)))))))
      s)))

(setq zwkfc:fields '(("t" "片厚" zwkfc:t) ("g" "间距" zwkfc:g)
  ("p" "步距" zwkfc:p) ("l" "齿根距" zwkfc:l) ("w" "齿宽" zwkfc:w)
  ("h" "齿高" zwkfc:h) ("hb" "底板厚" zwkfc:hb) ("hr" "齿根台阶补偿" zwkfc:hr)))

(defun zwkfc:tol-get (key / v)
  (setq v (cdr (assoc key zwkfc:tolerances)))
  (if v v '(0 0.0 0.0)))
(defun zwkfc:tol-put (key value / old)
  (setq old (assoc key zwkfc:tolerances))
  (if old (setq zwkfc:tolerances (subst (cons key value) old zwkfc:tolerances))
    (setq zwkfc:tolerances (cons (cons key value) zwkfc:tolerances))))
(defun zwkfc:run ( / r v problem field)
  (setq v (zwkfc:tol-get "l") problem nil)
  (foreach field zwkfc:fields
    (if (and (= (car (zwkfc:tol-get (car field))) 1)
      (> (cadr (zwkfc:tol-get (car field))) (caddr (zwkfc:tol-get (car field)))))
      (setq problem (strcat (cadr field) "的下公差不能大于上公差"))))
  (setq r (if problem (list (cons 'VALID nil) (cons 'WARN problem))
    (zwkfc:calc zwkfc:t zwkfc:g zwkfc:p zwkfc:l
      (if (= (car v) 1) (list (cadr v) (caddr v)) nil)
      zwkfc:h zwkfc:hb zwkfc:hr zwkfc:ang zwkfc:incl)))
  (setq zwkfc:res r)
  (setq zwkfc:lines (zwkfc:format-lines r zwkfc:t zwkfc:g zwkfc:p zwkfc:l nil zwkfc:h zwkfc:hb zwkfc:hr))
  (if (not (assoc 'VALID r))
    (setq zwkfc:lines (append zwkfc:lines
      (list (if (= (car v) 1)
        (strcat "齿根距允许范围：" (zwkfc:f (+ zwkfc:l (cadr v)) 4)
          " 至 " (zwkfc:f (+ zwkfc:l (caddr v)) 4) " mm")
        "未设置齿根距公差，仅按标称尺寸推荐片数。")))))
  zwkfc:lines)

(defun zwkfc:show (lines)
  (foreach s lines (if (= s "") (princ "\n") (princ (strcat "\n" s))))
  (princ))

;; ---------------- 命令行版 ----------------
(defun zwkfc:ask (msg cur prec / v)
  (setq v (getreal (strcat "\n" msg " <" (zwkfc:f cur prec) ">: ")))
  (if (null v) cur v))

(defun zwkfc:aski (msg cur / k)
  (initget "Y N")
  (setq k (getkword (strcat "\n" msg " <" (if (= cur 1) "Y" "N") ">: ")))
  (if (null k) cur (if (= k "Y") 1 0)))

(defun c:JSQP ( / field v on lo hi)
  (foreach field zwkfc:fields
    (set (caddr field) (zwkfc:ask (strcat (cadr field) " (mm)") (eval (caddr field)) 4))
    (setq v (zwkfc:tol-get (car field)))
    (setq on (zwkfc:aski (strcat (cadr field) "是否带公差 [是(Y)/否(N)]") (car v)))
    (setq lo (cadr v) hi (caddr v))
    (if (= on 1)
      (setq hi (zwkfc:ask "上公差（按图纸正负号填写）" hi 4)
            lo (zwkfc:ask "下公差（按图纸正负号填写）" lo 4)))
    (zwkfc:tol-put (car field) (list on lo hi)))
  (setq zwkfc:ang (zwkfc:ask "铲切角度（度，0=自动）" zwkfc:ang 2))
  (setq zwkfc:incl (zwkfc:aski "图纸齿高是否包含工艺台阶 [是(Y)/否(N)]" zwkfc:incl))
  (zwkfc:show (zwkfc:run)) (princ))

;; ---------------- 图上量取 ----------------
(defun zwkfc:pick ( / kw p1 p2 d)
  (initget "L W T G P")
  (setq kw (getkword "\n量取哪个? [齿根距(L)/齿宽(W)/片厚(T)/间距(G)/步距(P)] <L>: "))
  (if (null kw) (setq kw "L"))
  (setq p1 (getpoint "\n指定第一点: "))
  (if p1
    (progn
      (setq p2 (getpoint p1 "\n指定第二点: "))
      (if p2
        (progn
          (setq d (distance (list (car p1) (cadr p1) 0.0) (list (car p2) (cadr p2) 0.0)))
          (cond ((= kw "L") (setq zwkfc:l d))
                ((= kw "W") (setq zwkfc:w d))
                ((= kw "T") (setq zwkfc:t d))
                ((= kw "G") (setq zwkfc:g d))
                (T (setq zwkfc:p d)))
          (princ (strcat "\n量得 " (zwkfc:f d 4) " mm，已填入。"))))))
  (princ))

;; ---------------- 写到图纸 ----------------
(defun zwkfc:ins ( / pt h i tx made want result)
  (if (null zwkfc:lines) (zwkfc:run))
  (setq made 0 want 0)
  (if (assoc 'VALID zwkfc:res)
    (princ "\n输入有误，请先修正参数再写到图纸。")
    (progn
      (setq h (getvar "TEXTSIZE"))
      (if (or (null h) (<= h 0.0)) (setq h 3.5))
      (setq pt (getpoint "\n指定结果文字的插入点: "))
      (if pt
        (progn
          (setq pt (trans pt 1 0) i 0)
          (foreach tx zwkfc:lines
            (if (/= tx "")
              (progn
                (setq want (1+ want))
                (setq result
                  (vl-catch-all-apply 'entmake
                    (list (list '(0 . "TEXT")
                                (cons 8 (getvar "CLAYER"))
                                (cons 10 (list (car pt) (- (cadr pt) (* i h 1.8)) (caddr pt)))
                                (cons 40 h) (cons 1 tx)))))
                (if (and result (not (vl-catch-all-error-p result)))
                  (setq made (1+ made)))))
            (setq i (1+ i)))
          (princ (strcat "\n已写入 " (itoa made) " / " (itoa want) " 行文字。"))))))
  made)

;; ---------------- DCL 面板 ----------------
(setq zwkfc:dcl-lines (list
  "zwkfc : dialog {"
  "label = \"工艺计算器\";"
  " : row {"
  " : boxed_column {"
  " label = \"图纸尺寸（mm）\";"
  " : row { : text { label = \"尺寸\"; width = 18; } : text { label = \"数值\"; width = 10; } : text { label = \"公差\"; width = 8; } : text { label = \"上公差\"; width = 9; } : text { label = \"下公差\"; width = 9; } }"
  " : row { : text { label = \"片厚\"; width = 18; } : edit_box { key = \"t\"; edit_width = 10; fixed_width = true; } : toggle { key = \"t_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"t_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"t_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"间距\"; width = 18; } : edit_box { key = \"g\"; edit_width = 10; fixed_width = true; } : toggle { key = \"g_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"g_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"g_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"步距\"; width = 18; } : edit_box { key = \"p\"; edit_width = 10; fixed_width = true; } : toggle { key = \"p_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"p_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"p_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"齿根距\"; width = 18; } : edit_box { key = \"l\"; edit_width = 10; fixed_width = true; } : toggle { key = \"l_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"l_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"l_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"齿宽\"; width = 18; } : edit_box { key = \"w\"; edit_width = 10; fixed_width = true; } : toggle { key = \"w_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"w_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"w_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"齿高\"; width = 18; } : edit_box { key = \"h\"; edit_width = 10; fixed_width = true; } : toggle { key = \"h_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"h_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"h_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"底板厚\"; width = 18; } : edit_box { key = \"hb\"; edit_width = 10; fixed_width = true; } : toggle { key = \"hb_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"hb_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"hb_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : row { : text { label = \"齿根台阶补偿\"; width = 18; } : edit_box { key = \"hr\"; edit_width = 10; fixed_width = true; } : toggle { key = \"hr_tol_on\"; label = \"带公差\"; width = 8; } : edit_box { key = \"hr_tol_hi\"; edit_width = 9; fixed_width = true; } : edit_box { key = \"hr_tol_lo\"; edit_width = 9; fixed_width = true; } }"
  " : edit_box { key = \"ang\"; label = \"铲切角度（°）\"; edit_width = 10; }"
  " : toggle { key = \"incl\"; label = \"图纸齿高包含工艺台阶\"; }"
  " : text { label = \"齿根距、齿宽均按图纸两端外边量取。\"; }"
  " : text { label = \"步距 = 片厚 + 间距；步距填 0 自动计算。\"; }"
  " : text { label = \"公差按图纸填写正负号，例如 +0.10、-0.05。\"; }"
  " : text { label = \"单项公差作记录，齿根距公差用于排齿判断。\"; }"
  " : text { label = \"角度填 0 自动推荐；改完数值按 Tab 更新。\"; }"
  " }"
  " : boxed_column { label = \"计算结果\"; : list_box { key = \"res\"; height = 24; width = 58; } }"
  " }"
  " : row {"
  " : button { key = \"calc\"; label = \"重新计算\"; }"
  " : button { key = \"pick\"; label = \"图上量取\"; }"
  " : button { key = \"ins\"; label = \"写到图纸\"; }"
  " : button { key = \"def\"; label = \"恢复默认\"; }"
  " : button { key = \"cancel\"; label = \"关闭\"; is_cancel = true; }"
  " }"
  "}"))

(defun zwkfc:dcl-file ( / path f)
  (setq path (strcat (getenv "TEMP") "/zwk-proccalc.dcl"))
  (if (findfile path) (vl-file-delete path))
  (setq f (open path "w"))
  (if (null f)
    nil
    (progn
      (foreach s zwkfc:dcl-lines (write-line s f))
      (close f)
      path)))

(defun zwkfc:tol-tiles ( / field k v disabled)
  (foreach field zwkfc:fields
    (setq k (car field) v (zwkfc:tol-get k) disabled (if (= (car v) 1) 0 1))
    (mode_tile (strcat k "_tol_hi") disabled)
    (mode_tile (strcat k "_tol_lo") disabled)))
(defun zwkfc:fill ( / field k v)
  (foreach field zwkfc:fields
    (setq k (car field) v (zwkfc:tol-get k))
    (set_tile k (zwkfc:f (eval (caddr field)) 8))
    (set_tile (strcat k "_tol_on") (itoa (car v)))
    (set_tile (strcat k "_tol_lo") (zwkfc:f (cadr v) 8))
    (set_tile (strcat k "_tol_hi") (zwkfc:f (caddr v) 8)))
  (set_tile "ang" (zwkfc:f zwkfc:ang 4))
  (set_tile "incl" (itoa zwkfc:incl))
  (zwkfc:tol-tiles))
(defun zwkfc:read ( / field k v on lo hi)
  (foreach field zwkfc:fields
    (setq k (car field) v (zwkfc:tol-get k))
    (set (caddr field) (zwkfc:nv (get_tile k) (eval (caddr field))))
    (setq on (if (= (get_tile (strcat k "_tol_on")) "1") 1 0))
    (setq lo (zwkfc:nv (get_tile (strcat k "_tol_lo")) (cadr v))
          hi (zwkfc:nv (get_tile (strcat k "_tol_hi")) (caddr v)))
    (zwkfc:tol-put k (list on lo hi)))
  (setq zwkfc:ang (zwkfc:nv (get_tile "ang") zwkfc:ang)
        zwkfc:incl (if (= (get_tile "incl") "1") 1 0)))
(defun zwkfc:refresh ( / output item)
  (zwkfc:read) (zwkfc:tol-tiles)
  (setq output (zwkfc:run))
  (mode_tile "ins" (if (assoc 'VALID zwkfc:res) 1 0))
  (start_list "res") (foreach item output (add_list item)) (end_list) (princ))
(defun zwkfc:on (k v) (zwkfc:refresh))
(defun zwkfc:bind-inputs ( / field k)
  (foreach field zwkfc:fields
    (setq k (car field))
    (action_tile k "(zwkfc:refresh)")
    (action_tile (strcat k "_tol_on") "(zwkfc:refresh)")
    (action_tile (strcat k "_tol_hi") "(zwkfc:refresh)")
    (action_tile (strcat k "_tol_lo") "(zwkfc:refresh)")))

(defun zwkfc:on-incl (v)
  (setq zwkfc:incl (if (= v "1") 1 0))
  (zwkfc:refresh))

(defun zwkfc:on-def ()
  (zwkfc:reset)
  (zwkfc:fill)
  (zwkfc:refresh))

(defun c:JSQ ( / *error* path id dlg)
  (defun *error* (msg)
    (if (and id (> id 0)) (unload_dialog id))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*取消*")))
      (princ (strcat "\n计算器未能打开：" msg)))
    (princ))
  (setq path (zwkfc:dcl-file))
  (if (null path)
    (progn (princ "\n面板文件写不出（临时目录不可写），改用命令行版。") (c:JSQP))
    (progn
      (setq id (load_dialog path))
      (if (< id 0)
        (progn (princ "\n面板加载失败，改用命令行版。") (c:JSQP))
        (progn
          (setq dlg 99)
          (while (/= dlg 0)
            (if (new_dialog "zwkfc" id)
              (progn
                (zwkfc:fill)
                (zwkfc:refresh)
                (zwkfc:bind-inputs)
                (action_tile "ang" "(zwkfc:on \"ang\" $value)")
                (action_tile "incl" "(zwkfc:on-incl $value)")
                (action_tile "calc" "(zwkfc:refresh)")
                (action_tile "def"  "(zwkfc:on-def)")
                (action_tile "pick" "(done_dialog 2)")
                (action_tile "ins"  "(done_dialog 3)")
                (setq dlg (start_dialog)))
              (setq dlg 0))
            (cond ((= dlg 2) (zwkfc:pick))
                  ((= dlg 3) (zwkfc:ins))))
          (unload_dialog id)
          (setq id nil)))
      (princ))))

;; ---------------- 测试入口（无界面，结果写文件） ----------------
(defun c:JSQN ( / f r lines)
  (setq r (zwkfc:calc 0.05 0.08 0.0 59.92 0.1 0.8 1.0 0.15 0.0 1))
  (setq lines (zwkfc:format-lines r 0.05 0.08 0.0 59.92 0.1 0.8 1.0 0.15))
  (setq f (open (strcat (getenv "TEMP") "/zwk-proccalc-test.txt") "w"))
  (if f
    (progn (foreach s lines (write-line s f)) (close f)))
  ;; 再补三个算例，便于自动比对
  (setq r (zwkfc:calc 0.15 0.5 0.65 53.9 0.05 1.0 1.0 0.15 0.0 1))
  (setq f (open (strcat (getenv "TEMP") "/zwk-proccalc-test.txt") "a"))
  (if f
    (progn
      (write-line "" f)
      (write-line (strcat "算例2: L=53.9 P=0.65 -> N0=" (cdr (assoc 'N0 r)) " N=" (cdr (assoc 'N r))
                          " ROOT=" (cdr (assoc 'ROOT r)) " HC=" (cdr (assoc 'HC r))
                          " ANG=" (cdr (assoc 'ANG r)) " RUN=" (cdr (assoc 'RUN r))
                          " SLANT=" (cdr (assoc 'SLANT r)) " WARN=" (cdr (assoc 'WARN r))) f)
      (close f)))
  (setq r (zwkfc:calc 0.05 0.08 0.0 24.92 0.1 0.8 1.0 0.15 0.0 1))
  (setq f (open (strcat (getenv "TEMP") "/zwk-proccalc-test.txt") "a"))
  (if f
    (progn
      (write-line (strcat "算例3: L=24.92 P=0.13 -> N0=" (cdr (assoc 'N0 r)) " N=" (cdr (assoc 'N r))
                          " ROOT=" (cdr (assoc 'ROOT r)) " DS=" (cdr (assoc 'DS r))
                          " WARN=" (cdr (assoc 'WARN r))) f)
      (close f)))
  (setq r (zwkfc:calc 0.08 0.10 0.0 32.24 0.4 1.95 1.0 0.2 0.0 1))
  (setq f (open (strcat (getenv "TEMP") "/zwk-proccalc-test.txt") "a"))
  (if f
    (progn
      (write-line (strcat "算例4: L=32.24 P=0.18 -> N0=" (cdr (assoc 'N0 r)) " N=" (cdr (assoc 'N r))
                          " ROOT=" (cdr (assoc 'ROOT r)) " DS=" (cdr (assoc 'DS r))
                          " HC=" (cdr (assoc 'HC r)) " ANG=" (cdr (assoc 'ANG r))
                          " RUN=" (cdr (assoc 'RUN r)) " SLANT=" (cdr (assoc 'SLANT r))) f)
      (close f)))
  (princ (strcat "\n测试结果已写入 " (getenv "TEMP") "/zwk-proccalc-test.txt"))
  (princ))

;; 旧命令退出，重载当前图纸后统一使用 JSQ/JSQP/JSQN。
(setq c:CJS nil c:CJSP nil c:CJSN nil)
(princ)
