;;; ZF: set clicked dimension or text to a chosen font, height, and
;;; color. ZFD is the window/crossing/fence batch version.
;;; Fonts: 1=宋体(SimSun) 2=华文细黑(STXihei) 3=Arial. Then pick the height.
;;; Color: asked up front like font/height (0=不改, else 0=随块/256=随层/
;;; 1-255=ACI); stays resident and can be changed anytime with Y until the
;;; command ends (T=font, H=height, Y=color -- avoids ssget's built-in
;;; Fence/Crossing keywords F/C). For DIMENSION, only the dimension TEXT
;;; recolors (via a per-entity DIMCLRT/178 DSTYLE xdata override) -- lines/
;;; arrows untouched.
;;; Dimensions: rewrite text-run fonts only; GDT symbol runs (ZGDT/gdt.shx) stay.
;;; Text/MTEXT: switch to the matching text style and set the height.
;;; Distribution file is GBK encoded. ZT stays the drag trim command.
;;; Needs modules/dimcore.lsp loaded first (zf20:num/ask-color/dim-xdata-set).
(vl-load-com)

;; (名称 字体代码 样式名 样式字体文件)
(defun zf20:font-info (n)
  (cond
    ((= n 2) (list "华文细黑" "\\fSTXihei|b0|i0|c134|p2;" "华文细黑" "STXIHEI.TTF"))
    ((= n 3) (list "Arial" "\\fArial|b0|i0|c0|p34;" "Arial" "arial.ttf"))
    (T       (list "宋体" "\\fSimSun|b0|i0|c134|p2;" "宋体" "simsun.ttc"))))

(defun zf20:make-style (name font / data)
  (cond
    ((tblsearch "STYLE" name) name)
    (T
      (setq data (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                       (cons 2 name) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0) '(42 . 2.5)
                       (cons 3 font) '(4 . "")))
      (if (not (entmake data))
        (entmake (list '(0 . "STYLE") (cons 2 name) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0)
                       '(71 . 0) '(42 . 2.5) (cons 3 font) '(4 . ""))))
      (if (tblsearch "STYLE" name) name nil))))

;; height 0 so the per-object height applies; fall back to a -ZF variant
(defun zf20:ensure-style (name font / e d)
  (if (setq e (tblobjname "STYLE" name))
    (progn
      (setq d (entget e))
      (if (and (= 0.0 (cdr (assoc 40 d)))
               (equal (strcase (cdr (assoc 3 d))) (strcase font)))
        name
        (zf20:make-style (strcat name "-ZF") font)))
    (zf20:make-style name font)))

(defun zf20:ask-font (dflt / s)
  (setq s (getstring (strcat "\n选择字体 [1] 宋体  [2] 华文细黑  [3] Arial <" (itoa dflt) ">: ")))
  (cond
    ((= s "1") 1)
    ((= s "2") 2)
    ((= s "3") 3)
    (T dflt)))

(defun zf20:ask-height (dflt / s v)
  (if (null dflt) (setq dflt 2.5))
  (while (null v)
    (setq s (getstring (strcat "\n选择字高 [1]1.5 [2]2 [3]2.5 [4]3 [5]4 [6]8 [7]10 [8]其他 <" (zf20:num dflt) ">: ")))
    (cond
      ((= s "") (setq v dflt))
      ((= s "1") (setq v 1.5))
      ((= s "2") (setq v 2.0))
      ((= s "3") (setq v 2.5))
      ((= s "4") (setq v 3.0))
      ((= s "5") (setq v 4.0))
      ((= s "6") (setq v 8.0))
      ((= s "7") (setq v 10.0))
      ((= s "8")
        (setq v (getreal "\n输入字高 <2.5>: "))
        (if (null v) (setq v 2.5)))
      (T
        (setq v (atof s))
        (if (<= v 0.0)
          (progn (setq v nil) (princ "\n请输入 1-8，或一个大于 0 的数字。"))))))
  v)

;; color 0=ByBlock, 256=ByLayer, 1-255=ACI index; applies to any entity's own color
(defun zf20:set-color (ent color / d)
  (setq d (entget ent))
  (if (assoc 62 d)
    (setq d (subst (cons 62 color) (assoc 62 d) d))
    (setq d (append d (list (cons 62 color)))))
  (if (entmod d)
    (progn
      (entupd ent)
      (equal (cdr (assoc 62 (entget ent))) color))))

;; per-entity DIMCLRT (var 178) override: recolors only the dimension's
;; text, leaving the dimension/extension lines and arrows untouched.
(defun zf20:dim-set-text-color (ent color)
  (zf20:dim-xdata-set ent 178 1070 color))

;; ---- dimension text override ----

;; font name of a \f...; token: "\fZGDT|b1|i0|c0|p34" -> "ZGDT"
(defun zf20:fontname (tok / s i)
  (setq s (substr tok 3)
        i (vl-string-search "|" s))
  (if (null i) (setq i (vl-string-search ";" s)))
  (if i (substr s 1 i) s))

;; Rewrite top-level \f codes to ftok, but keep GDT symbol fonts as-is
;; (ZGDT/gdt.shx 的符号不能换成普通字体); drop top-level \H codes, and
;; keep nested ones such as {\H0.7x;\S+0.04^-0.01;} used by tolerances.
(defun zf20:retag (s ftok / i n depth c two pre post tok)
  (setq i 1 depth 0)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((= c "{") (setq depth (1+ depth) i (1+ i)))
      ((= c "}") (if (> depth 0) (setq depth (1- depth))) (setq i (1+ i)))
      ((and (= depth 0) (= c "\\") (= (strlen (setq two (substr s i 2))) 2)
            (member (strcase two) '("\\F" "\\H")))
        (setq n (vl-string-search ";" s i))
        (cond
          ((null n)
            (setq s (if (> i 1) (substr s 1 (1- i)) "") i 1))
          (T
            (setq tok (substr s i (- n i -1))
                  pre (if (> i 1) (substr s 1 (1- i)) "")
                  post (substr s (+ n 2)))
            (cond
              ((= (strcase two) "\\H")
                (setq s (strcat pre post)
                      i (1+ (strlen pre))))
              ((wcmatch (strcase (zf20:fontname tok)) "*GDT*")
                (setq i (+ n 2)))
              (T
                (setq s (strcat pre ftok post)
                      i (+ (strlen pre) (strlen ftok) 1)))))))
      (T (setq i (1+ i)))))
  s)

;; per-entity DIMTXT override (xdata code 42), nil when absent
(defun zf20:read-height (ent / pairs found)
  (setq pairs (cadr (zf20:split (zf20:acad-items ent))))
  (while (and pairs (not found))
    (if (and (listp (car pairs)) (= (car (car pairs)) 1070) (= (cdr (car pairs)) 42)
             (listp (cadr pairs)) (= (car (cadr pairs)) 1040))
      (setq found (cdr (cadr pairs)))
      (setq pairs (cdr pairs))))
  found)

;; height the override is relative to: DIMTXT override or the dimstyle's
(defun zf20:dim-base (ent / v ds)
  (setq v (zf20:read-height ent))
  (if (null v)
    (if (setq ds (tblobjname "DIMSTYLE" (cdr (assoc 3 (entget ent)))))
      (setq v (cdr (assoc 140 (entget ds))))))
  (if (and v (> v 0.0)) v nil))

(defun zf20:dim-override (old h base ftok / rest htok)
  (setq rest (zf20:retag old ftok))
  (if (= rest "") (setq rest "<>"))
  (if (and base (> base 0.0))
    (setq htok (strcat "\\H" (zf20:num (/ h base)) "x;"))
    (setq htok (strcat "\\H" (zf20:num h) ";")))
  (strcat ftok htok rest))

(defun zf20:set-dim (ent h ftok / d base new)
  (setq d (entget ent)
        base (zf20:dim-base ent)
        new (zf20:dim-override (if (assoc 1 d) (cdr (assoc 1 d)) "") h base ftok))
  (if (entmod (subst (cons 1 new) (assoc 1 d) d))
    (progn
      (entupd ent)
      (equal (cdr (assoc 1 (entget ent))) new))))

(defun zf20:set-text (ent h sty / d)
  (setq d (entget ent))
  (if (assoc 7 d)
    (setq d (subst (cons 7 sty) (assoc 7 d) d))
    (setq d (append d (list (cons 7 sty)))))
  (if (assoc 40 d)
    (setq d (subst (cons 40 h) (assoc 40 d) d))
    (setq d (append d (list (cons 40 h)))))
  (if (entmod d)
    (progn
      (entupd ent)
      (setq d (entget ent))
      (and (equal (cdr (assoc 7 d)) sty) (equal (cdr (assoc 40 d)) h 1e-6)))))

(defun c:ZF (/ *error* font h sty sel ent typ n finfo color)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\nZF 提示：" msg)))
    (princ))
  (setq font (zf20:ask-font (if (member zwk:zf:font '(1 2 3)) zwk:zf:font 1))
        finfo (zf20:font-info font)
        sty (zf20:ensure-style (caddr finfo) (cadddr finfo)))
  (if sty
    (progn
      (setq zwk:zf:font font)
      (setq h (zf20:ask-height (if (numberp zwk:zf:h) zwk:zf:h 2.5)))
      (setq zwk:zf:h h)
      (setq color (zf20:ask-color zwk:zf:color))
      (setq zwk:zf:color color)
      (princ (strcat "\nZF：" (car finfo) "，字高 " (zf20:num h)
                     (if color (strcat "，颜色 " (itoa color)) "，颜色不改")
                     "。连续点选标注或文字；输入 T 换字体，H 换字高，Y 改颜色，回车结束。"))
      (setq n 0)
      (while (progn (initget "T H Y") (setq sel (entsel "\n点选标注或文字 [换字体(T)/换字高(H)/改颜色(Y)] <回车结束>: ")))
        (cond
          ((equal sel "T")
            (setq font (zf20:ask-font font)
                  finfo (zf20:font-info font)
                  sty (zf20:ensure-style (caddr finfo) (cadddr finfo))
                  zwk:zf:font font)
            (princ (strcat "\n字体改为 " (car finfo) "。")))
          ((equal sel "H")
            (setq h (zf20:ask-height h)
                  zwk:zf:h h)
            (princ (strcat "\n字高改为 " (zf20:num h) "。")))
          ((equal sel "Y")
            (setq color (zf20:ask-color color)
                  zwk:zf:color color)
            (princ (if color (strcat "\n颜色改为 " (itoa color) "。") "\n颜色改为不改动。")))
          (T
            (setq ent (car sel) typ (cdr (assoc 0 (entget ent))))
            (cond
              ((= typ "DIMENSION")
                (if (zf20:set-dim ent h (cadr finfo))
                  (progn
                    (setq n (1+ n))
                    (if color (zf20:dim-set-text-color ent color)))
                  (princ "\n这个标注修改失败：文字替代未写入，请反馈。")))
              ((or (= typ "TEXT") (= typ "MTEXT"))
                (if (zf20:set-text ent h sty)
                  (progn
                    (setq n (1+ n))
                    (if color (zf20:set-color ent color)))
                  (princ "\n这个文字修改失败，请反馈。")))
              (T (princ (strcat "\n不支持的对象类型：" typ "。请点标注、单行文字或多行文字。")))))))
      (princ (strcat "\nZF 结束，共修改 " (itoa n) " 个对象。")))
    (princ (strcat "\nZF：无法准备" (car finfo) "文字样式，已停止。")))
  (princ))

;; ---- ZFD: drag/window/crossing/fence batch version of ZF ----

(defun zf20:apply (ent h ftok sty color / typ ok)
  (setq typ (cdr (assoc 0 (entget ent)))
        ok (cond
             ((= typ "DIMENSION") (zf20:set-dim ent h ftok))
             ((or (= typ "TEXT") (= typ "MTEXT")) (zf20:set-text ent h sty))
             (T nil)))
  (if (and ok color)
    (if (= typ "DIMENSION") (zf20:dim-set-text-color ent color) (zf20:set-color ent color)))
  ok)

(defun zf20:apply-ss (ss h ftok sty color / i n ent)
  (setq i 0 n 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i) i (1+ i))
    (if (zf20:apply ent h ftok sty color) (setq n (1+ n))))
  n)

;; ZWCAD's own ssget "Select objects:" prompt has a fixed built-in keyword
;; table (W/L/C/F/WP/CP/...) and does not relay custom initget keywords at
;; all -- so T/H/Y cannot be typed at the selection prompt itself. Instead,
;; every round goes straight into ssget so dragging batch after batch is
;; never interrupted; only when the user pauses (presses Enter with nothing
;; picked, the natural "I'm done selecting for now" gesture) do we surface a
;; getkword menu (which does honor initget) offering T/H/Y or an end.
(defun c:ZFD (/ *error* font h sty finfo total go ss color opt)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\nZFD 提示：" msg)))
    (princ))
  (setq font (zf20:ask-font (if (member zwk:zf:font '(1 2 3)) zwk:zf:font 1))
        finfo (zf20:font-info font)
        sty (zf20:ensure-style (caddr finfo) (cadddr finfo)))
  (if sty
    (progn
      (setq zwk:zf:font font)
      (setq h (zf20:ask-height (if (numberp zwk:zf:h) zwk:zf:h 2.5)))
      (setq zwk:zf:h h)
      (setq color (zf20:ask-color zwk:zf:color))
      (setq zwk:zf:color color)
      (princ (strcat "\nZFD：" (car finfo) "，字高 " (zf20:num h)
                     (if color (strcat "，颜色 " (itoa color)) "，颜色不改")
                     "。直接框选/交叉/栏选批量选中标注和文字（按住左键拖动即可，其余物体不会被选中），"
                     "可连续多次框选不间断；选累了按一下空回车会问要不要改字体/字高/颜色或结束。"))
      (setq total 0 go T)
      (while go
        (setq ss (ssget '((0 . "DIMENSION,TEXT,MTEXT"))))
        (cond
          (ss
            (setq total (+ total (zf20:apply-ss ss h (cadr finfo) sty color)))
            (princ (strcat "\n本次批量修改 " (itoa (sslength ss)) " 个，累计 " (itoa total)
                           " 个。可继续框选/交叉/栏选，回车（不选对象）可改字体/字高/颜色或结束。")))
          (T
            ;; empty pick: pause menu, since ssget itself can't relay T/H/Y
            (initget "T H Y")
            (setq opt (getkword "\n空选 - 输入 T 换字体/H 换字高/Y 换颜色，或直接回车结束 <结束>: "))
            (cond
              ((equal opt "T")
                (setq font (zf20:ask-font font)
                      finfo (zf20:font-info font)
                      sty (zf20:ensure-style (caddr finfo) (cadddr finfo))
                      zwk:zf:font font)
                (princ (strcat "\n字体改为 " (car finfo) "，继续框选/交叉/栏选。")))
              ((equal opt "H")
                (setq h (zf20:ask-height h)
                      zwk:zf:h h)
                (princ (strcat "\n字高改为 " (zf20:num h) "，继续框选/交叉/栏选。")))
              ((equal opt "Y")
                (setq color (zf20:ask-color color)
                      zwk:zf:color color)
                (princ (strcat (if color (strcat "\n颜色改为 " (itoa color) "。") "\n颜色改为不改动。")
                               "继续框选/交叉/栏选。")))
              (T (setq go nil))))))
      (princ (strcat "\nZFD 结束，共修改 " (itoa total) " 个对象。")))
    (princ (strcat "\nZFD：无法准备" (car finfo) "文字样式，已停止。")))
  (princ))

(princ "\nZF 改字体已加载：先选字体（宋体/华文细黑/Arial），再选字高、选颜色（可选不改）；ZF 连续点选单个对象，ZFD 框选/交叉/栏选批量修改，随时输入 T/H/Y 换字体/字高/颜色。")
(princ)
