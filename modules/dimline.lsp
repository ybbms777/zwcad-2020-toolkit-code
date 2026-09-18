;;; BZ / BZD: change a DIMENSION's own line color, extension-line color,
;;; arrow size, and lineweight -- never touches the dimension TEXT (that's
;;; ZF/ZFD's job, in font.lsp). Reuses the zf20:* xdata helpers there, so
;;; font.lsp must be loaded first (see modules.lsp order).
;;; Defaults: color=green(3), arrow size=1.8, lineweight=0.05mm.
;;; Distribution file is GBK encoded.
(vl-load-com)

(defun zl20:ask-color (dflt) (zf20:ask-color dflt))

(defun zl20:ask-size (dflt / s v)
  (if (null dflt) (setq dflt 1.8))
  (while (null v)
    (setq s (getstring (strcat "\n选择箭头大小 [1]1.8 [2]2.5 [3]3 [4]自定义 <" (zf20:num dflt) ">: ")))
    (cond
      ((= s "") (setq v dflt))
      ((= s "1") (setq v 1.8))
      ((= s "2") (setq v 2.5))
      ((= s "3") (setq v 3.0))
      ((= s "4")
        (setq v (getreal "\n输入箭头大小 <1.8>: "))
        (if (null v) (setq v 1.8)))
      (T
        (setq v (atof s))
        (if (<= v 0.0)
          (progn (setq v nil) (princ "\n请输入 1-4，或一个大于 0 的数字。"))))))
  v)

;; internal unit: hundredths of a millimeter (DXF group 370 convention)
(defun zl20:ask-lw (dflt / s v mm)
  (if (null dflt) (setq dflt 5))
  (while (null v)
    (setq s (getstring (strcat "\n选择线宽(毫米) [1]0.05 [2]0.13 [3]0.18 [4]0.25 [5]0.35 [6]自定义 <"
                               (zf20:num (/ dflt 100.0)) ">: ")))
    (cond
      ((= s "") (setq v dflt))
      ((= s "1") (setq v 5))
      ((= s "2") (setq v 13))
      ((= s "3") (setq v 18))
      ((= s "4") (setq v 25))
      ((= s "5") (setq v 35))
      ((= s "6")
        (setq mm (getreal "\n输入线宽(毫米) <0.05>: "))
        (if (null mm) (setq mm 0.05))
        (setq v (fix (+ 0.5 (* mm 100.0)))))
      (T
        (setq mm (atof s))
        (if (<= mm 0.0)
          (princ "\n请输入 1-6，或一个大于 0 的毫米数。")
          (setq v (fix (+ 0.5 (* mm 100.0))))))))
  v)

;; DIMCLRD(176)+DIMCLRE(177) override: dim line + ext line color (arrows
;; follow DIMCLRD too). Leaves DIMCLRT(178)/text alone. nil color = no-op.
(defun zl20:set-line-color (ent color)
  (if color
    (and (zf20:dim-xdata-set ent 176 1070 color)
         (zf20:dim-xdata-set ent 177 1070 color))
    T))

;; DIMASZ(41) override: arrow/tick size
(defun zl20:set-arrow-size (ent sz)
  (zf20:dim-xdata-set ent 41 1040 sz))

;; DIMLWD(371)+DIMLWE(372) override: dim/ext line weight (hundredths of mm)
(defun zl20:set-lw (ent lw)
  (and (zf20:dim-xdata-set ent 371 1070 lw)
       (zf20:dim-xdata-set ent 372 1070 lw)))

(defun zl20:apply (ent color asz lw)
  (if (= (cdr (assoc 0 (entget ent))) "DIMENSION")
    (progn
      (entupd ent)
      (and (zl20:set-line-color ent color)
           (zl20:set-arrow-size ent asz)
           (zl20:set-lw ent lw)))))

(defun zl20:apply-ss (ss color asz lw / i n ent)
  (setq i 0 n 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i) i (1+ i))
    (if (zl20:apply ent color asz lw) (setq n (1+ n))))
  n)

(defun c:BZ (/ *error* color asz lw sel ent n)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\nBZ 提示：" msg)))
    (princ))
  (setq color (zl20:ask-color (if zwk:zl:color zwk:zl:color 3))
        zwk:zl:color color)
  (setq asz (zl20:ask-size (if (numberp zwk:zl:asz) zwk:zl:asz 1.8))
        zwk:zl:asz asz)
  (setq lw (zl20:ask-lw (if (numberp zwk:zl:lw) zwk:zl:lw 5))
        zwk:zl:lw lw)
  (princ (strcat "\nBZ：颜色 " (if color (itoa color) "不改") "，箭头 " (zf20:num asz)
                 "，线宽 " (zf20:num (/ lw 100.0)) "mm。连续点选标注线；输入 Y 改颜色，S 改箭头，W 改线宽，回车结束。"))
  (setq n 0)
  (while (progn (initget "Y S W") (setq sel (entsel "\n点选标注线 [改颜色(Y)/改箭头(S)/改线宽(W)] <回车结束>: ")))
    (cond
      ((equal sel "Y")
        (setq color (zl20:ask-color color) zwk:zl:color color)
        (princ (if color (strcat "\n颜色改为 " (itoa color) "。") "\n颜色改为不改动。")))
      ((equal sel "S")
        (setq asz (zl20:ask-size asz) zwk:zl:asz asz)
        (princ (strcat "\n箭头改为 " (zf20:num asz) "。")))
      ((equal sel "W")
        (setq lw (zl20:ask-lw lw) zwk:zl:lw lw)
        (princ (strcat "\n线宽改为 " (zf20:num (/ lw 100.0)) "mm。")))
      (T
        (setq ent (car sel))
        (if (= (cdr (assoc 0 (entget ent))) "DIMENSION")
          (if (zl20:apply ent color asz lw)
            (setq n (1+ n))
            (princ "\n这个标注修改失败，请反馈。"))
          (princ "\nBZ 只支持标注(DIMENSION)对象，请点标注线。")))))
  (princ (strcat "\nBZ 结束，共修改 " (itoa n) " 个标注。"))
  (princ))

;; BZD: window/crossing/fence batch version of BZ. Same pattern as ZFD --
;; ssget's own "Select objects:" prompt can't relay custom keywords, so
;; continuous dragging is never interrupted; only an empty pick (Enter with
;; nothing selected) surfaces the Y/S/W settings menu via getkword.
(defun c:BZD (/ *error* color asz lw total go ss opt)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\nBZD 提示：" msg)))
    (princ))
  (setq color (zl20:ask-color (if zwk:zl:color zwk:zl:color 3))
        zwk:zl:color color)
  (setq asz (zl20:ask-size (if (numberp zwk:zl:asz) zwk:zl:asz 1.8))
        zwk:zl:asz asz)
  (setq lw (zl20:ask-lw (if (numberp zwk:zl:lw) zwk:zl:lw 5))
        zwk:zl:lw lw)
  (princ (strcat "\nBZD：颜色 " (if color (itoa color) "不改") "，箭头 " (zf20:num asz)
                 "，线宽 " (zf20:num (/ lw 100.0)) "mm。直接框选/交叉/栏选批量选中标注线（其余对象不会被选中），"
                 "可连续多次框选不间断；回车（不选对象）可改颜色/箭头/线宽或结束。"))
  (setq total 0 go T)
  (while go
    (setq ss (ssget '((0 . "DIMENSION"))))
    (cond
      (ss
        (setq total (+ total (zl20:apply-ss ss color asz lw)))
        (princ (strcat "\n本次批量修改 " (itoa (sslength ss)) " 个，累计 " (itoa total)
                       " 个。可继续框选/交叉/栏选，回车（不选对象）可改颜色/箭头/线宽或结束。")))
      (T
        (initget "Y S W")
        (setq opt (getkword "\n空选 - 输入 Y 改颜色/S 改箭头/W 改线宽，或直接回车结束 <结束>: "))
        (cond
          ((equal opt "Y")
            (setq color (zl20:ask-color color) zwk:zl:color color)
            (princ (strcat (if color (strcat "\n颜色改为 " (itoa color) "。") "\n颜色改为不改动。") "继续框选/交叉/栏选。")))
          ((equal opt "S")
            (setq asz (zl20:ask-size asz) zwk:zl:asz asz)
            (princ (strcat "\n箭头改为 " (zf20:num asz) "，继续框选/交叉/栏选。")))
          ((equal opt "W")
            (setq lw (zl20:ask-lw lw) zwk:zl:lw lw)
            (princ (strcat "\n线宽改为 " (zf20:num (/ lw 100.0)) "mm，继续框选/交叉/栏选。")))
          (T (setq go nil))))))
  (princ (strcat "\nBZD 结束，共修改 " (itoa total) " 个标注。"))
  (princ))

(princ "\nBZ/BZD 标注线属性已加载：改标注线颜色（默认绿）、箭头大小（默认1.8）、线宽（默认0.05mm），只影响标注线不影响文字。")
(princ)
