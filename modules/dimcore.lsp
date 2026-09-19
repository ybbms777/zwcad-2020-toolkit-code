;;; Shared low-level helpers for DIMENSION per-entity overrides, used by
;;; both font.lsp (ZF/ZFD text font/height/color) and dimline.lsp (BZ/BZD
;;; line/arrow/lineweight) -- kept independent of either so the two
;;; features stay separately selectable at install time. Always loaded.
;;; Distribution file is GBK encoded.
(vl-load-com)

;; 2.500000 -> "2.5", 0.8333333 -> "0.833333"
(defun zf20:num (x / s)
  (setq s (rtos x 2 6))
  (while (and (> (strlen s) 1) (= "0" (substr s (strlen s) 1)))
    (setq s (substr s 1 (1- (strlen s)))))
  (if (= "." (substr s (strlen s) 1))
    (setq s (substr s 1 (1- (strlen s)))))
  s)

;; nil = 不改颜色；否则 0=随块，256=随层，1-255=标准色（ACI）
(defun zf20:ask-color (dflt / s v done)
  (while (not done)
    (setq s (getstring (strcat "\n选择颜色 [0]不改 [1]红 [2]黄 [3]绿 [4]青 [5]蓝 [6]白/黑 [7]随层 [8]其他(编号) <"
                               (if dflt (itoa dflt) "0") ">: ")))
    (cond
      ((= s "") (setq v dflt done T))
      ((= s "0") (setq v nil done T))
      ((= s "1") (setq v 1 done T))
      ((= s "2") (setq v 2 done T))
      ((= s "3") (setq v 3 done T))
      ((= s "4") (setq v 4 done T))
      ((= s "5") (setq v 5 done T))
      ((= s "6") (setq v 7 done T))
      ((= s "7") (setq v 256 done T))
      ((= s "8")
        (setq v (getint "\n输入颜色编号 <7>（0=随块，256=随层，1-255=标准色）: "))
        (if (null v) (setq v 7))
        (if (or (< v 0) (> v 256))
          (princ "\n颜色编号需在 0-256 之间。")
          (setq done T)))
      (T
        (princ "\n请输入 0-8 之一。"))))
  v)

(defun zf20:acad-items (ent / xd)
  (setq xd (cdr (assoc -3 (entget ent '("ACAD")))))
  (cdr (assoc "ACAD" xd)))

(defun zf20:split (items / before pairs after state)
  (setq state 0)
  (foreach p items
    (cond
      ((= state 0)
        (if (equal p (cons 1002 "{")) (setq state 1) (setq before (cons p before))))
      ((= state 1)
        (if (equal p (cons 1002 "}")) (setq state 2) (setq pairs (cons p pairs))))
      (T (setq after (cons p after)))))
  (list (reverse before) (reverse pairs) (reverse after)))

;; remove a DSTYLE override pair (1070 . key)(<value>) from a raw xdata pair list
(defun zf20:xdata-drop (pairs key / out skip p)
  (foreach p pairs
    (cond
      (skip (setq skip nil))
      ((and (listp p) (= (car p) 1070) (= (cdr p) key)) (setq skip T))
      (T (setq out (cons p out)))))
  (reverse out))

;; generic per-entity DSTYLE override: (varcode . value) written under the
;; xdata "ACAD" app, e.g. DIMCLRT=178 (color, 1070), DIMASZ=41 (real, 1040).
;; valcode is the DXF group the value itself uses (1070 int / 1040 real).
;; Used for DIMENSION-only local overrides that never touch the dimstyle.
(defun zf20:dim-xdata-set (ent varcode valcode value / items parts b p a newpairs newitems)
  (setq items (zf20:acad-items ent))
  (if items
    (progn
      (setq parts (zf20:split items))
      (setq b (car parts) p (cadr parts) a (caddr parts)))
    (setq b (list (cons 1000 "DSTYLE")) p nil a nil))
  (setq newpairs (append (zf20:xdata-drop p varcode) (list (cons 1070 varcode) (cons valcode value))))
  (setq newitems (append b (list (cons 1002 "{")) newpairs (list (cons 1002 "}")) a))
  (if (not (tblsearch "APPID" "ACAD")) (regapp "ACAD"))
  (if (entmod (list (cons -1 ent) (cons -3 (list (cons "ACAD" newitems)))))
    (progn (entupd ent) T)))

(princ)
