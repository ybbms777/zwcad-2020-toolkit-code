;;; TKE: 图框属性编辑器。GBK 编码。
;;; 只针对【样品图】的图框，量产图以后单独做。
;;;
;;; 主要解决：公司料号要在三个地方输入 —— 客户编号 KY-xxx、产品编号 样品-xxx、
;;; 产品料号 01000xxx，三处数字相同，输多了容易漏改。这里只输一次数字。
;;;
;;; 前几版踩过的坑，别改回去：
;;;  1) 这个图框里「产品编号」和「客户编号」两个属性的标记都叫「编号」，
;;;     只按标记找会找错。做法：先用值前缀认（KY* / 样品*），认不出来再按出现次序取。
;;;  2) 取属性方法一律包 vl-catch-all-apply —— 不同 CAD 版本上方法名/返回值可能有差异。
;;;  3) 不要用 let —— 标准 AutoLISP 没有这个宏，只用 setq + defun 局部变量。
;;;  4) getkword 按回车返回 nil，不能直接 (= nil "Y") 比较，必须先判 null。
;;;  5) 提示里不要用宽字符框线和中文对齐填充，cmd.exe 里会错位。

(vl-load-com)

;; ---------- 角色表：(键 显示名 标记 值前缀 同标记里的第几个) ----------
(setq tke:roles
  '(("cno"  "客户编号" "编号"    "KY"   2)
    ("pno"  "产品编号" "编号"    "样品" 1)
    ("mat"  "产品料号" "料号"    nil    1)
    ("dwg"  "图名"     "图名"    nil    1)
    ("mtrl" "材质"     "材质"    nil    1)
    ("own"  "负责人"   "负责人A" nil    1)
    ("qty"  "样品数量" "数量"    nil    1)
    ("ver"  "版本"     "版本"    nil    1)
    ("dat"  "图纸日期" "日期"    nil    1)
    ("pg1"  "共几页"   "共几页"  nil    1)
    ("pg2"  "第几页"   "第几页"  nil    1)))

;; 料号前缀（可改）。产品料号 = 前缀 + 数字，例如 01000 + 851 = 01000851
(setq tke:matpre "01000")
(setq tke:custpre "KY-")
(setq tke:samplepre "样品-")

(setq tke:list nil)   ;; ((标记 当前值 属性对象) ...)
(setq tke:plan nil)   ;; ((键 显示名 新值 原值) ...)
(setq tke:ent nil)    ;; 当前编辑的图框块

;; ---------- 读一个块参照的全部属性 ----------
(defun tke:attrs (ent / obj arr out tag txt)
  (setq obj (vlax-ename->vla-object ent))
  (setq arr (vlax-safearray->list
              (vlax-variant-value (vla-get-attributes obj))))
  (setq out nil)
  (foreach a arr
    (setq tag (vl-catch-all-apply 'vla-get-tagstring (list a)))
    (if (vl-catch-all-error-p tag) (setq tag ""))
    (if (null tag) (setq tag ""))
    (setq txt (vl-catch-all-apply 'vla-get-textstring (list a)))
    (if (vl-catch-all-error-p txt) (setq txt ""))
    (if (null txt) (setq txt ""))
    (setq out (cons (list tag txt a) out)))
  (reverse out))

;; 读属性并判断是不是「图框块」：要有「料号」或「图名」属性
(defun tke:ok (ent / L)
  (if (null ent)
    nil
    (progn
      (setq L (vl-catch-all-apply 'tke:attrs (list ent)))
      (if (or (vl-catch-all-error-p L) (null L))
        nil
        (if (or (member "料号" (mapcar 'car L))
                (member "图名" (mapcar 'car L)))
          L
          nil)))))

(defun tke:blkname (ent / n)
  (setq n (cdr (assoc 2 (entget ent))))
  (if n n "?"))

;; ---------- 找图框 ----------
(defun tke:find (/ ss i ent hits n)
  (setq hits nil)
  (setq ss (vl-catch-all-apply 'ssget (list "X" '((0 . "INSERT") (66 . 1)))))
  (if (vl-catch-all-error-p ss) (setq ss nil))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (if (tke:ok (ssname ss i)) (setq hits (cons (ssname ss i) hits)))
        (setq i (1+ i)))))
  (setq n (length hits))
  (cond
    ((= n 1) (car hits))
    ((> n 1)
     (princ (strcat "\n      图纸里有 " (itoa n)
                    " 个带属性的图框，请点选要编辑的那一个: "))
     (setq ent (car (entsel)))
     (if (tke:ok ent) ent nil))
    (T
     (princ "\n      没自动找到图框（块里要有「料号」或「图名」属性）。请点选图框: ")
     (setq ent (car (entsel)))
     (if (tke:ok ent) ent nil))))

;; ---------- 角色 -> 属性下标 ----------
;; 同一标记可能有多个属性（「编号」有两处），先用值前缀认，认不出来再按出现次序取
(defun tke:idx (tag hint occ / cands i k e)
  (setq cands nil i 0)
  (foreach e tke:list
    (if (= (car e) tag) (setq cands (cons i cands)))
    (setq i (1+ i)))
  (setq cands (reverse cands))
  (if (null cands)
    nil
    (progn
      (setq k nil)
      (if hint
        (foreach i cands
          (if (and (null k)
                   (wcmatch (strcase (cadr (nth i tke:list)))
                            (strcat (strcase hint) "*")))
            (setq k i))))
      (if (and (null k) (<= occ (length cands)))
        (setq k (nth (1- occ) cands)))
      k)))

(defun tke:get (role / r k)
  (setq r (assoc role tke:roles))
  (if r
    (progn
      (setq k (tke:idx (nth 2 r) (nth 3 r) (nth 4 r)))
      (if k (cadr (nth k tke:list)) nil))
    nil))

(defun tke:set (role val / r k e)
  (setq r (assoc role tke:roles))
  (if r
    (progn
      (setq k (tke:idx (nth 2 r) (nth 3 r) (nth 4 r)))
      (if k
        (progn
          (setq e (nth k tke:list))
          (vl-catch-all-apply 'vla-put-textstring (list (nth 2 e) val))
          (setcar (cdr e) val)
          T)
        nil))
    nil))

;; ---------- 只有变化了才记进计划 ----------
(defun tke:push (role label new / old)
  (setq old (tke:get role))
  (if (and new (not (equal new old)))
    (setq tke:plan (cons (list role label new old) tke:plan))))

;; ---------- 问一项：回车 = 保持原值 ----------
(defun tke:ask (role label / old v)
  (setq old (tke:get role))
  (setq v (getstring (strcat "\n      " label " <"
                             (if (and old (/= old "")) old "空") ">: ")))
  (if (or (null v) (= v "")) old v))

;; ---------- 是不是纯数字 ----------
(defun tke:digits (s / i ok)
  (setq ok (and s (/= s "")) i 1)
  (while (and ok (<= i (strlen s)))
    (if (not (wcmatch (substr s i 1) "#")) (setq ok nil))
    (setq i (1+ i)))
  ok)

;; ---------- 从现有值里猜料号数字 ----------
(defun tke:guess-num (/ a b c p)
  (setq a (tke:get "mat") b (tke:get "cno") c (tke:get "pno"))
  (setq p (strlen tke:matpre))
  (cond
    ((and a (> (strlen a) p) (wcmatch a (strcat tke:matpre "*")))
     (substr a (1+ p)))
    ((and b (> (strlen b) (strlen tke:custpre))
          (wcmatch b (strcat tke:custpre "*")))
     (substr b (1+ (strlen tke:custpre))))
    ((and c (> (strlen c) (strlen tke:samplepre))
          (wcmatch c (strcat tke:samplepre "*")))
     (substr c (1+ (strlen tke:samplepre))))
    (T "")))

;; ---------- 显示当前值 ----------
(defun tke:dump (/ old)
  (princ (strcat "\n      图框块：" (tke:blkname tke:ent)
                 "（共 " (itoa (length tke:list)) " 个属性）"))
  (princ "\n      当前值：")
  (foreach r tke:roles
    (setq old (tke:get (car r)))
    (princ (strcat "\n        " (nth 1 r) " = "
                   (if (and old (/= old "")) old "（空）")))))

;; ---------- 主命令 ----------
(defun c:TKE (/ ent num ans n old ug)
  (princ "\n")
  (princ "\n  ============== 图框属性编辑器（样品图） ==============")
  (princ "\n  只针对【样品图】的图框，量产图暂不支持。")
  (princ "\n  料号只输一次数字，自动填三处：")
  (princ "\n     客户编号 KY-xxx   产品编号 样品-xxx   产品料号 01000xxx")
  (princ "\n  每一项直接回车 = 保持原值不变；Esc 随时取消。")
  (princ "\n  =====================================================")
  (setq ent (tke:find))
  (if (null ent)
    (princ "\n已取消（没选到图框）。")
    (progn
      (setq tke:ent ent)
      (setq tke:list (tke:attrs ent))
      (setq tke:plan nil)
      (tke:dump)

      ;; ---- [1/2] 料号数字 ----
      (setq old (tke:guess-num))
      (setq num (getstring (strcat "\n[1/2] 公司料号数字（只输数字，如 851）<"
                                   (if (= old "") "空" old) ">: ")))
      (if (or (null num) (= num "")) (setq num old))
      (while (and (/= num "") (not (tke:digits num)))
        (princ "\n      只能输纯数字，例如 851。")
        (setq num (getstring "\n      重新输入料号数字（回车=不改料号）: "))
        (if (or (null num) (= num "")) (setq num "")))
      (if (= num "")
        (princ "\n      没给料号数字，料号三处不动。")
        (progn
          (princ (strcat "\n      -> 客户编号 " tke:custpre num))
          (princ (strcat "\n      -> 产品编号 " tke:samplepre num))
          (princ (strcat "\n      -> 产品料号 " tke:matpre num))
          (tke:push "cno" "客户编号" (strcat tke:custpre num))
          (tke:push "pno" "产品编号" (strcat tke:samplepre num))
          (tke:push "mat" "产品料号" (strcat tke:matpre num))))

      ;; ---- [2/2] 其它字段 ----
      (initget "Y")
      (setq ans (getkword "\n[2/2] 还要改其它字段吗？[回车=不改（只更新料号）/ Y=逐个改]: "))
      (if (and ans (= ans "Y"))
        (progn
          (tke:push "dwg" "图名" (tke:ask "dwg" "图名（客户料号）"))
          (tke:push "mtrl" "材质" (tke:ask "mtrl" "材质"))
          (tke:push "own" "负责人" (tke:ask "own" "负责人（首次发行）"))
          (tke:push "qty" "样品数量" (tke:ask "qty" "样品数量"))
          (tke:push "ver" "版本" (tke:ask "ver" "版本"))
          (tke:push "dat" "图纸日期" (tke:ask "dat" "图纸日期"))
          (tke:push "pg1" "共几页" (tke:ask "pg1" "共几页"))
          (tke:push "pg2" "第几页" (tke:ask "pg2" "第几页"))))

      ;; ---- 预览 + 确认 ----
      (if (null tke:plan)
        (princ "\n      没有任何字段需要修改，未做改动。")
        (progn
          (setq n (length tke:plan))
          (princ "\n      ---------- 将要写入 ----------")
          (foreach p (reverse tke:plan)
            (princ (strcat "\n        " (nth 1 p) " = " (nth 2 p)
                           "   （原 "
                           (if (and (nth 3 p) (/= (nth 3 p) "")) (nth 3 p) "空")
                           "）")))
          (princ "\n      ------------------------------")
          (initget "N")
          (setq ans (getkword "\n      确认写入？[回车=写入 / N=取消]: "))
          (if (and ans (= ans "N"))
            (princ "\n      已取消，未做修改。")
            (progn
              ;; 全部改动合成一步，按一次 U 就能整体撤销
              (setq ug (vl-catch-all-apply 'command (list "_.UNDO" "_BEgin")))
              (foreach p (reverse tke:plan)
                (tke:set (car p) (nth 2 p)))
              (if (not (vl-catch-all-error-p ug))
                (vl-catch-all-apply 'command (list "_.UNDO" "_End")))
              (princ (strcat "\n      已写入 " (itoa n)
                             " 项。按 U 可整体撤销。"))))))))
  (princ))

(defun c:BKE () (c:TKE))
(princ "\nTKE 图框属性编辑器：一次输入料号数字，自动填 客户编号 / 产品编号 / 产品料号 三处（仅样品图）。")
(princ)
