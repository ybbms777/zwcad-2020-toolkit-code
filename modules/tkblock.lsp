;;; TKE: 图框属性编辑器。GBK 编码。
;;; 只针对【样品图】的图框，量产图以后单独做。
;;;
;;; 主要解决：公司料号要在三个地方输入 —— 客户编号 KY-xxx、产品编号 样品-xxx、
;;; 产品料号 01000xxx，三处数字相同，输多了容易漏改。这里只输一次数字。
;;;
;;; 前几版踩过的坑，别改回去：
;;;  1) 这个图框里「产品编号」和「客户编号」两个属性的标记都叫「编号」，
;;;     只按标记找会找错。做法：先用值前缀认（KY* / 样品*），认不出来再按出现次序取。
;;;  2) ★ 读块属性不要用 ActiveX 的 vla-get-attributes ★
;;;     用户实测：ZWCAD 2020 上这个方法一个属性都读不出来（块名认得对，属性数=0），
;;;     直接导致「没找到图框」。改成纯 DXF：属性就排在 INSERT 后面，
;;;     用 (entnext) 一路走，遇到非 ATTRIB 停 —— 不依赖任何 ActiveX 方法。
;;;  3) ssget "X" 不要加 (66 . 1)「有属性」过滤，ZWCAD 上一个块都选不到。
;;;     只过滤 (0 . "INSERT")，再自己判断。
;;;  4) ssget "X" 只搜当前空间，要按 (410 . 布局名) 逐个布局搜。
;;;  5) 不要用 vla-get-hasattributes 当门槛（部分 CAD 取不到，会把全部块判成没属性）。
;;;  6) 不要用 let —— 标准 AutoLISP 没有这个宏，只用 setq + defun 局部变量。
;;;  7) getkword 按回车返回 nil，不能直接 (= nil "Y") 比较，必须先判 null。
;;;  8) entsel 配 initget 时用户输关键字会返回「字符串」，比较前要 (type r) 判一下。
;;;  9) 提示里不要用宽字符框线和中文对齐填充，cmd.exe 里会错位。

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

(setq tke:frames nil)    ;; ((块参照 . 属性表) ...)，按 X 从左到右排好
(setq tke:allrefs nil)   ;; 图纸里全部块参照（点选兜底用）
(setq tke:list nil)      ;; 第一个图框的属性表 ((标记 当前值 属性实体) ...)
(setq tke:plan nil)      ;; ((键 显示名 新值 原值) ...)
(setq tke:multi nil)     ;; T = 本次是多个图框一起改
(setq tke:how nil)       ;; 属性是怎么读出来的（诊断用）
(setq tke:why nil)       ;; 读不出属性的原因（诊断用）

;; ---------- 路径 1：纯 DXF，entnext 走 INSERT 后面的 ATTRIB ----------
(defun tke:attrs-dxf (ent / e out tg tx tp)
  (setq out nil)
  (setq e (entnext ent))
  (setq tp (if e (cdr (assoc 0 (entget e))) nil))
  (while (and e (= tp "ATTRIB"))
    (setq tg (cdr (assoc 2 (entget e))))
    (setq tx (cdr (assoc 1 (entget e))))
    (if (null tg) (setq tg ""))
    (if (null tx) (setq tx ""))
    (setq out (cons (list tg tx e) out))
    (setq e (entnext e))
    (setq tp (if e (cdr (assoc 0 (entget e))) nil)))
  (reverse out))

;; ---------- 路径 2：全图找 ATTRIB，按「宿主块」匹配（330 组码）----------
(defun tke:attrs-owner (ent / ss i e out tg tx own)
  (setq out nil)
  (setq ss (vl-catch-all-apply 'ssget (list "X" '((0 . "ATTRIB")))))
  (if (vl-catch-all-error-p ss) (setq ss nil))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq own (cdr (assoc 330 (entget e))))
        (if (or (eq own ent) (equal own ent))
          (progn
            (setq tg (cdr (assoc 2 (entget e))))
            (setq tx (cdr (assoc 1 (entget e))))
            (if (null tg) (setq tg ""))
            (if (null tx) (setq tx ""))
            (setq out (cons (list tg tx e) out))))
        (setq i (1+ i)))))
  (reverse out))

;; ---------- 路径 3：ActiveX（备用，ZWCAD 上实测读不出来）----------
(defun tke:attrs-vla (ent / obj arr out tag txt)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    nil
    (progn
      (setq arr (vl-catch-all-apply 'vla-get-attributes (list obj)))
      (if (vl-catch-all-error-p arr)
        nil
        (progn
          (setq arr (vl-catch-all-apply 'vlax-safearray->list
                      (list (vl-catch-all-apply 'vlax-variant-value (list arr)))))
          (if (or (vl-catch-all-error-p arr) (null arr))
            nil
            (progn
              (setq out nil)
              (foreach a arr
                (setq tag (vl-catch-all-apply 'vla-get-tagstring (list a)))
                (if (vl-catch-all-error-p tag) (setq tag ""))
                (if (null tag) (setq tag ""))
                (setq txt (vl-catch-all-apply 'vla-get-textstring (list a)))
                (if (vl-catch-all-error-p txt) (setq txt ""))
                (if (null txt) (setq txt ""))
                (setq out (cons (list tag txt a) out)))
              (reverse out))))))))

;; ---------- 读属性：三条路依次试，成功就记下来 ----------
(defun tke:attrs (ent / L)
  (setq L (vl-catch-all-apply 'tke:attrs-dxf (list ent)))
  (if (vl-catch-all-error-p L) (setq L nil))
  (if L
    (progn (setq tke:how "DXF") L)
    (progn
      (setq L (vl-catch-all-apply 'tke:attrs-owner (list ent)))
      (if (vl-catch-all-error-p L) (setq L nil))
      (if L
        (progn (setq tke:how "全图匹配") L)
        (progn
          (setq L (vl-catch-all-apply 'tke:attrs-vla (list ent)))
          (if (vl-catch-all-error-p L) (setq L nil))
          (if L (setq tke:how "ActiveX"))
          L)))))

;; ---------- 读不出属性时，记一条原因 ----------
(defun tke:why-str (ent / d e tp g66)
  (setq d (entget ent))
  (setq g66 (cdr (assoc 66 d)))
  (setq e (entnext ent))
  (setq tp (if e (cdr (assoc 0 (entget e))) "（后面没有实体）"))
  (strcat "块 " (tke:blkname ent) "：66组=" (if g66 (itoa g66) "无")
          "，后面第一个实体=" tp))

(defun tke:blkname (ent / n)
  (setq n (cdr (assoc 2 (entget ent))))
  (if n n "?"))

;; 属性表里有没有「料号」或「图名」——判断是不是图框块
(defun tke:is-frame (L)
  (and L
       (or (member "料号" (mapcar 'car L))
           (member "图名" (mapcar 'car L)))))

;; 块的插入点（DXF 组码 10）。用它判左右顺序，不用 ActiveX 取包围盒。
(defun tke:ipt (ent / p)
  (setq p (cdr (assoc 10 (entget ent))))
  (if p p '(0.0 0.0 0.0)))

(defun tke:cx (ent) (car (tke:ipt ent)))

(defun tke:del-nth (L n / out i)
  (setq out nil i 0)
  (foreach e L
    (if (/= i n) (setq out (cons e out)))
    (setq i (1+ i)))
  (reverse out))

;; 按从左到右排序（插入排序，图框数量很少够用）
(defun tke:sortx (L / out e best bi k bx)
  (setq out nil)
  (while L
    (setq best nil bi 0 k 0 bx nil)
    (foreach e L
      (if (or (null bx) (< (tke:cx (car e)) bx))
        (setq bx (tke:cx (car e)) best e bi k))
      (setq k (1+ k)))
    (setq out (cons best out))
    (setq L (tke:del-nth L bi)))
  (reverse out))

;; ---------- 扫描全部布局里的块参照，找出所有图框 ----------
(defun tke:scan (/ doc lays n i lo nm ss j ent L hits nblk nattr tags)
  (setq hits nil tke:allrefs nil nblk 0 nattr 0 tags nil tke:why nil tke:how nil)
  (setq doc (vl-catch-all-apply 'vla-get-activedocument
              (list (vl-catch-all-apply 'vlax-get-acad-object))))
  (if (vl-catch-all-error-p doc) (setq doc nil))
  (setq lays nil)
  (if doc
    (progn
      (setq lays (vl-catch-all-apply 'vla-get-layouts (list doc)))
      (if (vl-catch-all-error-p lays) (setq lays nil))))
  (if lays
    (progn
      (setq n (vl-catch-all-apply 'vla-get-count (list lays)))
      (if (vl-catch-all-error-p n) (setq n 0))
      (setq i 0)
      (while (< i n)
        (setq lo (vl-catch-all-apply 'vla-item (list lays i)))
        (if (not (vl-catch-all-error-p lo))
          (progn
            (setq nm (vl-catch-all-apply 'vla-get-name (list lo)))
            (if (not (vl-catch-all-error-p nm))
              (progn
                (setq ss (vl-catch-all-apply 'ssget
                           (list "X" (list '(0 . "INSERT") (cons 410 nm)))))
                (if (vl-catch-all-error-p ss) (setq ss nil))
                (if ss
                  (progn
                    (setq j 0)
                    (while (< j (sslength ss))
                      (setq ent (ssname ss j))
                      (setq nblk (1+ nblk))
                      (setq tke:allrefs (cons ent tke:allrefs))
                      (setq L (tke:attrs ent))
                      (if L
                        (progn
                          (setq nattr (1+ nattr))
                          (if (null tags) (setq tags (mapcar 'car L)))
                          (if (tke:is-frame L)
                            (setq hits (cons (cons ent L) hits))))
                        (if (null tke:why) (setq tke:why (tke:why-str ent))))
                      (setq j (1+ j)))))))))
        (setq i (1+ i)))))
  (princ (strcat "\n      扫描：块参照 " (itoa nblk) " 个，带属性 " (itoa nattr)
                 " 个，认出图框 " (itoa (length hits)) " 个"))
  (if (and tke:how (> nattr 0))
    (princ (strcat "（属性读法：" tke:how "）")))
  (if (= nattr 0)
    (progn
      (princ "\n      提示：这 " (itoa nblk) " 个块都没读出属性。")
      (if tke:why (princ (strcat "\n      " tke:why)))))
  (if (and (> nattr 0) (null hits) tags)
    (progn
      (princ "\n      带属性块里的标记：")
      (setq i 0)
      (foreach tg tags
        (if (< i 14) (princ (strcat " " tg)))
        (setq i (1+ i)))
      (princ "\n      提示：里面没有「料号」或「图名」，图框模板和预期不一致。")))
  (tke:sortx hits))

;; ---------- 手动点选（点属性文字 / 点框线都能认出来） ----------
(defun tke:pick (/ r ent tp e2 best bd pt ip d)
  (initget "A")
  (setq r (entsel "\n      点选图框（点在块上；A=取消）: "))
  (cond
    ((null r) nil)
    ((= (type r) 'STR) nil)
    (T
     (progn
       (setq ent (car r) tp (cadr r))
       (setq e2 (entget ent))
       ;; 点到属性文字 -> 用它所属的块
       (if (= (cdr (assoc 0 e2)) "ATTRIB")
         (setq ent (cdr (assoc 330 e2))))
       ;; 还不是块 -> 找插入点离点击点最近的那个块
       (if (or (null ent) (null (entget ent))
               (/= (cdr (assoc 0 (entget ent))) "INSERT"))
         (progn
           (setq pt (trans tp 1 0) best nil bd nil)
           (foreach e tke:allrefs
             (setq ip (tke:ipt e))
             (setq d (+ (abs (- (car pt) (car ip))) (abs (- (cadr pt) (cadr ip)))))
             (if (or (null bd) (< d bd)) (setq bd d best e)))
           (setq ent best)))
       (if (tke:attrs ent) ent nil)))))

;; ---------- 角色 -> 属性下标 ----------
;; 同一标记可能有多个属性（「编号」有两处），先用值前缀认，认不出来再按出现次序取
(defun tke:idx (L tag hint occ / cands i k e)
  (setq cands nil i 0)
  (foreach e L
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
                   (wcmatch (strcase (cadr (nth i L)))
                            (strcat (strcase hint) "*")))
            (setq k i))))
      (if (and (null k) (<= occ (length cands)))
        (setq k (nth (1- occ) cands)))
      k)))

;; 写一个属性的值：属性实体是实体名就走 entmod，是 VLA 对象就走 vla-put
(defun tke:putval (e val / d a)
  (if (= (type e) 'VLA-OBJECT)
    (vl-catch-all-apply 'vla-put-textstring (list e val))
    (progn
      (setq d (entget e))
      (if (null d)
        nil
        (progn
          (setq a (assoc 1 d))
          (if a
            (entmod (subst (cons 1 val) a d))
            (entmod (append d (list (cons 1 val))))))))))

(defun tke:get1 (L role / r k)
  (setq r (assoc role tke:roles))
  (if (and r L)
    (progn
      (setq k (tke:idx L (nth 2 r) (nth 3 r) (nth 4 r)))
      (if k (cadr (nth k L)) nil))
    nil))

(defun tke:set1 (L role val / r k e)
  (setq r (assoc role tke:roles))
  (if (and r L)
    (progn
      (setq k (tke:idx L (nth 2 r) (nth 3 r) (nth 4 r)))
      (if k
        (progn
          (setq e (nth k L))
          (tke:putval (nth 2 e) val)
          (setcar (cdr e) val)
          T)
        nil))
    nil))

;; 读：以第一个图框为准；写：所有图框都写
(defun tke:get (role) (tke:get1 tke:list role))

(defun tke:set (role val / n)
  (setq n 0)
  (foreach f tke:frames
    (if (tke:set1 (cdr f) role val) (setq n (1+ n))))
  n)

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
(defun tke:dump (/ old i)
  (setq i 1)
  (foreach f tke:frames
    (princ (strcat "\n      图框 " (itoa i) "：" (tke:blkname (car f))
                   "（" (itoa (length (cdr f))) " 个属性）"))
    (setq i (1+ i)))
  (princ "\n      当前值（以第 1 个图框为准）：")
  (foreach r tke:roles
    (setq old (tke:get (car r)))
    (princ (strcat "\n        " (nth 1 r) " = "
                   (if (and old (/= old "")) old "（空）")))))

;; ---------- 多图框时：共几页 / 第几页 按左右顺序自动编号 ----------
(defun tke:autopage (/ n i)
  (setq n (length tke:frames) i 1)
  (foreach f (reverse tke:frames)
    (tke:set1 (cdr f) "pg1" (itoa n))
    (tke:set1 (cdr f) "pg2" (itoa i))
    (setq i (1+ i))))

;; ---------- 主命令 ----------
(defun c:TKE (/ hits r ans num n old ug)
  (princ "\n")
  (princ "\n  ============== 图框属性编辑器（样品图） ==============")
  (princ "\n  只针对【样品图】的图框，量产图暂不支持。")
  (princ "\n  料号只输一次数字，自动填三处：")
  (princ "\n     客户编号 KY-xxx   产品编号 样品-xxx   产品料号 01000xxx")
  (princ "\n  每一项直接回车 = 保持原值不变；Esc 随时取消。")
  (princ "\n  =====================================================")
  (setq hits (tke:scan))
  (setq tke:frames hits)
  (setq n (length hits))
  (cond
    ((= n 0)
     (princ "\n      没找到图框，改为点选: ")
     (setq r (tke:pick))
     (if r (setq hits (list (cons r (tke:attrs r))))))
    ((> n 1)
     (princ (strcat "\n      找到 " (itoa n) " 个图框。"))
     (princ "\n      [回车=全部一起改 / 点选=只改点中的那个 / A=取消]: ")
     (setq r (tke:pick))
     (if r
       (progn
         (princ "\n      只改点中的这一个。")
         (setq hits (list (cons r (tke:attrs r)))))
       (princ (strcat "\n      全部 " (itoa n) " 个图框一起改。")))))
  (if (or (null hits) (= (length hits) 0))
    (princ "\n已取消（没选到图框）。")
    (progn
      (setq tke:frames hits)
      (setq tke:multi (> (length hits) 1))
      (setq tke:list (cdr (car hits)))
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
          (if tke:multi
            (princ "\n      多个图框：共几页 / 第几页 将按左右顺序自动编号，不再单独问。")
            (progn
              (tke:push "pg1" "共几页" (tke:ask "pg1" "共几页"))
              (tke:push "pg2" "第几页" (tke:ask "pg2" "第几页"))))))

      ;; ---- 预览 + 确认 ----
      (if (and (null tke:plan) (not tke:multi))
        (princ "\n      没有任何字段需要修改，未做改动。")
        (progn
          (setq n (length tke:plan))
          (princ "\n      ---------- 将要写入 ----------")
          (foreach p (reverse tke:plan)
            (princ (strcat "\n        " (nth 1 p) " = " (nth 2 p)
                           "   （原 "
                           (if (and (nth 3 p) (/= (nth 3 p) "")) (nth 3 p) "空")
                           "）")))
          (if tke:multi
            (progn
              (princ (strcat "\n        共几页 = " (itoa (length tke:frames))))
              (princ (strcat "\n        第几页 = 按左右顺序自动编号 1 ~ "
                             (itoa (length tke:frames))))))
          (princ "\n      ------------------------------")
          (if tke:multi
            (princ (strcat "\n      将写入 " (itoa (length tke:frames)) " 个图框。")))
          (initget "N")
          (setq ans (getkword "\n      确认写入？[回车=写入 / N=取消]: "))
          (if (and ans (= ans "N"))
            (princ "\n      已取消，未做修改。")
            (progn
              ;; 全部改动合成一步，按一次 U 就能整体撤销
              (setq ug (vl-catch-all-apply 'command (list "_.UNDO" "_BEgin")))
              (foreach p (reverse tke:plan)
                (tke:set (car p) (nth 2 p)))
              (if tke:multi (tke:autopage))
              (if (not (vl-catch-all-error-p ug))
                (vl-catch-all-apply 'command (list "_.UNDO" "_End")))
              (princ (strcat "\n      已写入 " (itoa n) " 项"
                             (if tke:multi
                               (strcat "，共 " (itoa (length tke:frames)) " 个图框") "")
                             "。按 U 可整体撤销。"))))))))
  (princ))

(defun c:BKE () (c:TKE))
(princ "\nTKE 图框属性编辑器：一次输入料号数字，自动填 客户编号 / 产品编号 / 产品料号 三处（仅样品图）。")
(princ)
