;;; ZWCAD 2020 drag trim. Distribution file is GBK encoded.
(vl-load-com)

(defun zt:points (raw / sz h w ctr)
  (setq sz (getvar "SCREENSIZE") h (getvar "VIEWSIZE")
        w (* h (/ (car sz) (cadr sz)))
        ctr (trans (getvar "VIEWCTR") 1 2))
  (mapcar '(lambda (p)
    (trans (list (+ (car ctr) (* (- (car p) 0.5) w))
                 (+ (cadr ctr) (* (- (cadr p) 0.5) h))
                 (caddr ctr)) 2 1)) raw))

(defun zt:state (ent / data result sub item)
  (if (setq data (entget ent))
    (progn
      (setq result (list data))
      (if (= (cdr (assoc 0 data)) "POLYLINE")
        (progn
          (setq sub (entnext ent))
          (while (and sub (setq item (entget sub)) (/= (cdr (assoc 0 item)) "SEQEND"))
            (setq result (cons item result) sub (entnext sub)))))
      result)))

(defun zt:unlocked (ent / data layer)
  (and (setq data (entget ent))
       (setq layer (tblsearch "LAYER" (cdr (assoc 8 data))))
       (= 0 (logand 4 (cdr (assoc 70 layer))))))

(defun zt:trim (pts / p ss i ent snapshots item erased)
  ;; Snapshot only editable curve objects actually crossed by this stroke.
  (setq ss (ssget "_F" pts '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,ELLIPSE,SPLINE")))
        erased 0)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (if (zt:unlocked ent)
          (setq snapshots (cons (list ent (zt:state ent)) snapshots))))))
  (command "_.TRIM" "" "_F")
  (foreach p pts (command "_non" p))
  (command "" "")
  ;; Do not erase curves that TRIM already changed, split, or removed.
  (if (= (getvar "CMDACTIVE") 0)
    (foreach item snapshots
      (setq ent (car item))
      (if (and (zt:unlocked ent) (equal (cadr item) (zt:state ent)))
        (if (entdel ent) (setq erased (1+ erased))))))
  (if (> erased 0)
    (princ (strcat "\n已删除 " (itoa erased) " 个未被修剪的完整线/曲线对象。")))
  erased)

(defun zt:ensure () (zwk:ready))
(defun zt:capture (w h / r) (setq r (zwk:bridge "ZWKCAP102")) (if (and (listp r) (listp (car r)) (equal (caar r) "TRIM")) (setq r (list (cdar r)))) r)
(defun zt:run (preview capturefn / *error* echo snap doc mark count go sz raw pts p previous result)
  (setq echo (getvar "CMDECHO") snap (getvar "OSMODE")
        doc (vla-get-ActiveDocument (vlax-get-acad-object)) count 0)
  (defun *error* (msg)
    (if mark (progn (vla-EndUndoMark doc) (setq mark nil)))
    (setvar "CMDECHO" echo) (setvar "OSMODE" snap) (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n拖动修剪提示：" msg)))
    (princ))
  (cond
    ((not (zt:ensure))
      (princ "\n鼠标模块未通过检查，已停止。请加载本版 DragTrimV16.dll 后重试。"))
    ((or (/= (getvar "TILEMODE") 1) (not (member (getvar "PERSPECTIVE") '(nil 0)))
         (> (abs (car (getvar "VIEWDIR"))) 1e-8)
         (> (abs (cadr (getvar "VIEWDIR"))) 1e-8)
         (<= (caddr (getvar "VIEWDIR")) 0.0))
      (princ "\n本版支持模型空间的二维平面视图，请在当前 UCS 的俯视平面下使用。"))
    (T
      (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
      (setq go T)
      (if preview
        (princ "\n轨迹校验：按住左键画一笔，松开后显示转换后的轨迹，不修改图形。")
        (princ "\n拖动修剪：划过后无法修剪的线/曲线将整对象删除；U 可撤销本笔。白色准星中心点即鼠标落点。按住左键划过多余线段，松开执行；可连续划。滚轮缩放，中键拖动平移。U 撤销上一笔，Esc/回车结束。"))
      (while go
        (setq sz (getvar "SCREENSIZE")
              raw (vl-catch-all-apply capturefn (list (fix (car sz)) (fix (cadr sz)))))
        ;; ZWCAD 2020 wraps the .NET ResultBuffer in one additional list.
        (if (and (listp raw) (= (length raw) 1)) (setq raw (car raw)))
        (cond
          ((vl-catch-all-error-p raw)
            (princ (strcat "\n捕捉模块错误：" (vl-catch-all-error-message raw))) (setq go nil))
          ((equal raw "UNDO")
            (if (> count 0)
              (progn (command "_.UNDO" 1) (setq count (1- count)) (princ "\n已撤销上一笔。"))
              (princ "\n本次命令尚无可撤销的修剪。")))
          ((or (equal raw "CANCEL") (equal raw "ERROR") (null raw)) (setq go nil))
          ((equal raw "EMPTY") (princ "\n请按住左键移动一段距离，再松开。"))
          ((and (listp raw) (listp (car raw)) (numberp (caar raw)))
            (setq pts (zt:points raw))
            (if preview
              (progn
                (foreach p pts
                  (if previous (grdraw previous p 1 1))
                  (setq previous p))
                (princ "\n红色轨迹应与刚才拖动位置重合。按回车清除；若偏移，请勿运行 TR。")
                (getstring) (redraw) (setq go nil))
              (progn
                (vla-StartUndoMark doc) (setq mark T)
                (zt:trim pts)
                (vla-EndUndoMark doc) (setq mark nil count (1+ count))
                (princ "\n本笔修剪及整对象删除已完成。继续拖动；U 撤销；Esc 结束。"))))
          (T (princ "\n捕捉模块返回格式不符合预期，已停止。") (setq go nil))))
      (setvar "CMDECHO" echo) (setvar "OSMODE" snap) (redraw)))
  (princ))

(defun c:TR () (zt:run nil 'zt:capture))
(defun c:ZT () (zt:run nil 'zt:capture))
(defun c:ZTY () (zt:run T 'zt:capture))
(princ "\nTR 拖动修剪已就绪。")
(princ)
