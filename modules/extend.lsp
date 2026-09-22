;;; ZWCAD 2020 drag trim. Distribution file is GBK encoded.
(vl-load-com)


(defun zx:points (raw / sz h w ctr)
  (setq sz (getvar "SCREENSIZE") h (getvar "VIEWSIZE")
        w (* h (/ (car sz) (cadr sz)))
        ctr (trans (getvar "VIEWCTR") 1 2))
  (mapcar '(lambda (p)
    (trans (list (+ (car ctr) (* (- (car p) 0.5) w))
                 (+ (cadr ctr) (* (- (cadr p) 0.5) h))
                 (caddr ctr)) 2 1)) raw))

(defun zx:apply (pts trimMode / p)
  (princ (if trimMode "\nShift 修剪。" "\n延伸。"))
  (if (and trimMode (> (length pts) 1))
    (zt:trim pts)
    (progn
  ;; 边界给真实选择集（zt:bounds 在 trim.lsp，boot.lsp 保证 extend 启用时 trim.lsp 一定加载）；
  ;; 传 "" 在 ZWCAD 上什么都不延伸（1.2.31 实测）。
  (command (if trimMode "_.TRIM" "_.EXTEND") (zt:bounds) "")
  (if (= (length pts) 1)
    (command "_non" (car pts))
    (progn
      (command "_F")
      (foreach p pts (command "_non" p))
      (command "")))
  (command ""))))
(defun zx:ensure () (zwk:ready))
(defun zx:capture (w h) (zwk:bridge "ZWKCAP102"))
(defun zx:run (preview capturefn / *error* echo snap doc mark count go sz raw pts p previous result trimMode)
  (setq echo (getvar "CMDECHO") snap (getvar "OSMODE")
        doc (vla-get-ActiveDocument (vlax-get-acad-object)) count 0)
  (defun *error* (msg)
    (if mark (progn (vla-EndUndoMark doc) (setq mark nil)))
    (setvar "CMDECHO" echo) (setvar "OSMODE" snap) (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n拖动延伸提示：" msg)))
    (princ))
  (cond
    ((not (zx:ensure))
      (princ "\n整合鼠标模块未通过检查。请退出命令后双击工具包的一键安装.cmd，查看 install.log。"))
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
        
(princ "\n拖动延伸：默认延伸；按住 Shift 单击或拖动修剪；本笔按过 Shift 就按修剪处理。自动使用全部对象作为边界；单击靠近待延伸的一端，或拖动划过多条线。白色准星中心点即鼠标落点。按住左键划过多余线段，松开执行；可连续划。滚轮缩放，中键拖动平移。U 撤销上一笔，Esc/回车结束。"))
      (while go
        (setq sz (getvar "SCREENSIZE")
              raw (vl-catch-all-apply capturefn (list (fix (car sz)) (fix (cadr sz)))))
        ;; ZWCAD 2020 wraps the .NET ResultBuffer in one additional list.
        (if (and (listp raw) (= (length raw) 1)) (setq raw (car raw)))
        (setq trimMode (and (listp raw) (equal (car raw) "TRIM")))
        (if trimMode (setq raw (cdr raw)))
        (cond
          ((vl-catch-all-error-p raw)
            (princ (strcat "\n捕捉模块错误：" (vl-catch-all-error-message raw))) (setq go nil))
          ((equal raw "UNDO")
            (if (> count 0)
              (progn (command "_.UNDO" 1) (setq count (1- count)) (princ "\n已撤销上一笔。"))
              (princ "\n本次命令尚无可撤销的延伸。")))
          ((or (equal raw "CANCEL") (equal raw "ERROR") (null raw)) (setq go nil))
          ((equal raw "EMPTY") (princ "\n请单击线段或按住左键划过线段。"))
          ((and (listp raw) (listp (car raw)) (numberp (caar raw)))
            (setq pts (zx:points raw))
            (if preview
              (progn
                (foreach p pts
                  (if previous (grdraw previous p 1 1))
                  (setq previous p))
                (princ "\n红色轨迹应与刚才拖动位置重合。按回车清除；若偏移，请勿运行 EX。")
                (getstring) (redraw) (setq go nil))
              (progn
                (vla-StartUndoMark doc) (setq mark T)
                (zx:apply pts trimMode)
                (vla-EndUndoMark doc) (setq mark nil count (1+ count))
                (princ "\n本笔处理已完成。继续拖动；U 撤销；Esc 结束。"))))
          (T (princ "\n捕捉模块返回格式不符合预期，已停止。") (setq go nil))))
      (setvar "CMDECHO" echo) (setvar "OSMODE" snap) (redraw)))
  (princ))

(defun c:EX () (zx:run nil 'zx:capture))
;; EXD = 拖动延伸。同 TRD：ZWCAD 不认值别名，必须真 defun。
(defun c:EXD () (zx:run nil 'zx:capture))
(defun c:EXY () (zx:run T 'zx:capture))
(princ "\nEXD 拖动延伸已加载：单击或拖动延伸，按住 Shift 修剪；EXY 仅校验轨迹。")
(princ)
