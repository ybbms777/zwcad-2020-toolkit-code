;;; Click-click fence trim/extend/erase (AutoCAD-like). GBK encoded.
;;; TR/TRC: click A, move, click B, the fence trims at B. No holding.
;;; EX/EXC: click A, move, click B, the fence extends at B.
;;; FE: click A, move, click B, crossed curves are erased whole.
;;; Drag versions stay available: ZT = drag trim, EXD = drag extend.
(vl-load-com)

(defun click:fence (cmd tip / *error* echo snap bounds p1 p2)
  (setq echo (getvar "CMDECHO") snap (getvar "OSMODE"))
  (defun *error* (msg)
    (setvar "CMDECHO" echo) (setvar "OSMODE" snap)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n提示：" msg)))
    (princ))
  (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
  (princ tip)
  ;; Explicit boundary set: do not rely on Enter-selects-all.
  (setq bounds (ssget "_X"))
  (if (null bounds)
    (princ "\n图中没有对象。")
    (while (setq p1 (getpoint "\n指定栏选第一点 A <回车结束>："))
      (setq p2 (getpoint p1 "\n指定栏选第二点 B："))
      (if p2
        (progn
          (command cmd bounds "" "_F" "_non" p1 "_non" p2 "" "")
          (princ "\n已执行一笔，回车结束或继续。")))))
  (setvar "CMDECHO" echo) (setvar "OSMODE" snap)
  (princ))

(defun click:trim ()
  (click:fence "_.TRIM" "\n点选修剪：先点 A，再移鼠标点 B，B 点落下即剪；回车结束。"))
(defun click:extend ()
  (click:fence "_.EXTEND" "\n点选延伸：先点 A，再移鼠标点 B，B 点落下即延；回车结束。"))

(defun c:FE (/ *error* echo snap doc mark p1 p2 ss i ent n)
  (setq echo (getvar "CMDECHO") snap (getvar "OSMODE")
        doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if mark (progn (vla-EndUndoMark doc) (setq mark nil)))
    (setvar "CMDECHO" echo) (setvar "OSMODE" snap)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n提示：" msg)))
    (princ))
  (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
  (princ "\n栏选删除：点 A，移鼠标点 B，穿过的线整根删除；U 可撤销，回车结束。")
  (while (setq p1 (getpoint "\n指定栏选第一点 A <回车结束>："))
    (setq p2 (getpoint p1 "\n指定栏选第二点 B："))
    (if p2
      (progn
        (setq ss (ssget "_F" (list p1 p2)
                        '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,ELLIPSE,SPLINE"))))
        (if ss
          (progn
            (vla-StartUndoMark doc) (setq mark T n 0 i 0)
            (repeat (sslength ss)
              (setq ent (ssname ss i) i (1+ i))
              (if (entdel ent) (setq n (1+ n))))
            (vla-EndUndoMark doc) (setq mark nil)
            (princ (strcat "\n已删除 " (itoa n) " 个对象。")))
          (princ "\n栏选没有穿过可删除的线。")))))
  (setvar "CMDECHO" echo) (setvar "OSMODE" snap)
  (princ))

;; Keep the drag versions before overriding (ZT already covers drag trim).
(if (and c:TR (not c:TRD)) (setq c:TRD c:TR))
(if (and c:EX (not c:EXD)) (setq c:EXD c:EX))
(defun c:TR () (click:trim))
(defun c:TRC () (click:trim))
(defun c:EX () (click:extend))
(defun c:EXC () (click:extend))

(princ "\n点选式已加载：TR/TRC 修剪，EX/EXC 延伸，FE 栏选删除；ZT/TRD 拖动修剪，EXD 拖动延伸。")
(princ)
