;;; GC: native geometric tolerance shortcut. GBK encoded.
;;; Opens the built-in TOLERANCE dialog; place frames, double-click to edit.
(vl-load-com)

(defun c:GC (/ *error*)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*取消*,*退出*")))
      (princ (strcat "\n提示：" msg)))
    (princ))
  (princ "\n几何公差：选符号填数值后点位置放置，可连放，回车结束。")
  (command "_.TOLERANCE")
  (while (> (getvar "CMDACTIVE") 0) (command pause))
  (princ))

(princ "\n几何公差快捷已加载：GC 打开原生公差框。")
(princ)
