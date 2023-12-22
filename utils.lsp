(defun c:SendToBack ()
  (setq ss (ssget "X" '((8 . "x-lines"))))
  (if ss
    (progn
      (command "_.draworder" ss "" "B")
      (princ "\nEntities moved to back")
    )
    (princ "\nNo elements found on the layer 'x-lines'.")
  )
  (princ)
  )


