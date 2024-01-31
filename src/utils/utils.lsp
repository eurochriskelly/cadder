(defun C:HS () 
  (progn
    (princ "\nSelect object to hatch: ")
    (command "._hatch" "p" "solid" "s" (car (entsel)) "") 
    (princ "change draw order: ")s
    (command "._draworder" (entlast) "" "")
    (princ)
  )
)