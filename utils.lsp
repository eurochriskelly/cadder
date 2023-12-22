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

;; function c:hh draws a horizontal line on the layer 'x-lines'
;; and moves it to the back
;; The user clicks one point to define where the y position of the line should be
;; The first point x-position should be 0
;; The second pont x-position should be -10
;; The construction line is drawn from the first point to the second point
(defun create-line (horizontal)
  (setq pt1 (getpoint "\nSpecify the start point: "))
  
  (if horizontal
    (setq pt2 (list (+ (car pt1) 10) (cdr pt1))) ; Horizontal line
    (setq pt2 (list (car pt1) (- (cdr pt1) 10))) ; Vertical line
  )
  
  (setq layerName "x-lines") ; Layer name
  (if (not (tblsearch "LAYER" layerName))
    (command "._-LAYER" "M" layerName "" "C" "xlines")) ; Create layer if it doesn't exist
    
  (command "_LAYER" "M" layerName) ; Set current layer to 'x-lines'
  
  (command "_.LINE" pt1 pt2 "") ; Draw the line
  (command "._-HIDEOBJECT" (entlast)) ; Hide the line
)

(defun c:hh ()
  (create-line T) ; Horizontal line
)

(defun c:vv ()
  (create-line nil) ; Vertical line
)



