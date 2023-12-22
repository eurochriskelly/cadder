(defun c:bb () 
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
  ; Save current snap settings
  (setq oldsnap (getvar "OSMODE"))

  ; Save current layer
  (setq oldLayer (getvar "CLAYER")) 
  (setq layerName "x-lines") ; Layer name

  ; Create layer if it doesn't exist
  (if (not (tblsearch "LAYER" layerName))
    (command "._-LAYER" "N" layerName "" "")) 

  ; Set current layer to 'x-lines'
  (command "._-LAYER" "S" layerName "") 

  ; Ask user to specify the position for x-line
  (setq pt1 (getpoint "\nSpecify the position for x-line "))

  ; Continue asking for points until user cancels or presses Enter
  (while pt1
    (if horizontal
      (setq pt2 (list 0 (cadr pt1) 0)) ; Horizontal line start point
      (setq pt2 (list (car pt1) 0 0)) ; Vertical line start point
    )

    ; Disable snap settings for drawing
    (setvar "OSMODE" 0) 

    ; Draw the xline
    (command "._XLINE" pt1 pt2 "") 

    ; Restore snap settings
    (setvar "OSMODE" oldsnap)

    ; Ask for the next point
    (setq pt1 (getpoint "\nSpecify the position for x-line (Press Enter to finish): "))
  )

  ; Restore previous layer and snap settings
  (setvar "CLAYER" oldLayer) 
  (setvar "OSMODE" oldsnap) 
)



(defun c:hh ()
  (create-line T) ; Horizontal line
)

(defun c:vv ()
  (create-line nil) ; Vertical line
)

(defun c:xx-del (/ layer_name ss)
  (setq layer_name "x-lines")
  
  ; Select all entities on the specified layer
  (setq ss (ssget "X" (list (cons 8 layer_name))))

  ; Check if any entities are found
  (if ss
    (progn
      ; Iterate over the selection set and erase each entity
      (foreach ent (mapcar 'cadr (ssnamex ss))
        (command "_.erase" ent "")
      )
      (princ (strcat "\nDeleted all entities on layer: " layer_name))
    )
    (princ (strcat "\nNo entities found on layer: " layer_name))
  )
  (princ)
)



(princ "\nLoaded utils.lsp")