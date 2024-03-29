;; Insert circles of a given dimension on points on the current layer
(defun c:geocircles()
  ;; Ask user for radius
  ;; todo: take the radius from the suffix part of the current layer if its selected
  (setq layername (getvar "clayer"))
  ;; Layer name could be somethin like foo-bar-1020... the last part is the default radius
  ;; It must be the last part of the tokenized string
  (setq tokens (strtok layername "-"))
  (setq rad (car (last tokens)))

  ; Convert default value to string and create prompt message
  (setq prompt (strcat "Enter radius [" (rtos rad) "]: "))
  (setq input (getstring prompt))
  (if (null input)
    (setq result rad)
    (setq result (atof input))
  )
  ;; Get selection of points from user
  (setq ptlist (ssget '((0 . "POINT"))))
  ;; todo: default selection to all points on current layer
  (setq pts (ptlist-ss-getpts ptlist))
  ;; store old object snap mode
  (princ pts)
  (setq oldosmode (getvar "osmode"))
  ;; Turn off object snap
  (setvar "osmode" 0)
  ;; loop over all points and insert with radius 0.1

  (foreach pt pts (command "circle" pt (* rad 25.4)))
  ;; reset object snap mode
  (setvar "osmode" oldosmode)
  (geofix)
  (princ))



(defun c:geoin () 
  ;; import the file ~/Downloads/tmp.dxf into the drawing at point 0,0
  (command "_.insert" "/Users/chkelly/Downloads/tmp.dxf" '(0 0) 1 1 0)
  (setvar "pdsize" 2000)
  (setvar "pdmode" 3)
  ; Create layers subs-500, subs-500-circ, region, subs-250, subs-250-circ
  (command "_.layer" "_N" "active-area" "")
  (command "_.layer" "_N" "sea" "")
  (command "_.layer" "_N" "subs-1000" "")
  (command "_.layer" "_N" "subs-1000-circ" "")
  (command "_.layer" "_N" "subs-500" "")
  (command "_.layer" "_N" "subs-500-circ" "")
  (command "_.layer" "_N" "subs-250" "")
  (command "_.layer" "_N" "subs-250-circ" "")
  (command "_.layer" "_N" "region" "" ) 
  ; Explode the selected block
  (command "_.EXPLODE" "LAST")
  ; purge the block
  (command "_.PURGE" "B" "tmp" "N")
  ; regen
  (princ))  

(defun c:geofix() 
  ;; zoom extents
  (command "_.ZOOM" "_E")
  ;; move all circles to back using draworder
  (moveCirclesToBack))

(defun moveCirclesToBack ()
  ; Select all circles in the drawing
  (ssget "X" '((0 . "CIRCLE")))
  ; Apply the DRAWORDER command, moving selected circles to back
  (command "_.DRAWORDER" "" "B")
  (princ))
