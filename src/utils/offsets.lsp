;;Handy offset shortcuts
(defun offset (dist ent pt)
  (setq oldoffsetdist (getvar "offsetdist"))
  (command "._offset" dist ent pt "")
  (setvar "offsetdist" oldoffsetdist)
)

;; GENERAL OFFSET COMMANDS
; Define the main offset function
(defun offset-by-distance (distance)
  (if distance
    (progn
      ; Perform the offset
      (offset distance (car (entsel)) (getpoint "\nPick point on side"))

      ; Get the last created object (the offset object)
      (setq lastObj (entlast))

      ; Change the layer of the last created object to current layer
      (if lastObj
        (progn
          (setq currentLayer (getvar "clayer"))
          (setq objData (entget lastObj))
          (setq objData (subst (cons 8 currentLayer) (assoc 8 objData) objData))
          (entmod objData)
        )
      )

      (princ)
    )
  )
)

; List of allowed distances
(setq distances '(5 10 20 25 30 75 50 100 150 200 250 300 350 400 450 500 550 600 750 900 1000 1500 1600 1800 2000 2500))

; Loop through the distances to create commands
(foreach dist distances
  (eval
    (list 'defun
          (read (strcat "C:o" (itoa dist)))
          (list)
          (list 'offset-by-distance dist)
    )
  )
)

(defun C:ot () (command "offset" "t" pause))
