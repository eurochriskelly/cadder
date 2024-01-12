;; -------------------------------
;;     ACAD DRAWING FUNCTIONS     
;;       - by Chris Kelly         
;; -------------------------------
;; Copyright © MBK Associates 2000
;; 
;; Description: Wrapper functions for MBK Drawings
;;

;; Set the support path use (getvar"ACADPREFIX") to get the previous path so as not to override existing completely use \\
;;;(defun AddSupportPath (newpath)
;;;  (PutSupportPath (getvar"ACADPREFIX") ";" newpath)
;;;)
;;;(defun PutSupportPath ( Path ) 
;;;   (vla-put-SupportPath 
;;;         (vla-get-Preferences 
;;;            (vlax-get-acad-object) 
;;;      (vla-get-Files 
;;;         ) 
;;;      )
;;;     
;;;      Path 
;;;   ) 
;;;) 
;<...............................................................................................................>
; DESCRIPTION: Draws a circle on a given layer
(defun MakeCircle (ptCentre radius layer) 
  (setq oc (vla-addcircle (current-tab) (vlax-3d-point ptCentre) radius))
  (vla-put-layer oc layer)
)
;<...............................................................................................................>
; DESCRIPTION: Makes a line on a given layer
(defun MakeLine (pt1 pt2 layer / ol) 
  (setq ol (vla-addline (current-tab) (vlax-3d-point pt1) (vlax-3d-point pt2)))
  (if layer (vla-put-layer ol layer))
)
;<................................................................................................................>
; DESCRIPTION: Makes a layer of a give name, takes a string as layername and number for color. Linetype is continuous if field is nil
(defun MakeLayer (layername color linetype) 
  (if (not (layer-ExistP layername)) 
    (progn 
      (if linetype 
        (if (= nil (tblsearch "ltype" linetype))  ;make sure the line type exists if not create
          (LoadLineTypes (list linetype) "acad.lin")
        )
      )
      (setq ol (vla-add (vla-get-Layers (ACTIVE-DOCUMENT)) layername))
      (if color (vla-put-color ol color))
    )
  )
)

;<...............................................................................................................>
; DESCRIPTION: loads a list of named linetypes. Usage: (LoadLinetypes '("dashed" "dotted" "sewer") "aclt.lin")
(defun LoadLinetypes (ltList ltFile) 
  (setvar "FILEDIA" 0)
  (setq ii 0)

  (foreach ltype ltList 
    (command "._purge" "LT" ltype "n")
    (command ".-linetype" "load" ltype ltFile "")
  )

  (setvar "FILEDIA" 1)
)
;<...............................................................................................................>
; DESCRIPTION: 
(defun MakeText (content height justify insertion rotation layer / otext dontmove) 
  (setq otext (vla-addtext (model-space) content (vlax-3d-point insertion) height))
  (vla-put-rotation otext rotation)
  (vla-put-layer otext layer)
  (cond 
    ((= justify "ALIGN")
     (vla-put-alignment otext acalignmentaligned)
     (setq dontmove t)
    )
    ((= justify "FIT") (vla-put-alignment otext acalignmentfit) (setq dontmove t))
    ((= justify "CENTER") (vla-put-alignment otext acalignmentcenter))
    ((= justify "MIDDLE") (vla-put-alignment otext acalignmentmiddle))
    ((= justify "RIGHT") (vla-put-alignment otext acalignmentright))
    ((= justify "TL") (vla-put-alignment otext acalignmenttopleft))
    ((= justify "TC") (vla-put-alignment otext acalignmenttopcenter))
    ((= justify "TR") (vla-put-alignment otext acalignmenttopright))
    ((= justify "ML") (vla-put-alignment otext acalignmentmiddleleft))
    ((= justify "MC") (vla-put-alignment otext acalignmentmiddlecenter))
    ((= justify "MR") (vla-put-alignment otext acalignmentmiddleright))
    ((= justify "BL") (vla-put-alignment otext acalignmentbottomleft))
    ((= justify "BC") (vla-put-alignment otext acalignmentbottomcenter))
    ((= justify "BR") (vla-put-alignment otext acalignmentbottomright))
    (t (vla-put-alignment otext acalignmentleft) (setq dontmove t))
  )
  (if (not dontmove) (vla-put-textalignmentpoint otext (vlax-3d-point insertion)))
  ;(setq aligninfo (ent-text-getjustcons justify))
  ;;;  (entmake
  ;;;    (list
  ;;;      (cons 0 "TEXT")
  ;;;      (cons 1 content)
  ;;;      (cons 7 (getvar "TEXTSTYLE"))
  ;;;      (cons 8 layer)
  ;;;      (cons 10 insertion)
  ;;;      (cons 11 insertion)
  ;;;      (cons 40 height)
  ;;;      (cons 50 (dtr rotation))
  ;;;      (cons 71 0)
  ;;;      (nth 0 aligninfo)
  ;;;      (nth 1 aligninfo)
  ;;;    )
  ;;;  )
  (vlax-vla-object->ename otext)
)
;; Make pline w using vla
(defun MakePlineW (pt1 pt2 w1 w2 layer / opl h) 
  (setq ptlist (list pt1 pt2))
  (if (= 3 (length (car ptlist))) (setq h (caddr (car ptlist))) (setq h 0.0))
  (setq opl (vla-addlightweightpolyline (current-tab) (ptlist->vla-pts ptlist)))
  (vla-put-closed opl (if bClose :vlax-true :vlax-false))
  (vla-put-elevation opl h)
  (vla-put-layer opl layer)
  (vla-setwidth opl 0 w1 w2)
  (vlax-vla-object->ename opl)
)
;|______________________________________________________________________________________________________________|;
; DESCRIPTION: Makes a 2 point polyline with a starting width and an ending width
(defun MakePLineW2 (p1 p2 w1 w2 layer) 
  (entmake (list '(0 . "POLYLINE") (cons 8 layer) (cons 66 1)))
  (entmake (list '(0 . "VERTEX") (cons 10 p1) (cons 40 w1) (cons 41 w2)))
  (entmake (list '(0 . "VERTEX") (cons 10 p2)))
  (entmake (list (cons 0 "SEQEND")))
)
;|______________________________________________________________________________________________________________|;
(defun InsertPoint (pt lay) 
  (setq op (vla-AddPoint (MODEL-SPACE) (vlax-3d-point pt)))
  (vla-put-Layer op lay)
  (vlax-vla-object->ename op)
)
;|______________________________________________________________________________________________________________|;
; DESCRIPTION: Connects a list of points with lines
(defun JoinTheDots (ptList Layer) 
  (setq ii 0)
  (repeat (1- (length ptList)) 
    (Makeline (nth ii ptList) (nth (1+ ii) ptList) layer)
    (setq ii (1+ ii))
  )
)
;<...............................................................................................................>
; DESCRIPTION: Makes the err.. I don't know the name of it
(defun MakeRectang (insert xoffset yoffset layer / P1 P2 P3 P4) 

  (setq P2 (PT-TRANS insert xoffset 0.0 0.0))
  (setq P4 (PT-TRANS insert 0.0 yoffset 0.0))
  (setq P3 (PT-TRANS insert xoffset yoffset 0.0))

  (MakePline (list insert p2 p3 p4) t layer)
)
;<...............................................................................................................>
; DESCRIPTION:  erases whatever selection set or entity is passed
(defun Erase (selection / ent) 
  (if (= 'ENAME (type selection)) 
    (progn (setq todel (vlax-ename->vla-object selection)) 
           (if todel (vla-delete todel))
    )
    (if selection 
      (foreach ent (ss->entlist selection) 
        (vla-delete (vlax-ename->vla-object ent))
      )
    )
  )
)
;<...............................................................................................................>
; DESCRIPTION: Gets the area between a list of points
(defun GetArea (ptList / areaCMD str1 macro macname newlist) 
  (setq areaCMD '(command "._area"))
  (setq newlist (list))
  (foreach pt ptlist 
    (setq newlist (cons (cons 'list pt) newlist))
  ) ;foreach
  (setq areaCMD (append areaCMD newlist (list "")))
  (eval areaCMD)
  (getvar "AREA")
)
;<...............................................................................................................>
; DESCRIPTION: Makes a polyline given a set of points and the layer to put them on
(defun MakePline2 (ptList bClose layer / str1 macro macname newlist plineCMD) 
  (setq oldLayer (getvar "CLAYER"))
  (setvar "CLAYER" layer)

  (setq plineCMD '(command "._pline"))
  (setq newlist (list))

  (foreach pt ptlist 
    (setq newlist (cons (cons 'list pt) newlist))
  ) ;foreach

  (setq plineCMD (append plineCMD newlist (if bClose (list "c") (list ""))))
  (eval plineCMD)
  (setvar "CLAYER" oldLayer)
  (princ)
)
;<...............................................................................................................>
(defun MakePline (ptList bClose layer / opl h) 
  (if (= 3 (length (car ptlist))) (setq h (caddr (car ptlist))) (setq h 0.0))
  (setq opl (vla-addlightweightpolyline (current-tab) (ptlist->vla-pts ptlist)))
  (vla-put-closed opl (if bClose :vlax-true :vlax-false))
  (vla-put-elevation opl h)
  (vla-put-layer opl layer)
  (vlax-vla-object->ename opl)

  ;;;
  ;;;  (if bClose
  ;;;    (entmake (list (cons 0 "POLYLINE") (cons 8 layer) (cons 66 1) (cons 70 1)))
  ;;;    (entmake (list (cons 0 "POLYLINE") (cons 8 layer) (cons 66 1) (cons 70 0)))
  ;;;  )
  ;;;
  ;;;  (foreach pt ptlist
  ;;;    (entmake (list (cons 0 "VERTEX") (cons 10 pt)))
  ;;;  )
  ;;;
  ;;;  (entmake (list (cons 0 "SEQEND")))
  ;;;  (princ)
)

;<...............................................................................................................>
; DESCRIPTION: Makes a 3d polyline given a set of points and the layer to put them on
(defun Make3DPoly (ptlist layer / plineCMD newlist pt plineCMD oldLayer) 
  (setq oldLayer (getvar "CLAYER"))
  (setvar "CLAYER" layer)

  (setq plineCMD '(command "._3dpoly"))
  (setq newlist (list))
  (foreach pt ptlist 
    (setq newlist (cons (cons 'list pt) newlist))
  ) ;foreach
  (setq plineCMD (append plineCMD newlist (list "")))
  (eval plineCMD)
  (setvar "CLAYER" oldLayer)
  (princ)
)
;|______________________________________________________________________________________________________________|;
; DESCRIPTION: Prints co-ordinate values at the point specified
(defun PrintCoord2d (pt textsize / text) 
  (setq text (strcat (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3)))
  (MakeText text textsize "BR" pt 0 (getvar "CLAYER"))
  (princ)
)
;|______________________________________________________________________________________________________________|;
;; DESCRIPTION: inserts a mark into the drawing *MARK-SIZE* determines the size
(defun InsertMark (point layer / p1 p2 eFirst oldOsmode) 
  (if (= nil *MARK-SIZE*) (setq *MARK-SIZE* 400))
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq p1 (pt-trans point (/ *MARK-SIZE* 5) *MARK-SIZE* 0))
  (setq p2 (pt-trans point (- (/ *MARK-SIZE* 5)) *MARK-SIZE* 0))
  (Makeline point p1 layer)
  (setq eFirst (entlast))
  (Makeline p1 p2 layer)
  (Makeline p2 point layer)
  (setq ss (ssCreate eFirst))
  (command "._array" ss "" "p" point "4" "" "")
  (setvar "OSMODE" oldOsmode)
)
;|______________________________________________________________________________________________________________|;
;; DESCRIPTION: Moves a selection set in a given direction a given distance
;;  ud =  up-down
;;  ew =  east-west
;;  ns =  north-south
(defun move (ss direction dist / xval yval zval) 

  (setq xval 0.0)
  (setq yval 0.0)
  (setq zval 0.0)

  (cond 
    ((= "ud" direction)
     (setq zval dist)
    )
    ((= "ew" direction)
     (setq xval dist)
    )
    ((= "ns" direction)
     (setq yval dist)
    )
  )

  (command "._move" ss "" (list 0 0 0) (list xval yval zval))
)
;|______________________________________________________________________________________________________________|;
; DESCRIPTION: Makes a polyline given a set of points and the layer to put them on
(defun Make3DPline (ptList bClose layer / newlist plineCMD pt oldLayer) 
  (setq oldLayer (getvar "CLAYER"))
  (setvar "CLAYER" layer)

  (setq plineCMD '(command "._3Dpoly"))
  (setq newlist (list))
  (foreach pt ptlist 
    (setq newlist (cons (cons 'list pt) newlist))
  ) ;foreach
  (setq plineCMD (append plineCMD newlist (if bClose (list "c") (list ""))))
  (eval plineCMD)
  (setvar "CLAYER" oldLayer)
  (princ)
)
;|______________________________________________________________________________________________________________|;
(defun InsertPoints (ptlist) 
  (foreach pt ptlist 
    (entmake (list '(0 . "POINT") (cons 10 pt)))
  )
  (princ)
)
;|______________________________________________________________________________________________________________|;
;; Insert a block in a drawing at a random scale based on a minimum and a range
(defun Randominsert (insertion blockName scaleMin scaleRange randomrotate / scale 
                     rotation
                    ) 
  (setq scale (+ scaleMin (STD-RANDOM scaleRange)))
  (if randomrotate (setq rotation (STD-RANDOM 360)) (setq rotation 0))
  (command "-insert" blockName insertion scale "" rotation)
  (princ)
)
;|______________________________________________________________________________________________________________|;
(defun MultipleRandomInsert (ptlist blockName scaleMin scaleRange / scale rotation) 
  (foreach pt ptlist (Randominsert pt blockname scaleMin scaleRange))
)
;|______________________________________________________________________________________________________________|;
(defun 3dFaceAndMap (p1 p2 p3 p4 / e1 ss) 
  (command "._3Dface" p1 p2 p3 p4 "")
  (setq e1 (entlast))
  (setq ss (ssCreate e1))
  (C:setuv "A" ss "P" p2 p1 p3 1 '(1.0 1.0))
)
;|______________________________________________________________________________________________________________|;
(defun scaleblock (ent scaleby / entg) 

  (setq ins (entity-get-point ent))
  (command "_scale" ent "" ins scaleby)
  (princ)
)
(defun scaleblock1 (ent scaleby / entg) 

  (setq entg (entget ent))
  (setq xscale (* (cdr (assoc 41 entg)) scaleby))
  (setq yscale (* (cdr (assoc 42 entg)) scaleby))
  (setq zscale (* (cdr (assoc 43 entg)) scaleby))
  (setq entg (subst (cons 41 xscale) (assoc 41 entg) entg)
        entg (subst (cons 42 yscale) (assoc 42 entg) entg)
        entg (subst (cons 43 zscale) (assoc 43 entg) entg)
  )
  (entmod entg)
  (princ)
)

(defun flattenblocks () 
  (vlax-for obj (vla-get-blocks (active-document)) 
    (if 
      (and 
        (= :vlax-false (vla-get-isxref obj))
        (/= "*Model_Space" (vla-get-name obj))
        (not (wcmatch (vla-get-name obj) "*Paper_Space*"))
      )
      (progn 
        (vlax-for onest obj 
          (setq ent (vlax-vla-object->ename onest))
          (flatten ent)
        )
      )
    )
  )
)
(defun flatten (ent / entg) 

  (setq entg (entget ent)
        entg (subst 
               (cons 10 
                     (list (car (cdr (assoc 10 entg))) 
                           (cadr (cdr (assoc 10 entg)))
                           0.0
                     )
               )
               (assoc 10 entg)
               entg
             )
        entg (subst 
               (cons 11 
                     (list (car (cdr (assoc 11 entg))) 
                           (cadr (cdr (assoc 11 entg)))
                           0.0
                     )
               )
               (assoc 11 entg)
               entg
             )
        entg (subst 
               (cons 12 
                     (list (car (cdr (assoc 12 entg))) 
                           (cadr (cdr (assoc 12 entg)))
                           0.0
                     )
               )
               (assoc 12 entg)
               entg
             )
        entg (subst 
               (cons 13 
                     (list (car (cdr (assoc 13 entg))) 
                           (cadr (cdr (assoc 13 entg)))
                           0.0
                     )
               )
               (assoc 13 entg)
               entg
             )
        entg (subst (cons 38 0.0) (assoc 38 entg) entg)
  )
  (entmod entg)
  (princ)
)
(defun ss-flatten (ss / ii) 
  (setq ii 0)
  (repeat (sslength ss) 
    (flatten (ssname ss ii))
    (setq ii (1+ ii))
  )
)
;|______________________________________________________________________________________________________________|;
(defun Scaleblocks (ss scaleby / ent ii) 

  (setq ii 0)
  (repeat (sslength ss) 
    (setq ent (ssname ss ii))
    (setq entg (entget ent))
    (if (= "INSERT" (cdr (assoc 0 entg))) 
      (scaleblock ent scaleby)
    )
    (setq ii (1+ ii))
  )
)
;|______________________________________________________________________________________________________________________________________________________|;
;; DESCRIPTION:
(defun PlineIt (layer) 
  (if (not layer) 
    (setq ss (ssget "X" '((0 . "LINE"))))
    (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 layer))))
  )
  (setq continue nil)
  (setq e1 (ssname ss 0))
  (JoinLines e1)
  (princ)
)
;|______________________________________________________________________________________________________________________________________________________|;
;; DESCRIPTION: Selects all lines on a given layer and makes turns them into joined polylines if possible
(defun JoinAllLines (layer bSpline / ss continue e1 OLDCMDECHO OLDEXPERT elist) 

  (SETQ OLDEXPERT (GETVAR "EXPERT") OLDCMDECHO (GETVAR "CMDECHO"))
  (SETVAR "cmdecho" 0)
  (SETVAR "EXPERT" 5)
  (setq continue t)
  (if (not layer) 
    (setq ss (ssget "X" '((0 . "LINE"))))
    (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 layer))))
  )

  (while continue 
    (setq e1 (ssname ss 0))
    (if (not bSpline) 
      (command "._pedit" e1 "y" "j" ss "" "Lt" "ON" "")
      (command "._pedit" e1 "y" "j" ss "" "Lt" "ON" "")
    )
    (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 layer))))
    (if (not ss) (setq continue nil))
  )
  (SETVAR "cmdecho" OLDCMDECHO)
  (SETVAR "EXPERT" OLDEXPERT)
  elist
)
(defun Joinlines (polyline / linetype) 
  (setq linetype (cdr (SECOND (entget polyline))))

  (if (= t *SPLINE-CONTOURS*) 
    (if (or (= "LINE" linetype) (= "ARC" linetype)) 
      (command "._pedit" polyline "y" "j" "all" "" "s" "")
      (command "._pedit" polyline "j" "all" "" "s" "")
    )
    (if (or (= "LINE" linetype) (= "ARC" linetype)) 
      (command "._pedit" polyline "y" "j" "all" "" "")
      (command "._pedit" polyline "j" "all" "" "")
    )
  )
)
;|______________________________________________________________________________________________________________|;
(defun MakeDiamond (pt size / p1 p2 p3 p4) 
  (setq p1 (pt-trans pt size 0 0))
  (setq p2 (pt-trans pt 0 size 0))
  (setq p3 (pt-trans pt (- size) 0 0))
  (setq p4 (pt-trans pt 0 (- size) 0))
  (command "._line" p1 p2 p3 p4 p1 "")
)
;|______________________________________________________________________________________________________________|;
;; Get all the layers in the drawing
(defun GetNonXREFlayers (/ layer layers newlayers) 
  (setq layers (getlayers t))
  (foreach layer layers 
    (if (not (wcmatch layer "*|*")) 
      (setq newlayers (cons layer newlayers))
    )
  )
  (cdr (reverse newlayers))
)
(defun Getlayers (bOrder / laylist) 
  (tblnext "layer" t)
  (setq bFinished nil)
  (setq laylist '("0"))
  (while (not bFinished) 
    (setq lay (cdr (assoc 2 (tblnext "layer"))))
    (if lay 
      (setq laylist (append laylist (list lay)))
      (setq bFinished t)
    )
  )
  (if bOrder 
    (setq return (std-fast-sort laylist '<))
    laylist
  )
)
;|______________________________________________________________________________________________________________|;
(defun ChangeSelectionToLayer (ss layer) (ss-set-layer ss layer))
(defun ChangeSelectionToColor (ss col) (ss-set-color ss col))
(defun ChangeEntToLayer (ent layer) (ent-set-layer ent layer))
(defun ChangeEntToColor (ent col) (ent-set-color ent col))
(defun ss-set-layer (ss layer / ii) 
  (setq ii 0)
  (repeat (sslength ss) 
    (setq thisent (ssname ss ii))
    (ent-set-layer thisent layer)
    (setq ii (1+ ii))
  )
)
;|______________________________________________________________________________________________________________|;
(defun ss-set-color (ss col / ii) 
  (setq ii 0)
  (repeat (sslength ss) 
    (ent-set-color (ssname ss ii) col)
    (setq ii (1+ ii))
  )
)
;|______________________________________________________________________________________________________________|;
(defun ent-set-layer (ent layer) 
  (vla-put-layer (vlax-ename->vla-object ent) layer)
)
(defun ent-set-color (ent col / entg) 
  (vla-put-color (vlax-ename->vla-object ent) col)
)
;|______________________________________________________________________________________________________________|;
(defun c:randcol () 
  (setq ss (ssget "X" '((8 . "0"))))

  (setq ii 0)
  (repeat (sslength ss) 
    (setq e1 (ssname ss ii))
    (Changeenttocolor e1 (1+ (STD-RANDOM 240)))
    (setq ii (1+ ii))
  )
)
;; Scale a polyline in different directions : ESSENTIAL
(defun ScalePline (pent xscale yscale / neweg ele peg continue evert newptinfo) 

  (setq continue t)

  (setq evert pent)
  (while continue 
    (setq evert (entnext evert))
    (setq egvert (entget evert))
    (if (= "VERTEX" (cdr (assoc 0 egvert))) 
      (progn 
        (setq newptinfo (assoc 10 egvert))
        (setq newptinfo (list 10 
                              (* xscale (nth 1 newptinfo))
                              (* yscale (nth 2 newptinfo))
                              (nth 3 newptinfo)
                        )
        )
        (setq egvert (subst 
                       newptinfo
                       (assoc 10 egvert)
                       egvert
                     )
        )
        (entmod egvert)
      )
      (if (= "SEQEND" (cdr (assoc 0 egvert))) 
        (setq continue nil)
        (princ 
          (strcat "\nEncoutered entity other than SEQEND or VERTEX!!! " 
                  (cdr (assoc 0 egvert))
          )
        )
      )
    )
  )

  (entupd pent)
  (princ)
)
;|______________________________________________________________________________________________________________|;
(defun EntModifyPoint (ent newpt) 
  (setq eg (entget ent))
  (setq eg (subst 
             (cons 10 newpt)
             (assoc 10 eg)
             eg
           )
  )
  (entmod eg)
)
;|______________________________________________________________________________________________________________|;
;; Handy for testing how long a function takes without having to look at it's insufferable pointlist
(defun do (val) 
  (std-require 'STDDEBUG)
  (setq oldstr *TIMER-FORMAT-STRING*)
  (setq *TIMER-FORMAT-STRING* "\nTotal time: %E\nNew Globals: %G names %@\n%m milliseconds taken!")
  (std-time val)
  (setq *TIMER-FORMAT-STRING* oldstr)
  (princ)
)
;; need the old do back (evalute and expression without printing result to screen)
(defun do1 (val) 
  (princ)
)
;|______________________________________________________________________________________________________________|;
;; Search a list of entitys for an entity with a given point. Faster than searching from start to end
(defun SearchSubentsForPoint (subents ecurr pt / thisent continue index ii) 
  (setq continue t)
  (setq index (std-position ecurr subents))
  (setq ii 0)

  (while continue 
    (setq thisent (nth index subents))

    (if (equal pt (cdr (assoc 10 (entget thisent)))) 
      (progn 
        (setq continue nil)
      )
    )

    (setq ii (abs ii))
    (setq ii (1+ ii))
    (if (even ii) 
      (setq ii (- ii))
    )
    (setq index (+ index ii))
  )
  thisent
)
;|______________________________________________________________________________________________________________|;
(defun addtotext (ent val / eg etype currtext) 
  (setq eg (entget ent))
  (setq etype (cdr (assoc 0 eg)))
  (if (= "TEXT" etype) 
    (progn 
      (setq currtext (atof (cdr (assoc 1 eg))))
      (setq eg (subst 
                 (cons 1 (rtos (+ val currtext) 2 2))
                 (assoc 1 eg)
                 eg
               )
      )
      (entmod eg)
      (entupd ent)
    )
  )
)
;|______________________________________________________________________________________________________________|;
(defun GetTextStyles (/ nextstyle styles) 

  (setq styles (LIST (cdadr (tblnext "STYLE" T))))
  (while (setq nextstyle (tblnext "STYLE")) 
    (setq nextstyle (cdadr nextstyle))
    (setq styles (append styles (list nextstyle)))
  )
)
;|______________________________________________________________________________________________________________|;
(defun InsertDrawingFile (filename insertion) 
  (command "-insert" (findfile filename) insertion "" "" "")
  (princ)
)
;|______________________________________________________________________________________________________________|;
(defun GetXrefPathAndName (/ continue eXREF objXREF path name) 

  (setq continue t)
  (while continue 
    (setq eXREF (car (entsel "Select XREF to edit: ")))
    (if (not eXREF) 
      (exit)
      (if (/= "INSERT" (cdr (assoc 0 (entget eXREF)))) 
        (prompt "No XREF selected!")
        (setq continue nil)
      )
    )
  )
  (setq objXREF (vlax-ename->vla-object eXREF))
  (setq path (vla-get-path objXREF))
  (setq name (vla-get-name objXREF))
  (list path name)
)
;|______________________________________________________________________________________________________________|;
(defun elist->ss (elist / ent) 
  (setq ss (ssadd))
  (foreach ent elist 
    (setq ss (ssadd ent ss))
  )
)
;|______________________________________________________________________________________________________________|;
(defun JoinLineSelection (ss / ent) 
  (setq ent (ssname ss 0))
  (setq enttype (cdr (assoc 0 (entget ent))))
  (cond 
    ((= enttype "LWPOLYLINE")
     (command "._pedit" ent "J" ss "" "")
    )
    ((= enttype "LINE")
     (command "._pedit" ent "Y" "J" ss "" "")
    )
  )
)
;|______________________________________________________________________________________________________________|;
;<...............................................................................................................>
; DESCRIPTION: AutoCAD (align ...) command doesn't work properly
(defun MyAlign (ss s1Pt d1Pt s2Pt d2Pt scale / dist1 dist2 thescale ang) 
  (setq oldOS (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._move" ss "" s1Pt d1Pt)
  (setq ang (- 360 (- (rtd (angle s1Pt s2Pt)) (rtd (angle d1Pt d2Pt)))))
  (command "._rotate" ss "" d1Pt ang)

  (if scale 
    (command "._scale" ss "" d1pt (/ (distance d1Pt d2Pt) (distance s1Pt s2Pt)))
  )
  (setvar "OSMODE" oldOS)
)

(defun layer-delete (layername bpurge) 
  (std-layer-unlock layername)
  (std-layer-off layername)
  (Erase (ssget "X" (list (cons 8 layername))))
  (if bpurge 
    (command "purge" "LA" layername "n")
  )
  (princ)
)
(defun C:LAYER-DEL (/ ss ii laylist layname layer thisent) 
  (prompt "\nSelect entities on layers to be deleted")
  (setq ss (ssget)
        ii 0
  )
  (repeat (sslength ss) 
    (setq thisent (ssname ss ii))
    (setq layname (cdr (assoc 8 (entget thisent))))
    (if (std-intersection (list layname) laylist) 
      (princ)
      (setq laylist (append laylist (list layname)))
    )
    (setq ii (1+ ii))
  )
  (foreach layer laylist 
    (layer-delete layer t)
  )
  (princ "\nDeleted Layers:  ")
  (foreach layer laylist 
    (princ (strcat "\n" layer))
  )
  (princ)
)
(DEFUN c:LAYER-DEL1 () 
  (layer-delete (VBA-GETLAYER) t)
)

;(defun xref-unload (name))
;(defun xref-reload (name))
(defun snap-toggle (bOn) 
  (if bOn 
    (if (>= (getvar "OSMODE") 16384) 
      (setvar "OSMODE" (- (getvar "OSMODE") 16384))
    )
    (if (< (getvar "OSMODE") 16384) 
      (setvar "OSMODE" (+ 16384 (getvar "OSMODE")))
    )
  )
)

(defun table-get-names (tname / nlist thisel) 
  (setq nlst (append nlst (list (cdr (assoc 2 (tblnext tname t))))))
  (while (setq thisel (cdr (assoc 2 (tblnext tname)))) 
    (setq nlst (append nlst (list thisel)))
  )
)
    
(defun insert-allblocks (/ ii blist) 
  (setq ii 0)
  (setq blist (table-get-names "block"))
  (repeat (length blist) 
    (command "-insert" (nth ii blist) (getpoint "\nPick insertion pt: ") "" "" "")
    (setq ii (1+ ii))
  )
)
;|PRE entity-|;
(defun entity-set-Elevation (ent h / pt obj) 
  (setq obj (vlax-ename->vla-object ent))
  (setq ent-type (entity-get-type ent))
  (cond 
    ((member ent-type '("INSERT" "TEXT" "XREF" "LINE"))
     (command "._move" 
              ent
              ""
              (list 0 0 (caddr (entity-get-point ent)))
              (list 0 0 h)
     )
    )
    ((= "POLYLINE" ent-type)
     (command "._move" 
              ent
              ""
              (list 0 0 (caddr (first (std-getpts ent))))
              (list 0 0 h)
     )
    )
    (t
     (vla-put-elevation obj h)
    )
  )
)
(defun entity-set-color (ent col) (vla-put-Color (vlax-ename->vla-object ent) col))
(defun entity-set-text (ent str) 
  (vla-put-textstring (vlax-ename->vla-object ent) str)
)
(defun entity-get-type (ent) (cdr (assoc 0 (entget ent))))
(defun entity-get-point (ent) (cdr (assoc 10 (entget ent))))
(defun entity-get-point-end (ent) (cdr (assoc 11 (entget ent))))
(defun entity-get-layer (ent) (cdr (assoc 8 (entget ent))))
(defun entity-pline-closep (ent) (if (even (cdr (assoc 70 (entget ent)))) nil t))
(defun ent-hatch-solid (ent layer col / ob oh ol) 
  (setq ob (vlax-ename->vla-object ent))
  (setq ol (vla-add (vla-get-layers (ACTIVE-DOCUMENT)) layer))
  (setq oh (vla-addhatch (model-space) 0 "SOLID" :vlax-false))
  ;(vla-put-patternscale oh 200)
  (if col (vla-put-color oh col))
  (if (/= :vlax-true (vla-get-closed ob)) 
    (vla-put-closed ob :vlax-true)
  )
  (vla-appendouterloop oh (vla-objectlist->variant (list ob)))
  (vla-evaluate oh)
  (vlax-vla-object->ename oh)
)

;; convert to entmake info
;;Justify: align/fit/center/middle/right/TL/TC/TR/ML/MC/MR/BL/BC/BR
(defun ent-text-getjustcons (just) 
  (setq just (std-string-upcase just))
  (cond 
    ((= just "ALIGN") (list (cons 72 3) (cons 73 0)))
    ((= just "FIT") (list (cons 72 5) (cons 73 0)))
    ((= just "CENTER") (list (cons 72 1) (cons 73 0)))
    ((= just "MIDDLE") (list (cons 72 4) (cons 73 0)))
    ((= just "RIGHT") (list (cons 72 2) (cons 73 0)))
    ((= just "TL") (list (cons 72 0) (cons 73 3)))
    ((= just "TC") (list (cons 72 1) (cons 73 3)))
    ((= just "TR") (list (cons 72 2) (cons 73 3)))
    ((= just "ML") (list (cons 72 0) (cons 73 2)))
    ((= just "MC") (list (cons 72 1) (cons 73 2)))
    ((= just "MR") (list (cons 72 2) (cons 73 2)))
    ((= just "BL") (list (cons 72 0) (cons 73 1)))
    ((= just "BC") (list (cons 72 1) (cons 73 1)))
    ((= just "BR") (list (cons 72 2) (cons 73 1)))
    (T (list (cons 72 0) (cons 73 0)))
  )
)
(defun ent-Block-Add-ToLibrary (name path) 

  (if (= (ver) "Visual LISP 2000 (en)") 
    (command "-insert" (strcat path "/" name) (list 0 0 0) 1 1 0)
    (command "-insert" (strcat path "/" name) (list 0 0 0) 1 1 0)
  )
  (erase (entlast))
)
;|PRE entity-Add|;
;Expects a list of 2 points or a line definition
(defun ent-Add-xline (ln / xval) 
  (if (atom (car ln))  ;if it's line co-efficients extract some points
    (vlax-vla-object->ename 
      (vla-addxline (model-space) 
                    (vlax-3d-point (list (setq xval 0) (geom:line-gety xval ln)))
                    (vlax-3d-point 
                      (list (setq xval 100000.0) (geom:line-gety xval ln))
                    )
      )
    )
    (vlax-vla-object->ename 
      (vla-addxline (model-space) 
                    (vlax-3d-point (car ln))
                    (vlax-3d-point (cadr ln))
      )
    )
  )
  (entlast)
)
(defun ent-block-add (bname insertion sset) 
  (if (= (ver) "Visual LISP 2000 (en)") 
    (command "-block" bname insertion sset "")
    (command "_block" bname insertion sset "")
  )
  (entlast)
)
(defun ent-add-point (pt layer) (Insertpoint (trans pt 1 0) layer) (entlast))
(defun ent-add-Text (content height justify insertion rotation layer) 
  (MakeText content height justify (trans insertion 1 0) (dtr rotation) layer)
)
(defun ent-add-circle (ptCentre radius layer / oc) 
  (vla-put-layer 
    (setq oc (vla-addcircle (current-tab) (vlax-3d-point ptCentre) radius))
    layer
  )
  (vlax-vla-object->ename oc)
)
(defun ent-add-3dface (lst layer) 
  (entmake 
    (list '(0 . "3DFACE") 
          (cons 8 layer)
          (cons 10 (car lst))
          (cons 11 (cadr lst))
          (cons 12 (caddr lst))
          (cons 13 (cadddr lst))
    )
  )
  (entlast)
)
;wrapper functions to maintain existing code and take advantage of the apropros feature
(defun ent-add-Block (bname insertion sset) 
  (ent-Block-add bname (trans insertion 1 0) sset)
  (entlast)
)
(defun ent-add-3dpoly (ptlist layer) 
  (Make3dPoly (std-maptrans10 ptlist) layer)
  (entlast)
)
(defun ent-add-line (p1 p2 layer) 
  (MakeLine (trans p1 1 0) (trans p2 1 0) layer)
  (entlast)
)
(defun ent-add-lines (lnlst layer) 
  (foreach ln lnlst (Makeline (car ln) (cadr ln) layer))
)
(defun ent-add-pline (pts bclose layer) 
  (Makepline (std-maptrans10 pts) bclose layer)
  (entlast)
)
;<...............................................................................................................>
; DESCRIPTION: Inserts a block, different to AutoCAD 14 implentation as a dialog is now used
(defun ent-Block-Insert (name insertion scaleX scaleY scaleZ rotation) 
  (savars '("ATTREQ"))
  (setvar "ATTREQ" 0)
  (if (= (ver) "Visual LISP 2000 (en)") 
    (command "-insert" 
             name
             (trans insertion 1 0)
             "XYZ"
             scaleX
             scaleY
             scaleZ
             rotation
    )
    (command "insert" 
             name
             (trans insertion 1 0)
             "XYZ"
             scaleX
             scaleY
             scaleZ
             rotation
    )
  )
  (revars)
  (entlast)
)
;|PRE text-|;
(defun text-getnum (ent) 
  (setq txt (cdr (assoc 1 (entget ent))))
  (if (= 0.0 (STRING-GETENDNUM txt nil)) 
    (STRING-GETENDNUM txt t)
  )
)
;|______________________________________________________________________________________________________________________________________________________
LAYER FUNCTIONS SECTION -BEGIN                                                                                                                        |;
; DESCRIPTION: Accepts a layer name and returns true if it exists otherwise nil
(defun layer-ExistP (layername / return) (tblsearch "layer" layername))
; DESCRIPTION: Freezes all layers except the passed list
(defun layer-Isolate (layerList / layer) 
  (command ".-Layer" "thaw" "*" "")
  (command ".-Layer" "set" (car layerList) "freeze" "*" "")
  (foreach layer layerList 
    (command ".-Layer" "thaw" layer "")
  )
)
(defun layer-iso (layer) 
  (command ".-Layer" "set" (car layerList) "OFF" "*" "")
  (command ".-layer" "set" layer "ON" "")
)
;; oldname: entget-detail
(defun entity-getdetail (ent num) (cdr (assoc num (entget ent))))
;|PRE - text|;
(defun text-getnum (ent / txt) 
  (setq txt (cdr (assoc 1 (entget ent))))
  (if (= 0.0 (setq res (STRING-GETENDNUM txt nil))) 
    (setq res (STRING-GETENDNUM txt t))
  )
  res
)
(defun ent-text-set-value (nt ent / entg) 
  (setq entg (entget ent))
  (setq entg (subst 
               (cons 1 nt)
               (assoc 1 entg)
               entg
             )
  )
  (entmod entg)
)












;|Miscillaneous|;
(defun lines-get (/ ss thisent ii lst) 
  (setq ss (ssget '((0 . "LINE")))
        ii 0
  )
  (repeat (sslength ss) 
    (setq thisent (ssname ss ii))
    (setq lst (append lst 
                      (list 
                        (list (entity-get-point thisent) 
                              (entity-get-point-end thisent)
                        )
                      )
              )
    )
    (setq ii (1+ ii))
  )
  lst
)
(defun math:round-point (pt numplaces) 
  (mapcar '(lambda (x) (math:round-places x numplaces)) pt)
)
(defun math:round-lines (lns numplaces / ln newlns) 
  (foreach ln lns 
    (setq newlns (cons 
                   (list (math:round-point (car ln) 5) 
                         (math:round-point (cadr ln) 5)
                   )
                   newlns
                 )
    )
  )
  (reverse newlns)
)

;| 3D lines - 3DPolyjoin |;
(defun lines->pts (lns / ln pt newlns ptafter ptafterlst ptbeforelst getptsafter 
                   find-el-with-member
                  ) 

  (setq lns (math:round-lines lns 5)) ; round off all members of the list to make comparisons easy

  ;; REQUIRED HELPER FUNCTION DEFINITIONS - BEGIN
  ;; returns points after a given point
  (defun getptsafter (pt / pts lastpt ln ptafter) 
    (setq ln (find-el-with-member pt))
    (if ln 
      (setq lns     (std-remove ln lns)
            ptafter (if (= 1 (length (member pt ln))) (car ln) (cadr ln))
            res     (if lns (car (cons (cons ptafter (getptsafter ptafter)) pts)))
      )
    )
  )

  ;; finds an element with value memb as a member of it's list
  (defun find-el-with-member (memb / finished ii thisel) 
    (setq ii 0)
    (while (not finished) 
      (setq thisel (nth ii lns))
      (if (member memb thisel) (setq finished t) (setq thisel nil))
      (setq ii (1+ ii))
      (if (= ii (length lns)) (setq finished t))
    )
    thisel
  )
  ;; REQUIRED HELPER FUNCTION DEFINITIONS - END

  (setq ln          (car lns)
        lns         (std-remove ln lns)
        pt          (car ln)
        ptafter     (cadr ln)
        ptafterlst  (getptsafter ptafter)
        ptbeforelst (reverse (getptsafter pt))
  )

  (append ptbeforelst (append (list pt) (append (list ptafter) ptafterlst)))
)
;| 3D lines - 3DPolyjoin |;

(defun C:ltest (/ pts lns) 
  (setq lns (lines-get))
  (setq starttime (std-timer-start))
  (setq *lines* (length lns))
  (setq pts (lines-pointlist lns))
  (Makepline pts t "0")
  (setq endtime (std-timer-start))
  (- endtime starttime)
)

(defun C:TT (/ time) 
  (std-require 'STDDEBUG)
  (setq time (C:LTEST))
  (setq timelst (append timelst (list (list *lines* time))))
)

(DEFUN SEARCH-LINELIST (linelst LINE / lst ii el triplets triplet) 
  (foreach el linelst (setq lst (append lst (list (reverse el)))))
  (setq linelst (append linelst lst))
  (setq linelst (vl-sort linelst 
                         (function 
                           (lambda (e1 e2) (< (car (car e1)) (car (car e2))))
                         )
                )
        ii      0
  )

  ;; Reduce the double list to triplets (pt pt-before pt-after)
  ;; The triplet list is still ordered and therefore easier to search.
  (repeat (/ (length linelst) 2) 
    (setq triplet (list (car (nth ii linelst)) 
                        (cadr (nth ii linelst))
                        (cadr (nth (1+ ii) linelst))
                  )
    )
    (setq triplets (append triplets (list triplet)))
    (setq ii (+ ii 2))
  )
  triplets
)
(defun lines-sort (ls) 
  (setq firstline (car ls)
        remlines  (cdr ls)
  )

  (setq ii       0
        continue t
  )
  (while continue 
    (setq thisline (nth ii line))

    (if (member (car firstline) thisline) 
      (princ)
    )

    (setq ii (1+ ii))
  )
)
(defun layer-select (default-layer / layer) 
  (setq layer (getstring 
                (strcat "\nEnter layer name [<" 
                        (if (layer-existp default-layer) default-layer "0")
                        ">|(@)Select]: "
                )
              )
  )
  (cond 
    ((= "@" layer) (setq default-layer (vba-getlayer)))
    (t (setq default-layer (if (layer-existp layer) layer default-layer)))
  )
  (std-layer-on default-layer)
  (std-layer-thaw default-layer)
  (setvar "CLAYER" default-layer)
  default-layer
)

(defun text-replace (ss oldtext newtext bWholeWordOnly / ii text ent) 
  ;(setq ss (ssget "X" '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (-4 . "OR>"))))
  (setq ii 0)
  (repeat (sslength ss) 
    (setq ent (ssname ss ii))
    (setq text (cdr (assoc 1 (entget ent))))
    (if bWholeWordOnly 
      (if (= text oldtext) 
        (ent-text-set-value newtext ent)
      )
      (if (wcmatch text (strcat "*" oldtext "*")) 
        (ent-text-set-value (acet-str-replace oldtext newtext text) ent)
      )
    )
    (setq ii (1+ ii))
  )
  (princ "\nDone!")
  (princ)
)
(defun ssget-all-bylayer (layer / enttype) 
  (ssget "X" (if layer (list (cons 8 layer))))
)
(defun ssget-hatch-bylayer (layer / enttype) 
  (setq enttype "HATCH")
  (ssget "X" 
         (if layer (list (cons 0 enttype) (cons 8 layer)) (list (cons 0 enttype)))
  )
)
(defun ssget-lines-bylayer (layer / enttype) 
  (setq enttype "LINE")
  (ssget "X" 
         (if layer (list (cons 0 enttype) (cons 8 layer)) (list (cons 0 enttype)))
  )
)
(defun ssget-points-bylayer (layer / enttype) 
  (setq enttype "POINT")
  (ssget "X" 
         (if layer (list (cons 0 enttype) (cons 8 layer)) (list (cons 0 enttype)))
  )
)
(defun ss->entlist (ss / lst) 
  (setq ii 0)
  (repeat (sslength ss) 
    (setq thisent (ssname ss ii))
    (setq lst (cons thisent lst))
    (setq ii (1+ ii))
  )
  lst
)
(defun C:xlayoff () 
  (while (setq nent (car (nentsel))) 
    (prompt "\nSelect XREF layer to turn off: ")
    (std-layer-off (cdr (assoc 8 (entget nent))))
  )
)
;Written By Jason Piercey
(defun vport-getscale (evport / a41 a45) 
  (setq a41 (cdr (assoc 41 (entget evport)))
        a45 (cdr (assoc 45 (entget evport)))
  )
  (/ a45 a41)
)

(defun getvpscale (/ ename a41 a45) 
  (if (= 1 (getvar "tilemode")) 
    (progn (princ "\nNot allowed in modelspace") (princ))
    (progn 
      (setq ename (ssname 
                    (ssget "x" 
                           (list '(0 . "VIEWPORT") (cons 69 (getvar "cvport")))
                    )
                    0
                  )
            a41   (* 1.0 (cdr (assoc 41 (entget ename))))
            a45   (cdr (assoc 45 (entget ename)))
      )
      (/ a45 a41)
    )
  )
)
(defun layerlist (/ lays lay) 
  (tblnext "LAYER" t)
  (reverse 
    (while (setq lay (cdadr (tblnext "LAYER"))) 
      (if (not (wcmatch lay "*|*")) 
        (setq lays (cons lay lays))
      )
    )
  )
  lays
)

(defun layer-get-similar (pat / continue ii layers len lay) 
  (setq continue t
        ii       0
  )
  (setq layers (layerlist)
        len    (length layers)
  )
  (while continue 
    (if (wcmatch (std-string-upcase (nth ii layers)) (std-string-upcase pat)) 
      (setq continue nil
            lay      (nth ii layers)
      )
    )
    (setq ii (1+ ii))
    (if (> ii len) (setq continue nil))
  )
  lay
)
(defun layer-MakeSet (lset / ldesc ol) 
  (foreach ldesc lset 
    (setq ol (vla-add (vla-get-layers (ACTIVE-DOCUMENT)) (car ldesc)))
    (cond 
      ((cadr ldesc) (vla-put-color ol (cadr ldesc)))
      ((caddr ldesc) (princ)) ;(vla-put-color ol (caddr ldesc))
    )
  )
)
(defun block-get-attribute (ob tagname / attobj retatt) 
  (foreach attobj (vlax-safearray->list (vlax-variant-value (vla-getattributes ob))) 
    (if (= tagname (vla-get-tagstring attobj)) 
      (setq retatt (vla-get-textstring attobj))
    )
  )
  retatt
)
(defun block-Set-attribute (oBlock tagname newval / attobj tagname retatt) 
  (if newval 
    (foreach attobj (vlax-safearray->list (vlax-variant-value (vla-getattributes oBlock))) 
      (if (= tagname (vla-get-tagstring attobj)) 
        (setq retatt (vla-put-textstring attobj newval))
      )
    )
  )
)
(defun ent-add-text-paperspace (content height justify insertion rotation layer / 
                                otext dontmove
                               ) 
  (setq otext (vla-addtext (paper-space) content (vlax-3d-point insertion) height))
  (vla-put-rotation otext rotation)
  (vla-put-layer otext layer)
  (cond 
    ((= justify "ALIGN")
     (vla-put-alignment otext acalignmentaligned)
     (setq dontmove t)
    )
    ((= justify "FIT") (vla-put-alignment otext acalignmentfit) (setq dontmove t))
    ((= justify "CENTER") (vla-put-alignment otext acalignmentcenter))
    ((= justify "MIDDLE") (vla-put-alignment otext acalignmentmiddle))
    ((= justify "RIGHT") (vla-put-alignment otext acalignmentright))
    ((= justify "TL") (vla-put-alignment otext acalignmenttopleft))
    ((= justify "TC") (vla-put-alignment otext acalignmenttopcenter))
    ((= justify "TR") (vla-put-alignment otext acalignmenttopright))
    ((= justify "ML") (vla-put-alignment otext acalignmentmiddleleft))
    ((= justify "MC") (vla-put-alignment otext acalignmentmiddlecenter))
    ((= justify "MR") (vla-put-alignment otext acalignmentmiddleright))
    ((= justify "BL") (vla-put-alignment otext acalignmentbottomleft))
    ((= justify "BC") (vla-put-alignment otext acalignmentbottomcenter))
    ((= justify "BR") (vla-put-alignment otext acalignmentbottomright))
    (t (vla-put-alignment otext acalignmentleft) (setq dontmove t))
  )
  (if (not dontmove) (vla-put-textalignmentpoint otext (vlax-3d-point insertion)))
  (vlax-vla-object->ename otext)
)
;; replace all instances of one piece of text with another in a drawing
;; if bWholeWordOnly is t   - only whole words are replaced
(defun text-replace (oldtext newtext bWholeWordOnly / ss ii text ent) 
  (setq ss (ssget "X" '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (-4 . "OR>"))))
  (setq ii 0)
  (repeat (sslength ss) 
    (setq ent (ssname ss ii))
    (setq text (cdr (assoc 1 (entget ent))))
    (if bWholeWordOnly 
      (if (= text oldtext) 
        (ent-text-set-value newtext ent)
      )
      (if (wcmatch text (strcat "*" oldtext "*")) 
        (ent-text-set-value (acet-str-replace oldtext newtext text) ent)
      )
    )
    (setq ii (1+ ii))
  )
  (princ "\nDone!")
  (princ)
)
(defun ent-text-set-value (nt ent / entg) 
  (setq entg (entget ent))
  (setq entg (subst 
               (cons 1 nt)
               (assoc 1 entg)
               entg
             )
  )
  (entmod entg)
)
(defun entlist->ss (elist / ss e) 
  (setq ss (ssadd))
  (foreach e elist 
    (setq ss (ssadd e ss))
  )
)
(defun blocknames (/ blk blks) 
  (setq blks (cons (cdr (assoc 2 (tblnext "BLOCK" t))) blks))
  (while (setq blk (cdr (assoc 2 (tblnext "BLOCK")))) 
    (setq blks (cons blk blks))
  )
  (reverse blks)
)
(defun viewports (/ obj vplst) 
  (vlax-for obj (paper-space) 
    (if (vla-is-a obj "AcDbViewport") 
      (setq vplst (cons obj vplst))
    )
  )
  (reverse vplst)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRDRAW FUNCTIONS
(defun grdrawpoly (pts / pt lp) 
  (foreach pt pts 
    (if lp 
      (grdraw pt lp 30)
    )
    (setq lp pt)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;LAYER FUNCTIONS
;;
(defun Lay:Off (layername) 
  (if (layer-ExistP layername) 
    (progn 
      (setq ol (vla-add (vla-get-Layers (ACTIVE-DOCUMENT)) layername))
      (vla-put-LayerOn ol :vlax-false)
    )
  )
)
(defun Lay:On (layername) 
  (if (layer-ExistP layername) 
    (progn 
      (setq ol (vla-add (vla-get-Layers (ACTIVE-DOCUMENT)) layername))
      (vla-put-LayerOn ol :vlax-true)
    )
  )
)

(defun Layer-SaveState (/ laylist lay) 
  (vlax-for lay (vla-get-layers (active-document)) 
    (setq laylist (cons 
                    (list 
                      (vla-get-name lay)
                      (vla-get-layeron lay)
                      (vla-get-freeze lay)
                      (vla-get-lock lay)
                    )
                    laylist
                  )
    )
  )
)
(defun Layer-RestoreState (lstate / lay details) 
  (vlax-for lay (vla-get-layers (active-document)) 
    (setq details (assoc (vla-get-name lay) lstate))
    (if details 
      (progn 
        (vla-put-layeron lay (cadr details))
        (if (/= (vla-get-name lay) (getvar "CLAYER")) 
          (vla-put-freeze lay (caddr details))
        )
        (vla-put-lock lay (cadddr details))
      )
    )
  )
)

;SILENT LOADING

(defun lay-filters-erase (/) 
  (vl-Load-Com)
  (vl-Catch-All-Apply 
    '(lambda () 
       (vla-Remove 
         (vla-GetExtensionDictionary 
           (vla-Get-Layers 
             (vla-Get-ActiveDocument 
               (vlax-Get-Acad-Object)
             )
           )
         )
         "ACAD_LAYERFILTERS"
       )
     )
  )
  (princ 
    "\n All Layer Filters That Can Be Deleted...Have Been Deleted..."
  )
  (princ)
)
;; converts a selection of polylines into splines
(defun poly->Spline (ss / ii) 
  (setq ii 0)
  (repeat (sslength ss) 
    (setq thisent (ssname ss ii))
    (command "PEDIT" thisent "S" "")
    (setq ii (1+ ii))
  )
)

(princ)