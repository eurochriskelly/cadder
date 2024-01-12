;; Functions for dealing with lists of points

;; old name: 2dof
(DEFUN pt-3D->2D (PT) (SETQ PT (LIST (CAR PT) (CADR PT))))
(DEFUN pt-2D->3D (PT) (if (= 2 (length pt)) (append pt (list 0.0)) pt))
(defun ptlist-3d->2d (pts)  (mapcar '(lambda (x) (pt-3D->2D x)) pts))
;;old name: TP
(defun pt-TransByPt ( p1 p2 / p3 )
  (if (= 3 (length p1))
      (list (+ (car p1) (car p2)) (+ (cadr p1) (cadr p2)) (+ (caddr p1) (caddr p2)))
    (list (+ (car p1) (car p2)) (+ (cadr p1) (cadr p2)))
    )
  )
; DESCRIPTION: Translates a point by an x,y and z value
;;old name: XP
(defun pt-Trans ( p1 X Y Z / p3 ) (list (+ (car p1) X)(+ (cadr p1) Y) (if (= (length p1) 3) (+ (caddr p1) Z) 0) )
  )
(defun pt-gowest (p1 val) (list (- (car p1) val) (cadr p1) (if (= (length p1) 3) (caddr p1) 0)))
(defun pt-goeast (p1 val) (list (+ (car p1) val) (cadr p1) (if (= (length p1) 3) (caddr p1) 0)))
(defun pt-gonorth (p1 val) (list (car p1) (+ (cadr p1) val) (if (= (length p1) 3) (caddr p1) 0)))
(defun pt-gosouth (p1 val) (list (car p1) (- (cadr p1) val) (if (= (length p1) 3) (caddr p1) 0)))

(defun ptlist-removeduplicates (lst places)
  (std-remove-duplicates (mapcar '(lambda (x) (mapcar '(lambda (y) (math:round-places y places)) x)) lst)))
(defun ptlist->lines (pts / ii lnlst )
  (setq ii 0)
  (repeat (1- (length pts))
	  (setq lnlst (append lnlst (list (list (nth ii pts) (nth (1+ ii) pts)))))
	  (setq ii (1+ ii))
	  )
  lnlst
  )

; DESCRIPTION: Gets the area given a number of points
;;old name: getlistofpoints
(defun ptlist-UserPick (promptstr / pointList point return)
  (setq point (getpoint promptstr))
  
  (while point
    (setq pointList (cons point pointList))
    (setq point (getpoint "\nPick next point: "))
    )
  (setq return pointList)
  )

; DESCRIPTION:
;;old name: SortPointList
(defun ptlist-Sort (lst direction /)
  (setq lst (STD-SORT lst '(lambda (x y) (< (car x) (car y))))) 
  (if (= (STD-TOUPPER (ascii direction)) (ascii "D"))
      (setq lst (reverse lst))
    lst
    )
  )
(defun ptlist-Sort-ascend (lst /)
  (setq ptStart (list (car (car lst)) (cadr (car lst)) 0.0))
  (setq ptEnd (list (car (last lst)) (cadr (last lst)) 0.0))
  (command "_ucs" "_3p" ptStart ptEnd "")
  (setq lst (STD-MAPTRANS10 lst)
        lst (STD-SORT lst '(lambda (x y) (< (car x) (car y))))
        lst (std-maptrans01 lst))
  (command "_ucs" "_p")
  lst
  )
; DESCRIPTION:
;;old name: SortPointList
(defun ptlist-Sort-y (lst direction /)
  ;; A hint of object orient methodologies here:
  (setq lst (STD-SORT lst '(lambda (a b) (< (cadr a) (cadr b))))) 
  (if (= (STD-TOUPPER (ascii direction)) (ascii "D"))
      (setq lst (reverse lst))
    lst
    )
  )
;;old name: ScalePointList
(defun ptlist-Scale ( ptList xscale yscale zscale / newpointlist index)
  (foreach item ptList
	   (if (STD-POINTP item)
	       (if (STD-2DPOINTP item)
		   (setq newpointlist (append newpointlist (list (list (* xscale (FIRST item)) (* yscale (SECOND item))))))
		 (setq newpointlist (append newpointlist (list (list (* xscale (FIRST item)) (* yscale (SECOND item)) (* zscale (THIRD item))))))
		 )
	     )
	   (setq newpointlist (append newpointlist (list (ptlist-scale item xscale yscale zscale))))
	   )
  newpointlist
  )
(defun ptlist-getfromss (ss / pts ii thisent)
  (setq ii 0)
  (repeat (sslength ss)
	  (setq thisent (ssname ss ii))
	  (if (member (entity-get-type thisent) '("LWPOLYLINE" "POLYLINE" "LINE" "2DPOLYLINE"))
	      (setq pts (append pts (std-getpts thisent)))
	    )
	  (setq ii (1+ ii))
	  )
  pts
  )
;;old name: ScalePointList2d
(defun ptlist-Scale-2d ( ptList xscale yscale / newpointlist)
  (foreach item ptList
	   (if (STD-POINTP item)
	       (setq newpointlist (append newpointlist (list (list (* xscale (FIRST item)) (* yscale (SECOND item))))))
	     (setq newpointlist (append newpointlist (list (ptlist-scale-2d item xscale yscale))))
	     )
	   )
  newpointlist
  )
; DESCRIPTION: A nicely formatted list of nested points
;;old name: PrintPointList
(defun ptlist-Print (ptList strsub / fhand)
  
  (setq index 1)
  (setq index2 1)
  (if (/= "" strsub)
      (setq strsub (strcat strsub "."))
    )
  
  (foreach item ptList
	   (if (STD-POINTP item)
	       (progn
		 (printitem item index strsub)
		 (setq index (1+ index))
		 )
	     (progn
	       (setq index2 (1+ index2))
	       (princ (strcat "\n" strsub (itoa index2) "->  Branch.."))
	       (ptlist-Print item (strcat "    " strsub (itoa index2)))
	       )
	     )
	   )
  (princ)
  )
(defun ptlist-PrintQ (lst) (ptlist-print lst "%%"))
;;old name: centerofptlist
(defun  ptlist-GetCenterpt (ptlst
			    /
			    pt xvals yvals)

  (foreach pt ptlst
	   (setq xvals (append xvals (list (car pt))))
	   )
  (foreach pt ptlst
	   (setq yvals (append yvals (list (cadr pt))))
	   )
  (list (/ (apply '+ xvals) (length ptlst)) (/ (apply '+ yvals) (length ptlst)))
  )
; DESCRIPTION: Gets the accumalative distance of list
;;old name: GetDistOfList
(defun ptlist-GetDistOf (ptlst / total ii)
  (setq total 0 ii 1)
  (repeat (1- (length ptlst))
	  (setq total (+ total (distance (nth (1- ii) ptlst) (nth ii ptlst))))
	  (setq ii (1+ ii))
	  )
  total
  )
(defun ptlist-GetDistOf-2D (ptlst / total ii)
  (setq total 0 ii 1)
  (repeat (1- (length ptlst))
	  (setq total (+ total (distance (pt-3D->2D (nth (1- ii) ptlst)) (pt-3D->2D (nth ii ptlst)))))
	  (setq ii (1+ ii))
	  )
  total
  )
; DESCRIPTION: Gets the overall horizontal distance of a list
;;old name: GetHorizDistOfList
(defun ptlist-GetLength-XAxis (lst / total ii)
  (setq total 0 ii 1)
  (repeat (1- (length lst))
	  (setq total (+ total (abs (- (car (nth ii lst)) (car (nth (1- ii) lst)) ) )))
	  (setq ii (1+ ii))
	  )
  total
  )
(defun ptlist-GetLength-YAxis (lst / total ii)
  (setq total 0 ii 1)
  (repeat (1- (length lst))
	  (setq total (+ total (abs (- (cadr (nth ii lst)) (cadr (nth (1- ii) lst)) ) )))
	  (setq ii (1+ ii))
	  )
  total
  )
(defun ptlist-GetLength-ZAxis (lst / total ii)
  (setq total 0 ii 1)
  (repeat (1- (length lst))
	  (setq total (+ total (abs (- (caadr (nth ii lst)) (caadr (nth (1- ii) lst)) ) )))
	  (setq ii (1+ ii))
	  )
  total
  )
;;old name: GetEntityPoints
(defun ptlist-ss-getpts ( ss / eget ii item ptlist ent)
  (setq ii 0 jj 0)
  (repeat (sslength ss)
	  (setq ent (ssname ss jj))
	  (setq eget (entget ent))
	  (foreach item eget
		   (setq numitem (car item))
		   (if (or (= 10 numitem) (= 11 numitem) (= 12 numitem) (= 13 numitem))
		       (progn
			 (setq lastpoint (cdr item))
			 (setq ptlist (append ptlist (list lastpoint)))
			 )
		     )
		   (setq ii (1+ ii))
		   )
	  (setq jj (1+ jj))
	  )
  ptlist				;RETURNING
  )
(defun ptlist-GetPointsFromSS (ss / lst ii)
  (setq ii 0)
  (repeat (sslength ss)
	  (setq thisent (ssname ss ii))
    
	  (setq lst (ptlist-combinelists (list lst (std-getpts thisent))))
	  (setq ii (1+ ii))
	  )
  lst
  )

;;;TASK:- Thu May 18 14:38:55 2006 :
;;; MAJOR TASK!!!! - SET THIS UP TO WORK WITH XREFS
(defun ptlist-GetPointsFromSS-withLevs (ss / lst ii)
  (setq ii 0)
  (repeat (sslength ss)
	  (setq thisent (ssname ss ii))

	  ;Check if the point is a block
	  (if (= "INSERT" (cdr (assoc 0 (entget thisent))))

	      (progn
		; Then check if it is level or not
		(if
		    (and
		     (wcmatch (cdr (assoc 2 (entget thisent))) "MBK*")
		     (wcmatch (cdr (assoc 8 (entget thisent))) "V-SPOT-L*")
		     )
		    (setq lst (ptlist-combinelists (list lst (list (OLEV:GET-POINT3D thisent)))) )
		  )
		)
	    )
	  (setq ii (1+ ii))
	  )
  lst
  )
;;old name: getboundingbox
(defun ptlist-GetBoundingBox ( ptlist / xmin ymin xmax ymax)
  ;; Use STD-X-MIN1 etc
    
  (setq xmin  (PTLIST-MIN-X ptlist)
        ymin  (PTLIST-MIN-Y ptlist)
        xmax  (PTLIST-MAX-X ptlist)  
        ymax  (PTLIST-MAX-Y ptlist))
        
  (list (list xmin ymax) (list xmax ymin))
  )
;;old name: GetpointsOnArc
(defun ptlist-arc->ptlist (cp startang endang radius numpoints / ptlist)
  (setq totalang (- endang startang))
  (setq incang (/ totalang numpoints))
    
  (setq ii 0)
  (repeat numpoints
	  (setq pt (polar cp (+ startang (* ii incang)) radius))
	  (setq ptlist (append ptlist (list pt)))
	  (setq ii (1+ ii))
	  )
  (append ptlist (list ep))
  )
; DESCRIPTION: Gets the mid point of any 2 points
;;old name: GetMidpoint
(defun pt-GetMidpoint (pt1 pt2) (list (/ (+ (car pt1) (car pt2)) 2.0) (/ (+ (cadr pt1) (cadr pt2)) 2.0)))
; DESCRIPTION: Gets a point some proportion along a line of 2 points
;;old name: GetPointAlong
(defun pt-AlongLine-byFactor ( p1 p2 proportion ) (list (+ (car p1) (* (- (car p2) (car p1)) proportion)) (+ (cadr p1) (* (- (cadr p2) (cadr p1)) proportion))))
(defun pt-AlongLine-byAngleDist (p1 ang dist)
  (list
   (+
    (car p1)
    (* dist (cos ang))
    )
   (+
    (cadr p1)
    (* dist (sin ang))
    )
   )
  )

;;old name:MoveAlongLine

(defun pt-AlongLine-byDist (p1 p2 dist / xval yval factor)
  (if (equal p1 p2)
      (setq factor 0.0)
    (setq factor (/ dist (distance p1 p2)))
    )

  (setq xval (* factor (- (car p2) (car p1)) ))
  (setq yval (* factor (- (cadr p2) (cadr p1)) ))
  
  (if (and (STD-3DPOINTP p1) (STD-3DPOINTP p2))
      (pt-trans p1 xval yval (* factor (- (caddr p2) (caddr p1)) ))
    (pt-transbypt (list xval yval) p1)
    )
  )
;;old name:CombinePointLists
;;<..............................................................................................................>
;; DESCRIPTION: Combine a number of lists of 2d points and order them by x and then y
(defun ptlist-CombineLists ( ptListList / newlist lst pt)
  (foreach lst ptListList
	   (foreach pt lst
		    (setq newlist (cons pt newlist))))
  (std-fast-sort (reverse newlist) '(lambda (x y) (< (car x) (car y)))))
;;old name: STD-X-MAX1
;|Backwards compatibility|;(defun STD-X-MAX1 (lst) (ptlist-max-x lst))
(defun ptlist-Max-X ( lst / result tempMAX override)
  (apply 'max (mapcar 'car lst))
  )
;;old name: STD-X-MIN1
;|Backwards compatibility|;(defun STD-X-MIN1 (lst) (ptlist-min-x lst))
(defun ptlist-Min-X ( lst / result tempMAX)
  (apply 'min (mapcar 'car lst))
  )
;;old name: STD-Y-MAX1
(defun ptlist-Max-Y ( lst / result tempMAX)
  (apply 'max (mapcar 'cadr lst))
  )
;;old name: STD-Y-MIN1
(defun ptlist-Min-Y ( lst / result tempMAX)
  (apply 'min (mapcar 'cadr lst))
  )
;;old name: STD-Z-MAX1
(defun ptlist-Max-Z ( lst / result tempMAX)
  (apply 'max (mapcar 'caddr lst))
  )
;;old name: STD-Z-MIN1
(defun ptlist-Min-Z ( lst / result tempMAX)
  (apply 'min (mapcar 'caddr lst))
  )
;;old name: GetArea
;<...............................................................................................................>
; DESCRIPTION: Gets the area between a list of points
(defun ptlist-GetArea (ptList / areaCMD str1 macro macname newlist)
  (setq areaCMD '(command "._area"))
  (setq newlist (list))
  (foreach pt ptlist 
	   (setq newlist (cons (cons 'list pt) newlist))
	   )				;foreach
  (setq areaCMD (append areaCMD newlist (list "")))
  (eval areaCMD)
  (getvar "AREA")
  )
  (defun ptlist-removeduplicates (ptlist fuzz / it ret)
    (setq ptlist (mapcar '(lambda (x) (math:round-point x fuzz)) ptlist))
    (foreach item ptlist (if (not (member item ret)) (setq ret (cons item ret))))
    (reverse ret))
(defun pts-colinear (p0 p1 p2 /  Return  ) ;END LOCAL VARIABLES
  (setq Return
	(+  (* (car p0) (cadr p1))
	    (* (car p1) (cadr p2))
	    (* (car p2) (cadr p0))
	    (- (* (car p1) (cadr p0)))
	    (- (* (car p2) (cadr p1)))
	    (- (* (car p0) (cadr p2)))
	    )
	)
  (cond
   ( (equal Return 0.0 1.0e-4)
     0					; colinear
     )
   ( (minusp Return)
     -1					; clockwise
     )
   ( T
     1					; counterclockwise
     )
   )					;end cond.
  )
(defun pt-vlax->lisp (vlaxpt)
  (vlax-safearray->list (vlax-variant-value vlaxpt))
  )
(defun ptlist-ss-reduceby (ss reduceby
			      /
			      eg)
  (setq ii 0)
  (repeat (sslength ss)
	  (setq ethis (ssname ss ii))
	  (setq eg (entget ethis))
	  (setq pt (cdr (assoc 10 eg)))
	  (setq depth (atof (cdr (assoc 1 eg))))
	  (setq newpoint (list  (nth 0 pt) (nth 1 pt) (* 1000 (- depth reduceby)) ))
	  (setq newdepth (rtos (- depth reduceby) 2 2))
    
	  (setq eg (subst (cons 1 newdepth) (assoc 1 eg) eg ))
	  (entmod eg)

	  (ent-add-point newpoint "Survey-Points-3d")
	  (setq ii (1+ ii))
    
	  )
  (princ)
  )
(defun ptlist-ss-insert-atheightoftext (ss
					/
					eg ii depth newpoint)
  (setq ii 0)
  (repeat (sslength ss)
	  (setq ethis (ssname ss ii))
	  (setq eg (entget ethis))
	  (setq pt (cdr (assoc 10 eg)))
	  (setq depth (atof (cdr (assoc 1 eg))))
	  (setq newpoint (list  (nth 0 pt) (nth 1 pt) (* 1000 depth) ))
   
	  (ent-add-point newpoint "Survey-Points-3d")
	  (setq ii (1+ ii))
    
	  )
  (princ)
  )
(defun ptlist->vla-pts (pts / ) (list->variantarray (apply 'append (mapcar 'pt-3d->2d pts))))
(defun list->variantarray (ptslist / arrayspace sarray)
                                        ; allocate space for an array of 2d points stored as doubles
  (setq arrayspace
	(vlax-make-safearray
	 vlax-vbdouble			; element type
	 (cons 0
	       (- (length ptslist) 1)
	       )			; array dimension
	 )
        )
  (setq sarray (vlax-safearray-fill arrayspace ptslist))
                                        ; return array variant
  (vlax-make-variant sarray)

  )

(defun ptlist-scale-2D (ptlist sc / pl)  (mapcar (function (lambda (x) (list (* sc (car x)) (* sc (cadr x)) (caddr x)) )) lst))

(defun pt-flatten (pt)  (list (FIRST pt) (SECOND pt) 0.0)  )
