;;-------------------------------
;;   GENERIC REUSEABLE FUNCTIONS
;;       - by Chris Kelly
;; -------------------------------
;;
;; A collection of generic reuseable functions 
;; - maily wrapper functions for verbose (but necessarily so) AutoCAD implementations
(setq pi/2 (/ pi 2 )
  *acad-object* nil                ; 
  *active-document* nil            ; 
  *model-space* nil                ; 
  *paper-space* nil                ; 
)
(defun string-ReplaceChar ( Str Char1 Char2 / tempchar ii continue newstr)
  ;; DESCRIPTION: Replaces all occurences of one character in a string for another e.g. change c:/chris/ to c:\chris\
  (setq newstr "")
  (setq ii 0)
  (setq ii (1+ ii))
  (setq continue 1)
  (while continue
    (setq tempchar (substr Str ii 1))
    
    (if (= "" tempchar)
	(setq continue nil)
      (progn
        (if (= Char1 tempchar)
	    (setq tempchar Char2)
	  )
        (setq newstr (strcat newstr tempchar))
      
	)				;progn
      )					;if
    (setq ii (1+ ii))
    )					;while
  newstr
  )
;;UI<...............................................................................................................>
;; DESCRIPTION: returns a zero indexed integer equivilant to a letter. Upper and lower case are treated as the same
(defun string->NumberIndex ( str / return retval)
  (cond
   (( = str "0") 0)
   (( = str "1") 1)
   (( = str "2") 2)
   (( = str "3") 3)
   (( = str "4") 4)
   (( = str "5") 5)
   (( = str "6") 6)
   (( = str "7") 7)
   (( = str "8") 8)
   (( = str "9") 9)
   (t nil)
   )
  )
(defun string->LetterIndex ( str / return retval)
  (cond
   (( = (STD-TOUPPER (ascii str)) (ascii "A")) 0)
   (( = (STD-TOUPPER (ascii str)) (ascii "B")) 1)
   (( = (STD-TOUPPER (ascii str)) (ascii "C")) 2)
   (( = (STD-TOUPPER (ascii str)) (ascii "D")) 3)
   (( = (STD-TOUPPER (ascii str)) (ascii "E")) 4)
   (( = (STD-TOUPPER (ascii str)) (ascii "F")) 5)
   (( = (STD-TOUPPER (ascii str)) (ascii "G")) 6)
   (( = (STD-TOUPPER (ascii str)) (ascii "H")) 7)
   (( = (STD-TOUPPER (ascii str)) (ascii "I")) 8)
   (( = (STD-TOUPPER (ascii str)) (ascii "J")) 9)
   (( = (STD-TOUPPER (ascii str)) (ascii "K")) 10)
   (( = (STD-TOUPPER (ascii str)) (ascii "L")) 11)
   (( = (STD-TOUPPER (ascii str)) (ascii "M")) 12)
   (( = (STD-TOUPPER (ascii str)) (ascii "N")) 13)
   (( = (STD-TOUPPER (ascii str)) (ascii "O")) 14)
   (( = (STD-TOUPPER (ascii str)) (ascii "P")) 15)
   (( = (STD-TOUPPER (ascii str)) (ascii "Q")) 16)
   (( = (STD-TOUPPER (ascii str)) (ascii "R")) 17)
   (( = (STD-TOUPPER (ascii str)) (ascii "S")) 18)
   (( = (STD-TOUPPER (ascii str)) (ascii "T")) 19)
   (( = (STD-TOUPPER (ascii str)) (ascii "U")) 20)
   (( = (STD-TOUPPER (ascii str)) (ascii "V")) 21)
   (( = (STD-TOUPPER (ascii str)) (ascii "W")) 22)
   (( = (STD-TOUPPER (ascii str)) (ascii "X")) 23)
   (( = (STD-TOUPPER (ascii str)) (ascii "Y")) 24)
   (( = (STD-TOUPPER (ascii str)) (ascii "Z")) 25)
   (t nil)
   )
  )
(defun string->Chars (str / ii stringlist)
  (setq ii 0)
  (repeat (strlen str)
	  (setq ii (1+ ii))
	  (setq stringlist (append stringlist (list (substr str ii 1))))
	  )
  )
(defun string->ascii (str)
  (mapcar 'ascii (string->chars str))
  )
(defun SumOfFirstIn ( numToSum lst / index) ;superseded by math: functions
  ;; See MATH:SumOfFirstIn  
  (setq index 0)  
  (setq sum 0)
  
  (repeat numToSum 
	  (setq sum (+ sum (nth index lst) ) )
	  (setq index (1+ index) )    
	  )
  sum
  )
(defun SetNewLayer ( oldNameList newLayer penColor lineThickness lineType bPlot / itemfound num )
  ;; DESCRIPTION: Accepts a list of possible old names for the layer and replaces them with the new version
  (setq itemfound nil			;decide whether or not there is an old layer to be replaced
        num 0)
  
  (if (not (Layer-Existp newLayer))
      (command     ".-layer"
		   "Make"     newLayer
		   "lw"       lineThickness   ""
		   "lt"       lineType        ""
		   "color"    penColor        ""
		   "plot"     bPlot           ""
		   "" 
		   )
    )
  
  (foreach oldLayerName oldNameList
	   (layer-isolate (list oldLayerName newLayer))
	   (command "._change" (ssget "X") "" "pr" "la" newLayer "")
	   (command ".-purge" "la" oldLayerName "")
	   )				;foreach
    
  (princ)
  )
(defun getyn (PRMT DEF / OPT)
  ;; Gets a yes or no answer from user see authors explanation below
  ;;
  ;; (getyn) by Steve Smith
  ;; symbolic Yes or No entry
  ;; returns T if Yes | nil if No
  ;; USAGE:
  ;; (getyn <prompt> <def>)
  ;;
  (InitGet "Yes No")
  (If
    (=
      (If
        (Not
          (Setq OPT
            (GetKword
              (StrCat "\n" PRMT " "
                (If (= DEF "Yes")
                  "No/<Yes>? "
                  "Yes/<No>? "
                ) ; if
              ) ; strcat
            ) ; getkword
          ) ; setq
        ) ; not
        DEF
        OPT
      ) ; if
      "Yes"
    ) ; =
    T
    nil
  ) ; if
) ; defun
(defun br () (princ "\n") (princ))			;Used Like <br>
;<...............................................................................................................>
; DESCRIPTION: Takes a base string and adds an index to it
(defun GetNextName ( basename index / name)
  (setq name (strcat basename (itoa index)))
  )
(defun func(funcname)
  ;; Prints function name provided. Used for debugging
  (if *DEBUG*
      (progn
	(princ "\n******************************************************************************")
	(princ "\n***    ENTERING FUNCTION | ")
	(princ funcname)(princ "                             \n")
	)
    )
  )
(defun prinl ( saywhat var )
  ;; DESCRIPTION: Prints variable with a prompt on a new-line
  (princ saywhat)
  (princ " ")
  (princ var)
  (princ "\n")
  )
(defun dtr (a) (* pi (/ a 180.0))); Degrees to radians
(defun rtd (a) (* 180 (/ a pi)))  ; Radians to degrees
(defun SSCreate (eFirst / eNext ss)
  ;; DESCRIPTION: Gets all the entities in the drawing from the first entity created onwards!!
  (setq ss (ssadd eFirst))
  (setq eNext (entnext eFirst))
  
  (while eNext
    (ssadd eNext ss)
    (setq eNext (entnext eNext))
    )
  ss
  )
(defun PFLIST ( lst listname / num fhand str dashline)
  ;; DESCRIPTION: Prints a list to a file for later examination, USE WITH STDLIB
  (if (/= 0 *MBK_DEBUG_MODE*)
      (progn
	(setq fhand (open *DEBUG-FILE* "a"))
	(setq num 0)

	(setq dashline "")
	(repeat (strlen listname)
		(setq dashline (strcat dashline "+"))
		)
	(write-line dashline fhand)
	(write-line listname fhand)
	(write-line "" fhand)

	(repeat (length lst)
		(setq str (strcat (itoa num) "->  " (std-tostr (nth num lst))))
		(write-line str fhand)
		(setq num (1+ num))
		)
	(close fhand)
	(princ)
	)
    (princ)
    )
  )
(defun PFPlain ( str / )
  ;;   PRINT FILE FUNCTIONS
  ;;     
  ;;     Prints information to the debug file provided in the SEUPGLOBALS part of the
  ;;     LISP file.
  ;;     
  ;;     
  ;;<..............................................................................................................>
  ;; DESCRIPTION: Prints any string to the debug file. Cheating in a way but hey
  (if (/= 0 *MBK_DEBUG_MODE*)
      (progn
        (setq fhand (open *DEBUG-FILE* "a"))
        (write-line str fhand)
        (close fhand)
        (princ)
	)
    )
  (princ)

  )
(defun PF ( var varinfo / str)
  ;;<...............................................................................................................>
  ;; DESCRIPTION: Prints a line to the debug file, REQUIRES STDLIB
  (if (/= 0 *MBK_DEBUG_MODE*)
      (progn
        (setq fhand (open *DEBUG-FILE* "a"))
        (write-line (strcat "\n ° "varinfo "->   " (std-tostr var)) fhand)
        (close fhand)
        (princ)
	)
    )
  (princ)
  )
(defun UI-ProcessChoice ( str val datatype / str1 tempval return)
  ;;|______________________________________________________________________________________________________|;
  ;; DESCRIPTION: Process choices in a program. Used to set globals in UI manner

  (setq tempval val)
  (setq strval (STD-TOSTR val))
  
  (setq str1 (strcat "\n" str " [" strval "]: "))

  (cond
   ;; 'i' for int
   ( (= (StrToIndex datatype) 8)
     (setq val (getint str1))  
     )
    
   ;; 'r' for real
   ( (= (StrToIndex datatype) 17)
     (setq val (getreal str1))  
     )
    
   ;; 's' for string
   ( (= (StrToIndex datatype) 18)
     (setq val (getstring str1))  
     )
   )
  
  (if (or (= nil val) (= "" val))
      (progn
	(setq val tempval)
	)
    )
  
  (setq return val)
  )
(defun printitem (item index strsub)
  (princ (strcat "\n" strsub (itoa index) "->  " (std-tostr item)))
  (princ)
  )
;<...............................................................................................................>
; DESCRIPTION: A nicely formatted list of nested points. RECURSIVE FUNCTION
(defun PFPointList (ptlist Listinfo / fhand sHeaderinfo)
  (setq fhand (open *DEBUG-FILE* "a"))

  (setq totalstr "")
  (setq sHeaderInfo 
	(strcat 
	 "\n@=========================================================================================================@"
	 "\n@                                           ° " Listinfo "\n"
	 )
	)
  
  (write-line sHeaderInfo fhand)  
  
  (PFPL ptlist "" fhand)
  
  (write-line totalstr fhand)
  (close fhand)
  (princ)
  )
(defun PFPL (ptList strsub fhand / index)
  (setq index 1)
  (if (/= "" strsub)
      (setq strsub (strcat strsub "-"))
    )
  (foreach item ptList
	   (if (STD-POINTP item)
	       (pfitem item index strsub fhand)
	     (progn
	       (setq totalstr (strcat totalstr "\n" strsub (itoa index) "->  Branch.."))
	       (PFPL item (strcat "    " strsub (itoa index)) fhand)
	       )
	     )
	   (setq index (1+ index))
	   )
  (princ)
  )
(defun pfitem (item index strsub fhand / pts spaces)
  (setq pts "")
  (setq spaces "")
  (foreach num item
	   (setq spaces "")  
	   (setq sNum (rtos num 2 3))
	   (repeat (- 12 (strlen sNum))
		   (setq spaces (strcat spaces " "))
		   )
	   (setq pts (strcat pts sNum spaces))
	   )
  
  (setq totalstr (strcat totalstr "\n" strsub  (itoa index) "->  " pts))
  (princ)
  )
(defun savars (sv0 / x)
  ;; SAVARS.LSP - (c) 1988-97 Tee Square Graphics
  ;; Taken from the above file
  (setq svx1 nil
        olderr *error*)
  (foreach x sv0
	   (setq svx1 (append svx1 (list (list x (getvar x))))))
  (defun *error* (msg)
    (revars))
  )
(defun revars (/ x)
  (setq *error* olderr)
  (foreach x svx1
	   (setvar (car x)(cadr x)))
  (setq svx1 nil)
  (princ)
  )
(defun Loading ( filepath / ) (br) (princ "_$  Loading... ") (princ filepath) (load filepath) (princ));; DESCRIPTION: Prints a message before loading a file
(defun MBK_DEBUG_ON ( / ) (setq *MBK_DEBUG_MODE* 1))
(defun MBK_DEBUG_OFF ( / ) (setq *MBK_DEBUG_MODE* 0))
(defun NotBetween ( A C B / val return )
  ;; DESCRIPTION: Checks whether B value lies between A and C
  ;; See Math:NotBetween
  
  (setq val T)
  (if (and (>= A B) (<= C B)) (setq val nil))
  (if (and (<= A B) (>= C B)) (setq val nil))
  val
  )
(defun file-GetLength (filename / fhand )
  ;; DESCRIPTION: Gets the number of lines in a file
  ;; This function is DESTRUCTIVE so use it only on an un-open file.
  (setq ii 0)
  (setq fhand (open filename "r"))
  (while (read-line fhand)
    (setq ii (1+ ii))
    )
  (close fhand)
  ii
  )
(defun file-GetDetails ( str
			 /
			 tempchar newstr index continue path
			 )
  ;; DESCRIPTION: Accepts a full path and breaks it into path filename and extension
  (setq newstr "" index 0 continue 1)
  
  (while continue
    (setq tempchar (substr str (- (strlen str) index) 1))
    (if (= "/" tempchar)
	(progn
	  (setq continue nil)
	  (setq index (- (strlen str) index))
	  )				;progn
      (setq index (1+ index))
      )					;if
    )					;while
  
  (setq path (substr str 1 index))
  (setq str (substr str (1+ index)))

  (setq index (strlen str))
  (setq continue 1)
  (while continue
    (setq tempchar (substr str index 1))
    (if (= "." tempchar)
	(setq continue nil)
      (if (/= 1 index)
	  (setq index (1- index))
        (progn
          (setq continue nil)
          (setq index (1+ (strlen str)))
	  )
	)				;if
      )					;if
    )					;while

  (list path (substr str 1 (1- index)) (substr str (1+ index)))
  )
(defun getpoints ( ss / eget ii item ptlist ent)
  
  (setq ii 0)
  (repeat (sslength ss)
	  (setq ent (ssname ss ii))
	  (setq eget (entget ent))
	  (foreach item eget
		   (setq numitem (car item))
		   (if (or (= 10 numitem) (= 11 numitem) (= 12 numitem) (= 13 numitem))
		       (progn
			 (setq lastpoint (cdr item))
			 (setq ptlist (append ptlist (list lastpoint)))
			 )
		     )

		   ;; If a polyline contains arcs we need to get more detail from the arcs than what is contained in the polyline
		   (if (= 42 numitem)
		       (if (/= 0.0 (cdr item))
			   (progn
			     (setq found nil)
			     (while (not found)
			       (setq ii (1+ ii))
			       (if (or (= 10 numitem) (= 11 numitem) (= 12 numitem) (= 13 numitem))
				   (progn
				     (setq nextpoint (cdr item))
				     (setq ptlist (append ptlist (list nextpoint)))
				     )
				 )
			       )
			     (setq arcDetails (GetArcDetails lastpoint nextpoint (cdr item)))
			     )
			 )
		     )
      
		   )
	  (setq ii (1+ ii))
	  )
  
  ptlist				;RETURNING
  )
(defun GetArcDetails (lastpoint nextpoint fraction)
  (setq radius (/ (distance lastpoint nextpoint) 2))
  (setq ang (angle (pt-3d->2d lastpoint) (pt-3d->2d nextpoint)))
  (setq newang (+ (/ pi 2) ang))
  (setq cp (polar (pt-getmidpoint lastpoint nextpoint) newang (- 1.0 fraction)))
  
  (list cp startang endang radius)
  )
(defun string-InsertSpaces (num)
  ;; Insert any number of spaces into a file
  (setq spaces "")
  (repeat num
	  (setq spaces (strcat spaces " "))
	  )
  )
(defun string-Pad ( str totallen / len spaces)
  ;; DESCRIPTION: Pad characters to the end of a string so that the total length is equal to a given value
  (setq len (strlen str))
  (setq spaces (- totallen len))
  (setq str (strcat str (string-InsertSpaces spaces)))
  )
(defun string-Pad-Begin ( str totallen / len spaces)
  ;; DESCRIPTION: Pad characters to the end of a string so that the total length is equal to a given value
  (setq len (strlen str))
  (setq spaces (- totallen len))
  (setq str (strcat (string-InsertSpaces spaces) str))
  )
(defun MinVal (lst)  (apply 'min lst))
(defun MaxVal (lst)  (apply 'max lst))
(defun PrincOptions (titlestring lst btextscreen / bContinue nChoice ii)
  ;;Show items in a list on the textscreen
  (if btextscreen (textpage))
  (setq bContinue t)
         
  (while bContinue
    (prompt titlestring)
    (setq ii 0)
    (repeat (length lst)
	    (prompt (strcat "\n[" (itoa (1+ ii)) "]  " (std-tostr (nth ii lst))))
	    (setq ii (1+ ii))
	    )

    (setq nChoice (getint "\n\nEnter option [1]: "))
    
    (if (or (>  nChoice (length lst)) (< nChoice 1))
	(prompt "\nInvalid choice")
      (progn
        (if (= nChoice nil) (setq nChoice 1))
        (setq bContinue nil)
	)
      )
    )
  
  (prompt "\n")
  (nth (1- nChoice) lst)
  )
(defun even (num / h hf)
  ;; test if a number is even (However, does not test if it is odd)
  ;; looking back I'm not sure why I made this I'm sure there was a much better test if I looked around
  (setq h (/ num 2.0))
  (setq hf (fix h))
  (if (> (- h hf) 0) nil t)
  )
(defun Expression->String (item / retstr remstr ele)
  (setq retstr "")
  (if (atom item)
      (cond
       ((STRINGp item) (setq retstr (strcat "\"" item "\"" " ")))
       ((STD-REALP item)
	(if (wcmatch (std-tostr item) "*.*")
	    (setq retstr (strcat (std-tostr item) " "))
          (setq retstr (strcat (std-tostr item) ".0 "))
	  )
	)
       (T (setq retstr (strcat (std-tostr item) " ")))
       )
    (progn
      (if (std-dotted-pair-p item)
	  (progn
	    (setq retstr (strcat retstr "("))
	    (setq retstr (strcat retstr (Expression->String (car item))))
	    (setq retstr (strcat retstr ". "))
	    (setq retstr (strcat retstr (Expression->String (cdr item))))
	    (setq retstr (strcat retstr ")"))
	    )
        (progn
          (setq retstr (strcat retstr "("))
          (foreach ele item
		   (setq retstr (strcat retstr (Expression->String ele)))
		   )
          (setq retstr (strcat retstr ")"))
	  )
	)
      )
    )
  retstr
  )
(defun String->Expression (s / lst *error*)
  (setq *error* *std-error*)
  (progn
    (if (and (/= s "") (/= (std-firstchar s) ";"))
	(setq lst (cons (read (strcat "(" s ")")) lst))
      )
    )
  (caar (reverse lst))
  )
(defun Expression->String1 (item / retstr remstr ele)
  (setq retstr "")
  (if (atom item)
      (cond
       ((STRINGp item) (setq retstr (strcat "\"" item "\"" " ")))
       ((STD-REALP item)
	(if (wcmatch (std-tostr item) ".")
	    (setq retstr (strcat (std-tostr item) " "))
	  (setq retstr (strcat (std-tostr item) ".0 "))
	  )
	)
       (T (setq retstr (strcat (std-tostr item) " ")))
       )
    (progn
      (if (std-dotted-pair-p item)
	  (progn
	    (setq retstr (strcat retstr "("))
	    (setq retstr (strcat retstr (Expression->String (car item))))
	    (setq remstr (cdr item))
	    (if (atom remstr)
		(progn
		  (setq retstr (strcat retstr ". "))
		  (setq retstr (strcat retstr (Expression->String remstr)))
		  )
	      (foreach ele remstr
		       (setq retstr (strcat retstr (Expression->String ele)))
		       )
	      )
	    (setq retstr (strcat retstr ")"))
	    )
        (progn
          (setq retstr (strcat retstr "("))
          (foreach ele item
		   (setq retstr (strcat retstr (Expression->String ele)))
		   )
          (setq retstr (strcat retstr ")"))
	  )
	)
      )
    )
  retstr
  )
(defun ASSERT (value)
  (if (atom value)
      (if (not value)
	  (progn
	    (Alert "BAD ASSERTION! exiting")
	    (exit)
	    )
	)
    )
  )
(defun acad-object  ()
  (cond (*acad-object*)                 ; Return the cached object
        (t
         (setq *acad-object* (vlax-get-acad-object))
         )
        )
  )
(defun active-document  ()
  (cond (*active-document*)             ; Return the cached object
        (t
         (setq *active-document* (vla-get-activedocument (acad-object)))
         )
        )
  )
(defun model-space  ()
  (cond (*model-space*)                 ; Return the cached object
	(t
	 (setq *model-space* (vla-get-modelspace (active-document)))
	 )
	)
  )
(defun paper-space  ()
  (setq *paper-space* (vla-get-paperspace (active-document))) ; Don't cache paper-space cause problems
  )
(defun current-tab ()
  (if (/= "Model" (getvar "CTAB"))  (paper-space) (model-space))
  )
(defun string-GetDate (/ date)
  (setq date (std-date->datlst (getvar "date")))
  (strcat (itoa (THIRD date)) "/" (GetmonthOf (SECOND date) nil) "/" (itoa (FIRST date)))
  )
(defun string-GetTime (/ time hour )
  (setq time (strparse (dos_time) ":")
	hour (if (= "p" (substr (caddr time) 3 1)) (+ (atoi (car time)) 12) (atoi (car time)))
	time (strcat (itoa hour) ":" (cadr time) ":" (substr (caddr time) 1 2)))
  )
(defun RoundTo (val level)
  (setq val (* (float level) (std-round (/ (float val) (float level)))))
  )
(defun RoundUpTo (val level)
  (setq newval (RoundTo val level))
  (if (< newval val)
      (+ newval level)
    newval
    )
  )
(defun RoundDownTo (val level)
  (setq newval (RoundTo val level))
  (if (> newval val)
      (- newval level)
    newval
    )
  )
(defun GetMonthOf (mon bLongFormat / longnames);; mon is 1 indexed month
  (setq longnames '("JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE" "JULY" "AUGUST" "SEPTEMBER" "OCTOBER" "NOVEMBER" "DECEMBER"))
  (setq shortnames '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))
  (if (and (> mon 0) (< mon 13))
      (nth (1- mon) (if bLongFormat longnames shortnames))
    nil
    )
  )
(defun string-getendnum (str bRight
			     /
			     nums continue ii thisnum newnums)

  (if bRight
      (setq nums (reverse (string->ascii str)))
    (setq nums (string->ascii str))
    )
  (setq continue t ii 0)
  (while continue
    (setq thisnum (nth ii nums))
    (if (or (= thisnum 46) (and (>= thisnum 48) (<= thisnum 57)))
	(setq newnums (cons thisnum newnums))
      (setq continue nil)
      )
    (setq ii (1+ ii))
    )
  (if bRight
      (atof (apply 'strcat (mapcar 'chr newnums)))
    (atof (apply 'strcat (mapcar 'chr (reverse newnums))))
    )
  )
(defun getheight (/ d1 bFinished mv av)
  (if (not *lasthval*) (setq *lasthval* 100.0))
  (if (not *addval*) (setq *addval* 0.0))
  (if (not *multval*) (setq *multval* 1000.0))
  (if (not expr)
      (setq expr '(getdist (strcat "\nEnter height of point [<T>ext/<A>dd (" (rtos *addval* 2 2) "/<M>ultiple factor: default=(" (rtos *multval* 2 2) ")/" (rtos *lasthval* 2 1)  "] : ")))
    )
  (while (not bFinished)
    (initget 128)
    (setq d1 (eval expr))
    (if (= 'STR (type d1))
	(cond
	 ( (or (= "T" d1) (= "t" d1))
	   (setq expr '(progn (princ (strcat "\nEnter height of point [<T>ext/<A>dd (" (rtos *addval* 2 2) "/<M>ultiple factor: default=(" (rtos *multval* 2 2) ")/" (rtos *lasthval* 2 1)  "] : "))
			      (initget 128)
			      (setq val (car (entsel)))
			      (if val
				  (setq ret (TEXT-GETNUM val))))
		 )
	   )
	 ( (or (= "H" d1) (= "h" d1))
	   (setq expr '(getdist (strcat "\nEnter height of point [<T>ext/<A>dd (" (rtos *addval* 2 2) "/<M>ultiple factor: default=(" (rtos *multval* 2 2) ")/" (rtos *lasthval* 2 1)  "] : ")))
	   )
	 ( (or (= "M" d1) (= "m" d1))
	   (setq mv (getdist (strcat "\nEnter value for *multval* [" (rtos *multval*) "]: ")))
	   (if mv (setq *multval* mv))
	   )
	 ( (or (= "A" d1) (= "a" d1))
	   (setq av (getdist (strcat "\nEnter value for *addval* [" (rtos *addval*) "]: ")))
	   (if av (setq *addval* av))
	   )
	 (t (setq bFinished t) (setq ret (+ *ADDVAL* (* *multval* (atof d1) ))))
	 )
      (if d1
	  (setq bfinished t ret (setq ret (+ *ADDVAL* (* *multval* d1))))
        (setq expr '(getdist (strcat "\nEnter height of point [<T>ext/<A>dd (" (rtos *addval* 2 2) "/<M>ultiple factor: default=(" (rtos *multval* 2 2) ")/" (rtos *lasthval* 2 1)  "] : ")))
	)
      )
    )
  ret
  )
(defun ref ()
  (setvar "LASTPOINT" (getpoint "\nReference point: "))
  (getpoint "\nNext point: " (getvar "LASTPOINT"))
  )
(defun inters-2d (a1 a2 b1 b2) (INTERS (PT-3D->2D a1) (PT-3D->2D a2) (PT-3D->2D b1) (PT-3D->2D b2)))
;; These 2 functions work together *B_INIT sets value to zero and *B should be inserted at any point in the code to print a value higher than the last
(defun *B_INIT () (SETQ *MBK_BREAK_POINT* 0) (princ (strcat "\n===< BREAK " "INITIALISE" "  >===\n")))
(defun *B ()
  (if *MBK_BREAK_POINT* (setq *MBK_BREAK_POINT* (1+ *MBK_BREAK_POINT*)) (*B_INIT))
  (princ (strcat "\n===< BREAK " (itoa *MBK_BREAK_POINT*) "  >===\n"))
  )
(defun *PAUSE () (getstring "Press Enter to continue")) ;Pauses
(defun list-swap (lst i j / tmp)
  (setq tmp (nth j lst)
        lst (std-rplace lst j (nth i lst))
        lst (std-rplace lst i tmp)))
(defun load-stdlib-2k ()
  (if (not (member "stdlib15.arx" (arx)))
      (arxload (findfile "lib/stdlib15.arx"))
    )
  )
(defun between (lower val upper) (< lower val upper))
(defun vla-is-a (obj str)  (if (= str (vla-get-objectname obj)) t nil))
(defun UndoBegin ()   (vla-StartUndoMark (vla-get-activedocument (acad-object))));; Kevin Nehls
(defun UndoEnd ()   (vla-EndUndoMark (vla-get-activedocument (acad-object))))
;;Begin Tony Tanzillo - Thanks tony
;USAGE
;Then, you just call it with a list of the objects:
;(vla-ObjectList->Variant (list <vla-object>...))
;Or, if you want to pass a list of entity names:
;(vla-ObjectList->Variant   (mapcar 'vlax-ename->vla-Object  (list <ename>...) ))
(defun vla-ObjectList->Variant (ObjectList)
  (List->Variant
   vlax-VbObject
   ObjectList
   )
  )
(defun List->Variant (vtype inlist)
  (vlax-make-variant
   (vlax-safearray-fill
    (vlax-make-safearray
     vtype
     (cons 0 (1- (length inlist)))
     )
    inlist
    )
   )
  )
(defun stringlist-filter (filter lst / blk item items)
  (foreach item lst
	   (if (wcmatch (std-string-upcase item) (strcat "*" filter "*"))
	       (setq items (cons item items ))
	     )
	   )
  (reverse items)
  )
;;End Tony Tanzillo
(defun Dispinfo (func description)
  (princ (strcat (STD-STRING-UPCASE "\n_$ COMMAND NAME >  ") func "\n_$ Description  >  " (if (= description "") "!No description given!" description) "\n"))
  (princ)
  )
(defun lpadzero (str num) (STD-STRING-LEFT-PAD-CHAR str num "0"))
(princ)