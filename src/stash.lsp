;; Stash.lsp
;; 
;; Stash works similiar to the stash command in git. It allows you to
;;

;; Stash the current selection set as a block
(defun c:stash ()
  (princ "\n ========== STASHING ===========")
  (setq selset (ssget))
  (if selset
    (progn
      (setq maxnum 1)
      (setq blkdef (tblnext "BLOCK" T))
      (while blkdef
        (setq nextblk (strcase (cdr (assoc 2 blkdef)) T))
        (if (wcmatch nextblk "stashed-*")
          (progn
            (setq stashnum (atoi (substr (cdr (assoc 2 blkdef)) 9)))
            (if (> stashnum maxnum)
              (setq maxnum stashnum)
            )
          )
        )
        (setq blkdef (tblnext "BLOCK"))
      )
      (setq maxnum (+ maxnum 1))
      (setq blkname (strcat "stashed-" (itoa maxnum)))
      (command "_-block" blkname "0,0" selset "" "")
      (command "_.erase" selset "" "")
    )
    (princ "\nNo entities selected.")
  )
  (princ)
)


;; Like git stash pop, this will pop the last stashed block and explode it
(defun c:stashpop ()
  (setq maxnum 0 blkname "")
  (setq blkdef (tblnext "BLOCK" T))
  (while blkdef
    (prompt (strcat "\nChecking block: " (cdr (assoc 2 blkdef)))) ; Debugging print
    (if (wcmatch (strcase (cdr (assoc 2 blkdef)) T) "STASHED-*")
      (progn
        (prompt (strcat "\nStashed block found: " (cdr (assoc 2 blkdef)))) ; Debugging print
        (if (> (atoi (substr (cdr (assoc 2 blkdef)) 8)) maxnum)
          (progn
            (setq maxnum (atoi (substr (cdr (assoc 2 blkdef)) 8)))
            (setq blkname (cdr (assoc 2 blkdef)))
          )
        )
      )
    )
    (setq blkdef (tblnext "BLOCK"))
  )
  (prompt (strcat "\nMost recent stashed block: " blkname)) ; Debugging print
  (if (/= blkname "")
    (progn
      (command "_insert" blkname "0,0" 1 1 0)
      (command "_explode" (entlast))
      (command "_-purge" "_B" blkname "_N")
    )
    (prompt "\nNo stashed blocks found.")
  )
  (princ)
)


