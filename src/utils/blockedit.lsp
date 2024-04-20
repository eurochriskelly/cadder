(defun c:blockattup()
  (setq fileName "/Users/chkelly/Workspace/issues/stories/230047_metro-13/blocks.csv") ; Update this to the path of your CSV file
  (setq csvData (read-csv-to-list fileName)) ; You'll need to implement read-csv-to-list

  (setq en (car (entsel "\nSelect block: ")))
  (setq ent (entget en))
  (setq nodeName (get-attribute-value ent "NODE_NAME"))
  
  (setq rowData (find-csv-row csvData nodeName))
  
  (foreach attr rowData
    (update-attribute en (car attr) (cdr attr))
  )
  (princ)
)

(defun read-csv-to-list (fileName)
  (setq f (open fileName "r"))
  (setq data (list))
  (while (setq line (read-line f))
    (setq data (cons (parse-csv-line line) data))
  )
  (close f)
  (reverse data)
)

(defun parse-csv-line (line)
  (setq fields (list))
  (setq field "")
  (setq inQuotes nil)
  (foreach ch (append line nil) ; Convert string to list of chars
    (cond
     ((= ch 44) ; comma
       (if inQuotes
         (setq field (strcat field (chr ch)))
         (progn
           (setq fields (cons field fields))
           (setq field "")
         )
       )
     )
     ((= ch 34) ; double quotes
       (setq inQuotes (not inQuotes))
     )
     (t
       (setq field (strcat field (chr ch)))
     )
    )
  )
  (setq fields (cons field fields)) ; Add the last field
  (reverse fields)
)

(defun find-csv-row (csvData nodeName)
  (setq result nil)
  (foreach row csvData
    (if (= (nth 0 row) nodeName)
      (setq result row)
    )
  )
  result
)

(defun update-attribute (block attribName attribValue)
  (setq atts (entnext block))
  (while atts
    (setq att (entget atts))
    (if (= (cdr (assoc 2 att)) attribName)
      (progn
        (entmod (subst (cons 1 attribValue) (assoc 1 att) att))
        (entupd atts)
      )
    )
    (setq atts (entnext atts))
  )
)
