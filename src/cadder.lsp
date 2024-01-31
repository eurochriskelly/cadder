(setq *dir* (strcat (getenv "HOME") "/Workspace/repos/cadder/src/"))

;; load all files in the file-list
(defun load-files (file-list) 
  (foreach file file-list 
    (setq file-path (strcat *dir* file ".lsp"))
    (if (findfile file-path) 
      (progn 
        (princ (strcat "\nLoading library [" file-path "]... "))
        (load file-path)
        (princ "done")
      )
      (princ (strcat "\nFile " file-path " not found"))
    )
  )
)

(setq file-list '("utils/generic" "utils/drawing"
                  "utils/offsets" "utils/utils"
                  "utils/points" "utils/wkt"
                  "x-lines" "stash"
                 )
)
(load-files file-list)

;; convenience function to re-load cadder.lsp
(defun c:src () 
  (setq fileName (strcat *dir* "cadder.lsp")) ; Define the relative path for "cadder.lsp"
  (if (findfile fileName) 
    (progn 
      (load fileName)
      (princ (strcat "\nLoaded " fileName))
    )
    (princ (strcat "\nFile " fileName " not found"))
  )
)
(princ "\nLoaded cadder.lsp")