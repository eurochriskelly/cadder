(setq *dir* (strcat (getenv "HOME") "/Workspace/repos/cadder/src/"))
(load (strcat *dir* "x-lines.lsp"))
(load (strcat *dir* "stash.lsp"))
(load (strcat *dir* "utils/offsets.lsp"))

;; convenience function to re-load cadder.lsp
(defun c:src ()
  (setq fileName (strcat *dir* "cadder.lsp")) ; Define the relative path for "cadder.lsp"
  (if (findfile fileName)
    (progn
      (load fileName)
      (princ (strcat "\nLoaded " fileName)))
    (princ (strcat "\nFile " fileName " not found")))
)
(princ "\nLoaded cadder.lsp")