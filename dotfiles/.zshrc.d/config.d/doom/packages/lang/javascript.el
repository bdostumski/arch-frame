;;; packages/lang/javascript.el -*- lexical-binding: t; -*-

;;; packages/lang/javascript.el -*- lexical-binding: t; -*-
;; ... projectile config ...

;; Helper for local node binaries
(defun local-node-bin (bin)
  "Return path to local BIN (from node_modules/.bin) if it exists, else just BIN."
  (let* ((root (or (locate-dominating-file default-directory "node_modules/.bin/")
                   (projectile-project-root)
                   default-directory))
         (local-bin (expand-file-name (concat "node_modules/.bin/" bin) root)))
    (if (and local-bin (file-exists-p local-bin))
        local-bin
      bin)))

(after! apheleia
  (setq apheleia-formatters
        (append
         `((prettier . (,(local-node-bin "prettier") "--stdin-filepath" filepath))
           (eslint . (,(local-node-bin "eslint") "--fix" "--stdin" "--stdin-filename" filepath)))
         apheleia-formatters))

  (set-formatter! 'javascript-mode 'prettier)
  (set-formatter! 'typescript-mode 'prettier)
  (set-formatter! 'web-mode 'prettier)
  (set-formatter! 'json-mode 'prettier))
