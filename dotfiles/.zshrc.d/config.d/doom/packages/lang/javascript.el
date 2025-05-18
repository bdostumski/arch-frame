;;; packages/lang/javascript.el -*- lexical-binding: t; -*-

;; Projectile project path
(after! projectile
  (setq projectile-project-search-path '("~/Workspace/")))

;; Configure prettier and eslint in apheleia to use local binaries first
(after! apheleia
  (setq apheleia-formatters
        (append
         `((prettier . (, (local-node-bin "prettier") "--stdin-filepath" filepath))
           (eslint . (, (local-node-bin "eslint") "--fix" "--stdin" "--stdin-filename" filepath)))
         apheleia-formatters))

  (set-formatter! 'javascript-mode 'prettier)
  (set-formatter! 'typescript-mode 'prettier)
  (set-formatter! 'web-mode 'prettier)
  (set-formatter! 'json-mode 'prettier))
