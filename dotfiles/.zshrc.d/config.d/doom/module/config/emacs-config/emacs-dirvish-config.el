;;; module/config/emacs-config/emacs-dirvish-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Dirvish configuration with quick access entries and preview dispatchers.

;;; Code:

(use-package! dirvish
  :after dired
  :init
  ;; Override standard Dired with Dirvish
  (dirvish-override-dired-mode)
  :custom
  ;; Quick access shortcuts
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Documents/" "Documents")
     ("w" "~/Workspace/" "Workspace")))
  ;; File preview handlers
  (dirvish-preview-dispatchers
   '(image gif video audio epub pdf archive)))

(provide 'emacs-dirvish-config)

;;; emacs-dirvish-config.el ends here
