;;; configurations/emacs-config.el --- Dired and Emacs Behavior Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; General Emacs behavior configuration, including Dired enhancements.

;;; Code:

;; ----------------------------------------
;; Dirvish: Modern Dired Replacement
;; ----------------------------------------

(use-package! dirvish
  :after dired
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("d" "~/Documents/" "Documents")
          ("w" "~/Workspace/" "Workspace")))
  (setq dirvish-preview-dispatchers
        '(image gif video audio epub pdf archive)))

(use-package! treemacs
  :config
  (setq treemacs-width 30
        treemacs-is-never-other-window t
        treemacs-follow-after-init t))

(use-package! ranger
  :commands ranger)  ;; call manually with M-x ranger

;;; emacs-config.el ends here
