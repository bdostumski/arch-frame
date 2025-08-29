;;; configurations/emacs-config.el --- Dired and Emacs Behavior Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; General Emacs behavior configuration, including Dired enhancements.

;;; Code:

;; ----------------------------------------
;; Dirvish: Modern Dired Replacement
;; ----------------------------------------

(use-package! dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode))

;;; emacs-config.el ends here
