;;; module/emacs-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module configures Emacs' core behavior and built-in tools.
;; It includes:
;;   - Dirvish: Modern file manager built on top of Dired
;;   - Electric: Automatic insertion/management of paired characters
;;   - EWW: Emacs Web Wowser, a text-based web browser
;;   - Ibuffer: Advanced buffer management and filtering
;;   - Undo: Enhanced undo/redo system
;;   - VC: Emacs’ built-in version control integration

;;; Code:

;; File management with Dirvish (improved Dired)
(load! "config/emacs-config/emacs-dirvish-config.el")

;; Electric pair management
(load! "config/emacs-config/emacs-electric-config.el")

;; EWW web browser integration
(load! "config/emacs-config/emacs-eww-config.el")

;; Ibuffer for advanced buffer management
(load! "config/emacs-config/emacs-ibuffer-config.el")

;; Undo system configuration
(load! "config/emacs-config/emacs-undo-config.el")

;; Version control integration
(load! "config/emacs-config/emacs-vc-config.el")

(load! "config/emacs-config/emacs-ranger-config.el")

(load! "config/emacs-config/emacs-yasnippet-config.el")

(provide 'emacs-module)

;;; emacs-module.el ends here
