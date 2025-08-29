;;; module/config/checkers-config/checkers-syntax-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Flycheck configuration with optional posframe integration for syntax checking.
;; Runs checks on save or mode activation, shows errors in popups, and provides
;; convenient leader keybindings for navigation.

;;; Code:

;; ----------------------------
;; Flycheck core configuration
;; ----------------------------
(after! flycheck
  ;; Run checks only on save or when mode enabled (avoid per-keystroke checks)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3))

;; ----------------------------
;; Flycheck Posframe (popup error display)
;; ----------------------------
(use-package! flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'point)         ;; Position of popup
  (flycheck-posframe-border-width 1)
  :config
  (custom-set-faces!
    '(flycheck-posframe-face         :foreground "#ECEFF4")
    '(flycheck-posframe-border-face  :background "#4B5263")))

;; ----------------------------
;; Keybindings for Flycheck
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("c" . "check")
;;       :desc "List errors" "x" #'flycheck-list-errors
;;       :desc "Next error" "n" #'flycheck-next-error
;;       :desc "Previous error" "p" #'flycheck-previous-error))

(provide 'checkers-syntax-config)

;;; checkers-syntax-config.el ends here
