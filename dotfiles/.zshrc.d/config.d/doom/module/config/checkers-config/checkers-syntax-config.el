;;; module/config/checkers-config/checkers-syntax-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Flycheck configuration with optional posframe integration for syntax checking.
;; Only enables Flycheck in text modes and org-mode, not programming modes.
;; Runs checks on save or mode activation, shows errors in popups, and provides
;; convenient leader keybindings for navigation.

;;; Code:

;; --------------------------------------
;; Only enable Flycheck in text and specific modes (like org)
;; --------------------------------------
(defun checkers-syntax--maybe-enable-flycheck ()
  "Enable Flycheck only in text or specified non-programming modes."
  (when (or (derived-mode-p 'text-mode)
            (derived-mode-p 'org-mode)
            (derived-mode-p 'markdown-mode)
            (derived-mode-p 'rst-mode))
    (flycheck-mode 1)))

(add-hook 'after-change-major-mode-hook #'checkers-syntax--maybe-enable-flycheck)

;; Ensure Flycheck is *not* enabled in programming modes
(defun checkers-syntax--disable-flycheck-in-prog ()
  "Disable Flycheck in prog modes."
  (when (derived-mode-p 'prog-mode)
    (flycheck-mode -1)))

(add-hook 'prog-mode-hook #'checkers-syntax--disable-flycheck-in-prog)

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
