;;; module/config/checkers-config/checkers-syntax-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Flycheck configuration with optional posframe integration for syntax checking.
;; Only enables Flycheck in text modes and org-mode, not programming modes.
;; Enhanced with better mode detection and performance optimizations.

;;; Code:

;; Define modes where Flycheck should be enabled
(defcustom checkers-syntax-enabled-modes
  '(text-mode org-mode markdown-mode rst-mode latex-mode)
  "List of modes where Flycheck should be enabled."
  :type '(repeat symbol)
  :group 'checkers-syntax)

;; Define modes where Flycheck should be explicitly disabled
(defcustom checkers-syntax-disabled-modes
  '(prog-mode fundamental-mode)
  "List of modes where Flycheck should be disabled."
  :type '(repeat symbol)
  :group 'checkers-syntax)

;; --------------------------------------
;; Smart Flycheck enablement
;; --------------------------------------
(defun checkers-syntax--should-enable-p ()
  "Check if Flycheck should be enabled in current buffer."
  (and (not (minibufferp))
       (not (derived-mode-p 'help-mode))
       (or (cl-some #'derived-mode-p checkers-syntax-enabled-modes)
           (and (not (cl-some #'derived-mode-p checkers-syntax-disabled-modes))
                (derived-mode-p 'text-mode)))))

(defun checkers-syntax--maybe-enable-flycheck ()
  "Enable Flycheck only in appropriate modes."
  (when (checkers-syntax--should-enable-p)
    (flycheck-mode 1)))

;; Use a single hook for better performance
(add-hook 'after-change-major-mode-hook #'checkers-syntax--maybe-enable-flycheck)

;; Ensure Flycheck is disabled in programming modes (safety net)
(defun checkers-syntax--disable-flycheck-in-prog ()
  "Disable Flycheck in programming modes."
  (when (and flycheck-mode (derived-mode-p 'prog-mode))
    (flycheck-mode -1)))

(add-hook 'prog-mode-hook #'checkers-syntax--disable-flycheck-in-prog)

;; ----------------------------
;; Enhanced Flycheck configuration
;; ----------------------------
(use-package! flycheck
  :defer t
  :config
  ;; Performance optimizations
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change)
        flycheck-idle-change-delay 2.0           ;; Wait 2s before checking
        flycheck-display-errors-delay 0.5        ;; Show errors after 0.5s
        flycheck-highlighting-mode 'symbols      ;; Highlight symbols instead of lines
        flycheck-indication-mode 'left-fringe    ;; Show indicators in left fringe
        flycheck-emacs-lisp-load-path 'inherit   ;; Inherit load path
        flycheck-global-modes '(not prog-mode))  ;; Disable globally in prog modes

  ;; Custom error navigation with better feedback
  (defun checkers-syntax-next-error-with-message ()
    "Go to next error and show its message."
    (interactive)
    (flycheck-next-error)
    (when-let ((errors (flycheck-overlay-errors-at (point))))
      (message "%s" (flycheck-error-message (car errors)))))

  (defun checkers-syntax-previous-error-with-message ()
    "Go to previous error and show its message."
    (interactive)
    (flycheck-previous-error)
    (when-let ((errors (flycheck-overlay-errors-at (point))))
      (message "%s" (flycheck-error-message (car errors)))))

  ;; Function to toggle Flycheck
  (defun checkers-syntax-toggle ()
    "Toggle Flycheck mode."
    (interactive)
    (if flycheck-mode
        (flycheck-mode -1)
      (if (checkers-syntax--should-enable-p)
          (flycheck-mode 1)
        (message "Flycheck not enabled in this mode"))))

  ;; Clear all errors
  (defun checkers-syntax-clear-errors ()
    "Clear all Flycheck errors in current buffer."
    (interactive)
    (flycheck-clear)))

;; ----------------------------
;; Enhanced Flycheck Posframe
;; ----------------------------
(use-package! flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-position 'window-bottom-left-corner
        flycheck-posframe-border-width 2
        flycheck-posframe-inhibit-functions
        '((lambda (&rest _) (bound-and-true-p company-backend))))

  ;; Better styling
  (custom-set-faces!
    '(flycheck-posframe-face :inherit tooltip)
    '(flycheck-posframe-border-face :background "#4B5263")
    '(flycheck-posframe-info-face :foreground "#88C0D0")
    '(flycheck-posframe-warning-face :foreground "#EBCB8B")
    '(flycheck-posframe-error-face :foreground "#BF616A"))

  ;; Don't show posframe for certain error types
  (setq flycheck-posframe-error-prefix "⚠ "
        flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "ℹ "))

(provide 'checkers-syntax-config)

;;; checkers-syntax-config.el ends here
