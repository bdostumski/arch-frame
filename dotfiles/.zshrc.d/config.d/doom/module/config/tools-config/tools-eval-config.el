;;; module/config/tools-config/tools-eval-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for inline evaluation of Emacs Lisp expressions.
;; Provides enhanced evaluation experience with overlays, keybindings, and visual feedback.

;;; Code:

(use-package! eval-sexp-fu
  :when (modulep! :tools eval)
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode)
  :config
  ;; Flash evaluation results for better visual feedback
  (setq eval-sexp-fu-flash-duration 0.5))

(after! eval
  ;; Enhanced evaluation display configuration
  (setq +eval-display-style 'overlay
        +eval-overlay-duration 5.0          ;; Show results for 5 seconds
        +eval-popup-min-height 10           ;; Minimum popup height
        +eval-popup-max-height 20))         ;; Maximum popup height

;; Custom evaluation functions
(defun eval-and-replace ()
  "Evaluate the preceding sexp and replace it with the result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun ielm-eval-last-sexp ()
  "Evaluate the last sexp in IELM."
  (interactive)
  (let ((sexp (elisp--preceding-sexp)))
    (with-current-buffer (get-buffer-create "*ielm*")
      (unless (eq major-mode 'inferior-emacs-lisp-mode)
        (ielm))
      (goto-char (point-max))
      (insert (format "%S" sexp))
      (ielm-send-input))))

;; Enhanced evaluation hooks and feedback
(after! elisp-mode
  ;; Show function signatures in minibuffer
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

  ;; Auto-highlight matching parentheses
  (add-hook 'emacs-lisp-mode-hook #'show-paren-mode)

  ;; Enable aggressive indentation for cleaner code
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; Pretty printing configuration
(after! pp
  (setq pp-default-function 'pp-to-string
        pp-max-width 80))

;; IELM (Interactive Emacs Lisp Mode) enhancements
(after! ielm
  (setq ielm-prompt "λ> "
        ielm-use-dynamic-return t)

  ;; Better history handling
  (add-hook 'ielm-mode-hook
            (lambda ()
              (setq-local comint-input-ring-size 1000)
              (setq-local comint-input-ignoredups t))))

;; Evaluation result formatting
(defun +eval/format-result (result)
  "Format evaluation RESULT for display."
  (cond
   ((stringp result) (format "\"%s\"" result))
   ((numberp result) (number-to-string result))
   ((symbolp result) (symbol-name result))
   ((listp result) (format "%S" result))
   (t (format "%S" result))))

;; Performance optimization for large evaluations
(setq max-lisp-eval-depth 10000
      max-specpdl-size 13000)

;; Visual enhancements for evaluation
(when (modulep! :ui popup)
  (set-popup-rule! "^\\*eval"
    :size 0.3 :side 'bottom :select t :quit t))

(provide 'tools-eval-config)

;;; tools-eval-config.el ends here
