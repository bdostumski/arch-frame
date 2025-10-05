;;; module/config/lang-config/lang-emacs-lisp-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs Lisp development configuration with optional LSP support and convenient leader keybindings.

;;; Code:

(after! emacs-lisp-mode
  ;; Enable LSP in Emacs Lisp buffers
  (add-hook 'emacs-lisp-mode-hook #'lsp)
  ;; Set indentation level
  (setq lisp-indent-offset 2))

;; Leader keybindings for Emacs Lisp
;;(map! :leader
;;      (:prefix-map ("e" . "elisp")
;;       :desc "Evaluate buffer"       "b" #'eval-buffer
;;       :desc "Evaluate expression"   "e" #'eval-last-sexp
;;       :desc "Evaluate region"       "r" #'eval-region
;;       :desc "Describe function"     "f" #'describe-function
;;       :desc "Describe variable"     "v" #'describe-variable))

(provide 'lang-emacs-lisp-config)

;;; lang-emacs-lisp-config.el ends here
