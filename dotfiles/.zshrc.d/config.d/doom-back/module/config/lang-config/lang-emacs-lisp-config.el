;;; lang-emacs-lisp-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs Lisp development configuration with convenient development tools and keybindings.

;;; Code:

(after! emacs-lisp-mode
  ;; Set indentation level
  (setq lisp-indent-offset 2)
  
  ;; Add helpful packages for Emacs Lisp development
  (use-package eldoc
    :hook (emacs-lisp-mode . eldoc-mode)
    :config
    (setq eldoc-idle-delay 0.1))
  
  ;; Enhance variable and function descriptions
  (use-package helpful
    :commands (helpful-callable helpful-variable helpful-key helpful-command)
    :bind
    ([remap describe-function] . helpful-callable)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key)))

;; Enable syntax checking
(after! flycheck
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode))

;; Enable code completion
(after! company
  (add-hook 'emacs-lisp-mode-hook #'company-mode))

;; Add structural editing support
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(provide 'lang-emacs-lisp-config)

;;; lang-emacs-lisp-config.el ends here
