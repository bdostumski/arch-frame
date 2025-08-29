;;; module/config/emacs-config/emacs-electric-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized electric indentation for Doom Emacs.

;;; Code:

;; Enable electric-indent-mode globally (affects all buffers)
(electric-indent-mode +1)

;; Optional: ensure local electric indent for programming and Emacs Lisp modes
(dolist (hook '(prog-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'electric-indent-local-mode))

(provide 'emacs-electric-config)

;;; emacs-electric-config.el ends here
