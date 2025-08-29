;;; module/term-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module manages terminal integration inside Emacs.
;; It includes:
;;   - Eshell: Emacs’ built-in shell, highly customizable and Lisp-native
;;   - vterm: A fast and modern terminal emulator inside Emacs

;;; Code:

;; Eshell configuration (lightweight, Lisp-powered shell)
(load! "config/term-config/term-eshell-config.el")

;; vterm configuration (full terminal emulator for compatibility)
(load! "config/term-config/term-vterm-config.el")

(provide 'term-module)

;;; term-module.el ends here
