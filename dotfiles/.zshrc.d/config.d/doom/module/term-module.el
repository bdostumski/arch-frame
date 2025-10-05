;;; module/term-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module manages terminal integration within Emacs.
;; It ensures you have both lightweight and full-featured terminal options.
;;
;; Load order rationale for safety:
;; 1. Eshell - built-in Emacs shell, lightweight and Lisp-native.
;;    Provides basic shell functionality and is safe to load first.
;; 2. vterm - a fast, full-featured terminal emulator.
;;    Depends on libraries and environment support; loaded after Eshell
;;    to ensure minimal conflicts and better startup safety.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Eshell configuration
;; ---------------------------------------------------------------------------
;; Lightweight shell integrated in Emacs.
;; Offers deep Emacs integration, scripting, and customization.
(load! "config/term-config/term-eshell-config.el")

;; ---------------------------------------------------------------------------
;; 2. vterm configuration
;; ---------------------------------------------------------------------------
;; Full-featured terminal emulator.
;; Requires system libraries (libvterm) and provides near-native terminal
;; performance inside Emacs.
(load! "config/term-config/term-vterm-config.el")

(provide 'term-module)

;;; term-module.el ends here
