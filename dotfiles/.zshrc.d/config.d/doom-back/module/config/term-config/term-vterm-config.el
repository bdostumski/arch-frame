;;; module/config/term-config/term-vterm-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Vterm configuration for Doom Emacs.
;; Sets a large scrollback and provides a quick keybinding to open Vterm.

;;; Code:

(after! vterm
  ;; Keep a large scrollback buffer
  (setq vterm-max-scrollback 10000))

(provide 'term-vterm-config)

;;; term-vterm-config.el ends here
