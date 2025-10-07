;;; module/config/lang-config/lang-lua-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Lua configuration for Doom Emacs.
;; Provides:
;;  - Proper indentation
;;  - Leader keybindings for evaluating buffer/region and starting REPL

;;; Code:

(after! lua-mode
  ;; Set indentation level
  (setq lua-indent-level 2))

(provide 'lang-lua-config)

;;; lang-lua-config.el ends here
