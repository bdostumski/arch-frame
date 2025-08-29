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

;; Leader keybindings
;;(map! :leader
;;      (:prefix-map ("l" . "lua")
;;       :desc "Evaluate buffer" "b" #'lua-send-buffer
;;       :desc "Evaluate region" "r" #'lua-send-region
;;       :desc "Run REPL" "s" #'lua-start-process))

(provide 'lang-lua-config)

;;; lang-lua-config.el ends here
