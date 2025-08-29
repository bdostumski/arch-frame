;;; module/config/lang-config/lang-sh-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shell script (sh) configuration for Doom Emacs.
;; Enables LSP support and sets indentation preferences,
;; with leader keybindings for running, linting, and navigation.

;;; Code:

(after! sh-mode
  ;; Enable LSP for shell buffers
  (add-hook 'sh-mode-hook #'lsp)

  ;; Set basic indentation
  (setq sh-basic-offset 2))

;; ----------------------------
;; Leader keybindings for sh-mode
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("s" . "shell")
;;       :desc "Run buffer"       "r" #'sh-run-current-buffer
;;       :desc "Lint buffer"      "l" #'lsp-diagnostics
;;       :desc "Go to definition" "d" #'lsp-find-definition))

(provide 'lang-sh-config)

;;; lang-sh-config.el ends here
