;;; module/config/lang-config/lang-yaml-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; YAML development configuration for Doom Emacs.
;; Enables LSP in YAML buffers, sets indentation, and provides leader keybindings
;; for formatting, navigation, and validation.

;;; Code:

(after! yaml-mode
  ;; Enable LSP in YAML buffers
  (add-hook 'yaml-mode-hook #'lsp)

  ;; Indentation settings
  (setq yaml-indent-offset 2))

;; ----------------------------
;; Leader keybindings for YAML
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("y" . "yaml")
;;       :desc "Format buffer"      "f" #'lsp-format-buffer
;;       :desc "Go to definition"   "d" #'lsp-find-definition
;;       :desc "Validate buffer"    "v" #'lsp-diagnostics))

(provide 'lang-yaml-config)

;;; lang-yaml-config.el ends here
