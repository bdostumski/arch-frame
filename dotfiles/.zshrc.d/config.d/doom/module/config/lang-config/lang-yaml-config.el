;;; module/config/lang-config/lang-yaml-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; YAML development configuration for Doom Emacs.
;; Enables LSP in YAML buffers, sets indentation, and provides leader keybindings
;; for formatting, navigation, and validation.

;;; Code:

(after! yaml-mode
  ;; Enable LSP in YAML buffers
  (add-hook 'yaml-mode-hook #'lsp!)

  ;; Indentation settings
  (setq yaml-indent-offset 2)
  
  ;; Enable format on save (optional)
  (add-hook 'yaml-mode-hook #'(lambda ()
                                (add-hook 'before-save-hook #'lsp-format-buffer nil 'local))))

;; Configure YAML language server
(after! lsp-mode
  (setq lsp-yaml-schema-store-enable t)  ;; Enable JSON schema store
  (setq lsp-yaml-format-enable t))       ;; Enable formatting

(provide 'lang-yaml-config)

;;; lang-yaml-config.el ends here
