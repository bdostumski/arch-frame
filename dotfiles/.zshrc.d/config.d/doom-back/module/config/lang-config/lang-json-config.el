;;; module/config/lang-config/lang-json-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; JSON setup with LSP, formatting, and convenient keybindings.

;;; Code:

;; Ensure json-reformat package is installed
(use-package json-reformat
  :ensure t)

(after! json-mode
  ;; Enable LSP in JSON buffers
  (add-hook 'json-mode-hook #'lsp)

  ;; Formatting options
  (setq js-indent-level 2
        json-reformat:indent-width 2))

(provide 'lang-json-config)

;;; lang-json-config.el ends here
