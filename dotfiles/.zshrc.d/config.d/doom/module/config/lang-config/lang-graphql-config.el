;;; module/config/lang-config/lang-graphql-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; GraphQL development configuration with LSP, indentation, and leader keybindings.

;;; Code:

(after! graphql-mode
  ;; Enable LSP in GraphQL buffers
  (add-hook 'graphql-mode-hook #'lsp)
  ;; Indentation level
  (setq graphql-indent-level 2))

;; Leader keybindings for GraphQL
;;(map! :leader
;;      (:prefix-map ("q" . "graphql")
;;       :desc "Execute query"       "e" #'graphql-run-query
;;       :desc "Jump to definition"  "d" #'lsp-find-definition
;;       :desc "Hover docs"          "h" #'lsp-hover))

(provide 'lang-graphql-config)

;;; lang-graphql-config.el ends here
