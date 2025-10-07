;;; lang-graphql-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; GraphQL development configuration with LSP, indentation, and leader keybindings.

;;; Code:

(after! graphql-mode
  ;; Enable LSP in GraphQL buffers
  (add-hook 'graphql-mode-hook #'lsp)
  
  ;; Indentation level
  (setq graphql-indent-level 2)
  
  ;; Additional GraphQL settings
  (setq graphql-additional-headers '(("Authorization" . "Bearer ${TOKEN}")))
  (setq graphql-url "http://localhost:8080/graphql"))

;; Check if graphql-run-query function exists
(when (fboundp 'graphql-send-query)
  (defalias 'graphql-run-query 'graphql-send-query))

;; Configure GraphQL LSP
(use-package lsp-mode
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("graphql-lsp" "server" "-m" "stream"))
                    :major-modes '(graphql-mode)
                    :priority 1
                    :server-id 'graphql-lsp)))

(provide 'lang-graphql-config)

;;; lang-graphql-config.el ends here
