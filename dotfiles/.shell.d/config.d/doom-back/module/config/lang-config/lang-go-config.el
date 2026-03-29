;;; module/config/lang-config/lang-go-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Go development configuration with LSP, auto-formatting, and leader keybindings.

;;; Code:

(after! go-mode
  ;; Enable LSP in Go buffers
  (add-hook 'go-mode-hook #'lsp)
  
  ;; Use goimports instead of gofmt for better import management
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  
  ;; Configure go test
  (setq go-test-args "-v")
  
  ;; Configure LSP for Go
  (after! lsp-mode
    (setq lsp-go-analyses
          '((fieldalignment . t)
            (nilness . t)
            (unusedparams . t)
            (unusedwrite . t)
            (useany . t)))
    
    ;; Configure gopls
    (setq lsp-go-hover-kind "SynopsisDocumentation")
    (setq lsp-go-link-target "pkg.go.dev")
    (setq lsp-go-codelenses
          '((gc_details . nil)
            (generate . t)
            (regenerate_cgo . t)
            (test . t)
            (tidy . t)
            (upgrade_dependency . t)
            (vendor . t)))
    
    ;; Enable documentation on hover
    (setq lsp-eldoc-enable-hover t)))

;; Install and set up delve (debugger) integration if dap-mode is available
(when (modulep! :tools debugger)  ; Changed from featurep! to modulep!
  (after! dap-mode
    (require 'dap-go)
    (dap-go-setup)))

;; Add company-go as a backend for company-mode (code completion)
(after! company
  (set-company-backend! 'go-mode '(company-go :with company-yasnippet)))

(provide 'lang-go-config)

;;; lang-go-config.el ends here
