;;; configurations/tools-config.el --- Tools and LSP Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for development tools, LSP, testing, and Git integration.

;;; Code:

;; ----------------------------------------
;; Tree-sitter
;; ----------------------------------------

(use-package! tree-sitter
  :hook (prog-mode . tree-sitter-mode))

(use-package! tree-sitter-langs
  :after tree-sitter)

;; ----------------------------------------
;; Clojure Development
;; ----------------------------------------

(use-package! clojure-mode
  :mode "\\.clj\\'")

(use-package! cider
  :hook (clojure-mode . cider-mode))

;; ----------------------------------------
;; LSP Mode and Extensions
;; ----------------------------------------

(use-package! lsp-mode
  :commands lsp
  :hook ((js-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp)
         (php-mode . lsp)
         (ruby-mode . lsp)
         (html-mode . lsp)
         (css-mode . lsp)
         (scss-mode . lsp)
         (less-css-mode . lsp)
         (json-mode . lsp)
         (yaml-mode . lsp)
         (markdown-mode . lsp)
         (lua-mode . lsp)
         (sql-mode . lsp)
         (dockerfile-mode . lsp)
         (graphql-mode . lsp)
         (elixir-mode . lsp)
         (haskell-mode . lsp)
         (kotlin-mode . lsp)
         (terraform-mode . lsp)
         (sh-mode . lsp)
         (xml-mode . lsp))
  :config
  (setq lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-prefer-flymake nil))

;; LSP UI enhancements
(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil))

;; ----------------------------------------
;; Debug Adapter Protocol
;; ----------------------------------------

(use-package! dap-mode
  :after lsp-mode
  :config
  ;; Enable UI features
  (require 'dap-ui)
  (dap-ui-mode 1)

  ;; Language-specific setup
  (require 'dap-node)
  (require 'dap-chrome)
  (require 'dap-firefox))

;; Optional: enable tooltips and controls
(use-package! dap-ui
  :after dap-mode
  :config
  (dap-ui-controls-mode 1))

;; ----------------------------------------
;; Build Tools
;; ----------------------------------------

(use-package! gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))

(use-package! maven-test-mode
  :commands maven-test-mode)

;; ----------------------------------------
;; Testing Frameworks
;; ----------------------------------------

(use-package! test-simple
  :commands test-simple-run)

;; ----------------------------------------
;; Git Integration
;; ----------------------------------------

(use-package! git-link
  :commands git-link)

(use-package! git-messenger
  :commands git-messenger:popup-message)

;; ----------------------------------------
;; GitHub Copilot
;; ----------------------------------------

(after! copilot
  ;; Enable Copilot in programming modes
  (add-hook 'prog-mode-hook #'copilot-mode)

  ;; Optional: adjust idle delay
  (setq copilot-idle-delay 0.5))

;;; tools-config.el ends here
