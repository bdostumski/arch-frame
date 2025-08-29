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
  :hook ((java-mode . lsp)
         (js-mode . lsp)
         (python-mode . lsp))
  :commands lsp)

(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t))

(use-package! lsp-treemacs
  :after lsp)

;; ----------------------------------------
;; Debug Adapter Protocol
;; ----------------------------------------

(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

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
