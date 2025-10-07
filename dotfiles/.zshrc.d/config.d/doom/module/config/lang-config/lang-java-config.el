;;; lang-java-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Java setup with LSP, formatting, DAP, and keybindings for compilation and testing.

;;; Code:

;; Ensure needed packages
(use-package! lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-save-action-organize-imports t
        ;; Performance optimizations
        lsp-java-vmargs '("-XX:+UseParallelGC" 
                          "-XX:GCTimeRatio=4"
                          "-XX:AdaptiveSizePolicyWeight=90"
                          "-Dsun.zip.disableMemoryMapping=true"
                          "-Xmx2G"
                          "-Xms100m")
        lsp-java-completion-import-order '["java" "javax" "com" "org" ""]
        lsp-java-content-provider-preferred "fernflower"))

;; DAP mode setup for debugging
(use-package! dap-java
  :after (lsp-java dap-mode)
  :config
  (require 'dap-java))

(after! java-mode
  ;; Enable LSP in Java buffers
  (add-hook 'java-mode-hook #'lsp)
  
  ;; Optional formatting style
  (setq c-basic-offset 4
        c-default-style "java")
  
  ;; Enable code folding
  (add-hook 'java-mode-hook #'hs-minor-mode)
  
  ;; Enable auto-import
  (setq lsp-java-import-gradle-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-java-references-code-lens-enabled t))

;; Configure test runner
(after! lsp-java
  (setq lsp-java-test-runner "junit"))

(provide 'lang-java-config)

;;; lang-java-config.el ends here
