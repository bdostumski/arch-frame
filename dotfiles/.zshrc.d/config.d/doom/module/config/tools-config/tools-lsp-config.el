;;; module/config/tools-config/tools-lsp-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP configuration for Doom Emacs with UI enhancements via lsp-ui.
;; Provides language server integration, UI features (peek, doc, imenu),
;; performance optimizations, and leader keybindings for common LSP actions.

;;; Code:

;; ----------------------------
;; Core LSP configuration
;; ----------------------------
(use-package! lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode        . lsp-deferred)  ;; most programming languages
         (yaml-mode        . lsp-deferred)
         (json-mode        . lsp-deferred)
         (markdown-mode    . lsp-deferred)
         (dockerfile-mode  . lsp-deferred)
         (terraform-mode   . lsp-deferred)
         (sh-mode          . lsp-deferred)
         (xml-mode         . lsp-deferred))
  :init
  ;; Performance improvements
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :config
  (setq lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-prefer-flymake nil
        lsp-idle-delay 0.3                      ;; faster feedback
        lsp-log-io nil
        lsp-completion-provider :capf           ;; company/corfu integration
        lsp-completion-enable-additional-text-edit t
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-code-actions-enable t
        lsp-signature-auto-activate t
        lsp-signature-doc-lines 2))

;; ----------------------------
;; LSP UI enhancements
;; ----------------------------
(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-enable nil              ;; disable sideline clutter
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-fontify 'always
        lsp-ui-imenu-enable t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil))

;; ----------------------------
;; Keybindings for LSP actions
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("l" . "lsp")
;;       :desc "Peek definition" "d" #'lsp-ui-peek-find-definitions
;;       :desc "Peek references" "r" #'lsp-ui-peek-find-references
;;       :desc "Hover doc" "h" #'lsp-ui-doc-glance
;;       :desc "Restart LSP" "R" #'lsp-restart-workspace
;;       :desc "Format buffer" "f" #'lsp-format-buffer))

(provide 'tools-lsp-config)

;;; tools-lsp-config.el ends here
