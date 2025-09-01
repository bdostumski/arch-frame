;;; module/config/tools-config/tools-lsp-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP configuration for Doom Emacs with UI enhancements via lsp-ui.
;; Provides language server integration, UI features (peek, doc, imenu),
;; performance optimizations, and leader keybindings for common LSP actions.

;;; Code:

;; ------------------------------------------
;; Doom Emacs: Full LSP Setup (VSCode-like)
;; ------------------------------------------

;; ----------------------------
;; Core LSP setup
;; ----------------------------
(after! lsp-mode
  ;; Performance tuning
  (setq read-process-output-max (* 1024 1024)  ;; 1MB
        gc-cons-threshold 100000000           ;; 100MB
        lsp-idle-delay 0.3
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-prefer-flymake nil                ;; use flycheck
        lsp-completion-provider :capf
        lsp-completion-enable-additional-text-edit t
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-code-actions-enable t
        lsp-signature-auto-activate t
        lsp-signature-doc-lines 2))

;; ----------------------------
;; LSP-UI
;; ----------------------------
(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; Hover docs at bottom
  (setq lsp-ui-doc-enable nil ;; disable ui-doc
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 150
        lsp-ui-doc-border (face-foreground 'default))

  ;; Disable sideline clutter
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-ignore-duplicate: t
        lsp-ui-sideline-show-code-actions t)

  ;; Peek definitions/references
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-always-show nil
        lsp-ui-peek-fontify 'always
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 20)

  ;; Symbols outline
  (setq lsp-ui-imenu t
        lsp-ui-imenu-enable t
        lsp-ui-imenu-auto-refresh t
        ;;lsp-ui-imenu-kind-position 'top
        lsp-ui-imenu-buffer-position 'right))

;; ----------------------------
;; Treemacs integration for symbols
;; ----------------------------
(use-package! lsp-treemacs
  :after lsp
  :config
  (lsp-treemacs-sync-mode 1))

;; ----------------------------
;; LSP Ivy (workspace/file symbols)
;; ----------------------------
(use-package! lsp-ivy
  :after lsp)

;; ----------------------------
;; Flycheck (on-the-fly diagnostics)
;; ----------------------------
(after! flycheck
  (setq flycheck-indication-mode 'left-fringe
        flycheck-highlighting-mode 'symbols))

;; ----------------------------
;; Company (Completion engine)
;; ----------------------------
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-show-quick-access t))

;; ----------------------------
;; Yasnippet (LSP snippets)
;; ----------------------------
(use-package! yasnippet
  :config
  (yas-global-mode 1))

(provide 'tools-lsp-config)

;;; tools-lsp-config.el ends here
