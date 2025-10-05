;;; module/config/editor-config/ui-evil-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Evil mode setup for Doom Emacs.
;; Provides Vim-like behavior everywhere, with quality-of-life tweaks,
;; surround support, and commentary integration.

;;; Code:

(after! evil
  ;; ----------------------------
  ;; Core Evil settings
  ;; ----------------------------
  (setq evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump t
        evil-want-C-w-in-emacs-state t
        evil-respect-visual-line-mode t
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil
        evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-ex-interactive-search-highlight 'selected-window
        evil-insert-skip-empty-lines t
        evil-mode-line-format nil
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(bar "white")
        evil-visual-state-cursor '(hollow "orange")
        evil-replace-state-cursor '(hbar "red")
        evil-operator-state-cursor '(evil-half-cursor "red"))

  ;; Important: Ensure undo-tree integration is properly set
  (when (featurep 'undo-tree)
    (setq evil-undo-system 'undo-tree)))

;; ----------------------------
;; Evil-surround (like tpope's plugin)
;; ----------------------------
(use-package! evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ----------------------------
;; Evil-commentary (gc for commenting)
;; ----------------------------
(after! evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(provide 'ui-evil-config)

;;; ui-evil-config.el ends here
