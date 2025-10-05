;;; module/config/editor-config/emacs-undo-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive undo-tree setup for Doom Emacs
;; - Works in all programming and text buffers
;; - Evil integration
;; - Persistent history support
;; - Visualizer enhancements

;;; Code:

;; Ensure undo-tree package is loaded
(require 'undo-tree)

;; Integrate undo-tree with Evil mode (Vim emulation)
(use-package! evil
  :config
  ;; Tell Evil to use undo-tree for undo/redo operations instead of the default
  (setq evil-undo-system 'undo-tree))

;; Enable undo-tree globally in all buffers
(global-undo-tree-mode 1)

;; Enable undo-tree in every buffer that has evil-local-mode enabled
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

;; Undo-tree persistence and behavior configuration
(setq undo-tree-auto-save-history nil  ;; Disable auto-save history for now; set to t to persist history between sessions
      undo-tree-history-directory-alist
      `(("." . ,(expand-file-name "undo-tree-history/" doom-user-dir))) ; save history files under Doom Emacs directory
      undo-tree-enable-undo-in-region t)  ;; allow undo/redo in selected regions

;; Adjust undo memory limits for better performance
(setq undo-limit 800000           ; maximum number of bytes for undo in normal situations
      undo-strong-limit 1200000   ; maximum for "strong" undo operations
      undo-outer-limit 120000000) ; maximum bytes before Emacs stops undoing entirely

;; Visualizer enhancements for undo-tree
(setq undo-tree-visualizer-diff t                 ; show differences between states
      undo-tree-visualizer-timestamps t           ; show timestamps in visualizer
      undo-tree-visualizer-lazy-drawing nil       ; draw full tree immediately
      undo-tree-visualizer-relative-timestamps t  ; show relative times
      undo-tree-mode-lighter " UT")               ; short mode line lighter for undo-tree

;; Optional: load eval-sexp-fu if available, for better visual feedback when evaluating expressions
(when (locate-library "eval-sexp-fu")
  (require 'eval-sexp-fu))

(provide 'emacs-undo-config)
;;; emacs-undo-config.el ends here
