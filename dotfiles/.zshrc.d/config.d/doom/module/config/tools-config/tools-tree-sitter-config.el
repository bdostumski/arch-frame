;;; module/config/tools-config/tools-tree-sitter-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Tree-sitter integration for Doom Emacs.
;; Provides high-performance syntax highlighting, code navigation, and
;; language grammar management with optional async installation.

;;; Code:

;; ----------------------------
;; Core Tree-sitter setup
;; ----------------------------
(use-package! tree-sitter
  :hook (prog-mode . tree-sitter-mode)
  :config
  ;; Enable global tree-sitter highlighting
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ----------------------------
;; Language grammars
;; ----------------------------
(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  ;; Directory for downloaded grammars
  (setq tree-sitter-langs-grammar-dir
        (expand-file-name "tree-sitter/grammars/" doom-data-dir))
  ;; Enable async parser installation
  (setq tree-sitter-langs-async-install t))

;; LSP optimization for large files
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (> (buffer-size) (* 5 1024 1024)) ; 5MB threshold
              (lsp-mode -1))))

;; ----------------------------
;; Leader keybindings for navigation
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("t" . "tree-sitter")
;;       :desc "Go to function start" "f" #'tree-sitter-goto-function-start
;;       :desc "Go to function end" "e" #'tree-sitter-goto-function-end))

(provide 'tools-tree-sitter-config)

;;; tools-tree-sitter-config.el ends here
