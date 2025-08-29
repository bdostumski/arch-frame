;;; module/config/emacs-undo-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Global undo-tree setup with persistent undo history.
;; Provides Vim-style undo/redo and saves history between sessions.

;;; Code:

(use-package! undo-tree
  :demand t
  :init
  (global-undo-tree-mode 1)
  :config
  ;; Save undo history to disk for persistence
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo"))))
  (setq undo-tree-auto-save-history t))

(provide 'emacs-undo-config)

;;; emacs-undo-config.el ends here
