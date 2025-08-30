;;; module/config/emacs-undo-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Global undo-tree setup with persistent undo history.
;; Provides Vim-style undo/redo and saves history between sessions.

;;; Code:

(use-package! undo-tree
  :demand t
  :init
  ;; Enable undo-tree globally
  (global-undo-tree-mode 1)
  :config
  ;; Ensure undo directory exists
  (let ((undo-dir (concat user-emacs-directory "undo")))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t)))
  ;; Save undo history to disk for persistence
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo"))))
  (setq undo-tree-auto-save-history t)
  ;; Enable undo-tree in non-file buffers for Evil
  (add-hook 'evil-local-mode-hook #'turn-on-undo-tree-mode))

(provide 'emacs-undo-config)

;;; emacs-undo-config.el ends here
