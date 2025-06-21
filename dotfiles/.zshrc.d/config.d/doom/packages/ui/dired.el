;;; packages/ui/dired.el -*- lexical-binding: t; -*-

(use-package! all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
