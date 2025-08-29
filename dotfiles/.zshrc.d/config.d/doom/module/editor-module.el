;;; module/editor-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module consolidates all editor-related configurations.
;; It primarily handles editing behavior, formatting, folding,
;; Evil-mode (Vim emulation), and multiple cursors.

;;; Code:

;; Evil-mode: Vim emulation inside Emacs
(load! "config/editor-config/editor-evil-config.el")

;; Code folding (hide/show code blocks)
(load! "config/editor-config/editor-fold-config.el")

;; Automatic code formatting and style enforcement
(load! "config/editor-config/editor-format-config.el")

;; Multiple cursors support (edit text in several places at once)
(load! "config/editor-config/editor-multiple-cursors-config.el")

(load! "config/editor-config/editor-rainbow-delimiters-config.el")

(provide 'editor-module)

;;; editor-module.el ends here
