;;; module/config/ui-config/ui-word-wrap-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable soft word wrapping globally, with optional auto-fill for text modes
;; and visual indicators in the fringe for wrapped lines.

;;; Code:

;; Enable visual line wrapping globally
(global-visual-line-mode t)

;; Enable auto-fill (hard wrap) in text modes only
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Show continuation arrows in the fringe
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(provide 'ui-word-wrap-config)

;;; ui-word-wrap-config.el ends here
