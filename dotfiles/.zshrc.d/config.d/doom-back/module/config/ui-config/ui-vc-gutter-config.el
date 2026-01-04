;;; module/config/ui-config/ui-vc-gutter-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Show version control diff info in the fringe using Doom's vc-gutter module.

;;; Code:

(after! vc-gutter
  :after vc
  :config
  ;; Enable vc-gutter globally
  (vc-gutter-mode +1)

  ;; Optional: use pretty symbols in the fringe
  ;; Default symbols are ▮ for added, ▯ for modified, ▯ for removed
  (setq vc-gutter-fringe-insert-face 'diff-added
        vc-gutter-fringe-change-face 'diff-changed
        vc-gutter-fringe-delete-face 'diff-removed)

  ;; Optional: customize update interval (in seconds)
  (setq vc-gutter-update-interval 0.5))

(provide 'ui-vc-gutter-config)

;;; ui-vc-gutter-config.el ends here
