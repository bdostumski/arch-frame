;;; module/config/ui-config/ui-vi-tilde-fringe-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Display tildes in the fringe beyond the end of buffer (EOB), similar to Vim.

;;; Code:

(use-package! vi-tilde-fringe
  :after doom-themes
  :config
  ;; Enable globally
  (global-vi-tilde-fringe-mode +1)

  ;; Optional: customize face for tildes
  ;; (set-face-foreground 'vi-tilde-fringe "#4B5263")  ;; subdued color

  ;; Optional: only show in programming modes
  ;; (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode)

  ;; Optional: customize bitmap
  ;; (setq vi-tilde-fringe-bitmap-array
  ;;       [#b00000000
  ;;        #b00000000
  ;;        #b00000000
  ;;        #b11111111])
  )

(provide 'ui-vi-tilde-fringe-config)

;;; ui-vi-tilde-fringe-config.el ends here
