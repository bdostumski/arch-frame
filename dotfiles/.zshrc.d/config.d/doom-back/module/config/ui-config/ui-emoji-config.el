;;; module/config/ui-config/ui-emoji-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable global emoji support with emojify.

;;; Code:

(use-package! emojify
  :defer t
  :hook (after-init . global-emojify-mode)
  :config
  ;; Optional: adjust emoji display style
  ;; (setq emojify-display-style 'unicode) ;; or 'image

  ;; Optional: disable in certain modes for performance
  ;; (add-hook 'text-mode-hook #'emojify-mode)
  )

(provide 'ui-emojify-config)

;;; ui-emojify-config.el ends here
