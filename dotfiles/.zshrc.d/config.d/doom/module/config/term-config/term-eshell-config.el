;;; module/config/term-config/term-eshell-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Eshell configuration for Doom Emacs.
;; Improves history, enables aliases, syntax highlighting, and company completion.

;;; Code:

(after! eshell
  ;; History settings
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input 'all)

  ;; Aliases file
  (setq eshell-aliases-file (expand-file-name "eshell/aliases" doom-user-dir))

  ;; Enable company-mode for completions
  (add-hook 'eshell-mode-hook #'company-mode)

  ;; Enable syntax highlighting
  (add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode))

;; Keybinding for quick Eshell access
(map! :leader
      :desc "Eshell" "o e" #'eshell)

(provide 'term-eshell-config)

;;; term-eshell-config.el ends here
