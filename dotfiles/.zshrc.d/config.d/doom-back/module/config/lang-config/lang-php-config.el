;;; lang-config/lang-php-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; PHP configuration for Doom Emacs.
;; Provides LSP support, coding style settings, and leader keybindings
;; for running scripts, formatting, navigation, and testing.

;;; Code:

;; Ensure required packages are installed
(use-package! php-mode)
(use-package! phpunit)  ;; For running PHP tests
(use-package! php-cs-fixer
  :config
  (setq php-cs-fixer-rules-default-rule-set "PSR2"))

(after! php-mode
  ;; Enable LSP in PHP buffers
  (add-hook 'php-mode-hook #'lsp)

  ;; PHP coding style
  (setq php-mode-coding-style 'psr2
        c-basic-offset 4)

  ;; Enable PHP CS Fixer integration
  (add-hook 'php-mode-hook #'php-cs-fixer-enable))

(provide 'lang-php-config)

;;; lang-php-config.el ends here
