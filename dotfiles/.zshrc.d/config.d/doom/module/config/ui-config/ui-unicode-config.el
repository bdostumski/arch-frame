;;; module/config/ui-config/ui-unicode-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable extended Unicode support for various languages and symbols.

;;; Code:

;; Ensure UTF-8 encoding globally
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Optional: enable symbol prettification (e.g., λ instead of 'lambda')
;; (global-prettify-symbols-mode +1)

;; Optional: configure additional fonts for extended characters
;; (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono-14")

(provide 'ui-unicode-config)

;;; ui-unicode-config.el ends here
