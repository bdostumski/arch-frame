;;; module/config/emacs-config/emacs-eww-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized EWW configuration for Doom Emacs.

;;; Code:

;; Set default search engine to Google
(setq eww-search-prefix "https://www.google.com/search?q=")

;; Enable images in EWW
(setq shr-inhibit-images nil)

;; Improve readability: slightly larger font with JetBrains Mono
(custom-set-faces!
  '(shr-face :height 1.1 :family "JetBrains Mono"))

(provide 'emacs-eww-config)

;;; emacs-eww-config.el ends here
