;;; module/config/ui-config/ui-deft-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Optimized Deft configuration for Doom Emacs.
;; Supports Org, Markdown, and plain text files with recursive search.

;;; Code:

(use-package! deft

  :defer t
  :commands (deft)
  :init
  :config
  ;; Core settings
  (setq deft-directory "~/Documents/notes"
        deft-extensions '("org" "md" "txt")
        deft-recursive t
        deft-auto-save-interval 1.0
        deft-use-filename-as-title t
        deft-default-filter ""
        deft-parse-title t))

(provide 'ui-deft-config)

;;; ui-deft-config.el ends here
