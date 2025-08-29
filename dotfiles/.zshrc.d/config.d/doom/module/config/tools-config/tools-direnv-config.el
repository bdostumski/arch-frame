;;; module/config/tools-config/tools-direnv-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Direnv integration for Emacs.
;; Automatically loads environment variables per project using .envrc files.

;;; Code:

(after! direnv
  ;; Enable Direnv integration globally
  (direnv-mode 1))

;; Optional: always show the Direnv summary after environment changes
(setq direnv-always-show-summary t)

(provide 'tools-direnv-config)

;;; tools-direnv-config.el ends here
