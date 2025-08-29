;;; module/config/os-config/os-unix-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Unix-specific enhancements for Doom Emacs.
;; Provides integration with common Unix tools and sets up environment paths.

;;; Code:

(after! unix
  ;; Prefer ripgrep for searches
  (setq +unix-search-tool 'rg))

;; Optional: set up PATH for Unix commands
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(provide 'os-unix-config)

;;; os-unix-config.el ends here
