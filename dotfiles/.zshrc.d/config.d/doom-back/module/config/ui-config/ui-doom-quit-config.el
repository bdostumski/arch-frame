;;; module/config/ui-config/ui-doom-quit-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Doom Emacs quit/exit behavior.
;; Provides clean confirmation prompts and optional autosave/cleanup hooks.

;;; Code:

(use-package! doom-quit
  :defer t
  :config
  ;; ----------------------------------------
  ;; Confirm before quitting
  ;; ----------------------------------------
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; ----------------------------------------
  ;; Optional: autosave all buffers before quit
  ;; ----------------------------------------
  ;; (add-hook 'kill-emacs-hook #'save-some-buffers)

  ;; ----------------------------------------
  ;; Optional: clean temporary files or cache on exit
  ;; ----------------------------------------
  ;; (add-hook 'kill-emacs-hook
  ;;           (lambda ()
  ;;             (delete-file "/path/to/temp/file")
  ;;             (message "Temporary files cleaned.")))

  ;; ----------------------------------------
  ;; Optional: custom goodbye message
  ;; ----------------------------------------
  ;; (setq doom-quit-message "Have a productive day!")
  )

(provide 'ui-doom-quit-config)

;;; ui-doom-quit-config.el ends here
