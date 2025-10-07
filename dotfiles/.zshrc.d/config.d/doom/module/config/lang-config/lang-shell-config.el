;;; lang-shell-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shell configuration for Doom Emacs.
;; Sets the default shell to Zsh and provides leader keybindings to open various shells.

;;; Code:

(after! shell
  ;; Set default shell
  (setq shell-file-name "/bin/zsh"))

;; Check if shells exist before configuring
(defun shell-exists-p (shell-path)
  "Check if SHELL-PATH exists in the system."
  (file-executable-p shell-path))

;; Define a function to open a specific shell
(defun open-shell-in (shell-path)
  "Open a shell using the specified SHELL-PATH."
  (if (shell-exists-p shell-path)
      (shell)
    (message "Shell %s not found or not executable." shell-path)))

;; Optionally, if you want to configure shell mode settings for all shells
(after! shell
  (setq comint-prompt-read-only t)  ; Make the prompt read-only
  (add-hook 'shell-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'shell-mode-hook #'with-editor-export-editor))

(provide 'lang-shell-config)

;;; lang-shell-config.el ends here
