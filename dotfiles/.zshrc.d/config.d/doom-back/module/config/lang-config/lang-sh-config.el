;;; module/config/lang-config/lang-sh-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shell script (sh) configuration for Doom Emacs.
;; Enables LSP support and sets indentation preferences,
;; with leader keybindings for running, linting, and navigation.

;;; Code:

(after! sh-mode
  ;; Enable LSP for shell buffers if available
  (when (modulep! :tools lsp)
    (add-hook 'sh-mode-hook #'lsp!))

  ;; Set basic indentation
  (setq sh-basic-offset 2
        sh-indentation 2))

;; Function to run current shell script buffer
(defun +sh/run-current-buffer ()
  "Run the current buffer as a shell script."
  (interactive)
  (let ((file-name buffer-file-name))
    (if file-name
        (async-shell-command (concat "chmod +x " file-name " && " file-name))
      (message "Buffer is not visiting a file!"))))

(provide 'lang-sh-config)

;;; lang-sh-config.el ends here
