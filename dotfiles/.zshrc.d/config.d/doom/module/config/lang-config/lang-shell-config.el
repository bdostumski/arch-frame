;;; module/config/lang-config/lang-shell-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Shell configuration for Doom Emacs.
;; Sets the default shell to Zsh and provides leader keybindings to open various shells.

;;; Code:

(after! shell
  ;; Set default shell
  (setq shell-file-name "/bin/zsh"))

;; ----------------------------
;; Leader keybindings for shells
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("t" . "terminal")
                                 :desc "Open Zsh"  "z" (lambda () (interactive) (shell "/bin/zsh"))
                                 :desc "Open Bash" "b" (lambda () (interactive) (shell "/bin/bash"))
                                 :desc "Open Fish" "f" (lambda () (interactive) (shell "/bin/fish"))))))

(provide 'lang-shell-config)

;;; lang-shell-config.el ends here
