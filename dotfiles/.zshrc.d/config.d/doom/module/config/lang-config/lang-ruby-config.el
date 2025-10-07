;;; lang-ruby-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Ruby and Rails configuration for Doom Emacs.
;; Enables Rails minor mode, sets indentation, and defines leader keybindings
;; for running scripts, Rails server, jumping to definitions, and running tests.

;;; Code:

;; Ensure required packages are installed
(use-package ruby-mode
  :ensure t
  :config
  ;; Enable Rails minor mode if available
  (when (fboundp 'ruby-on-rails-mode)
    (add-hook 'ruby-mode-hook #'ruby-on-rails-mode))

  ;; Set indentation level
  (setq ruby-indent-level 2))

;; Add robe for code navigation
(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

;; Add rspec-mode for testing
(use-package rspec-mode
  :ensure t
  :hook (ruby-mode . rspec-mode))

;; Define ruby-run-file function if not available
(unless (fboundp 'ruby-run-file)
  (defun ruby-run-file ()
    "Run the current ruby file."
    (interactive)
    (when buffer-file-name
      (shell-command (concat "ruby " buffer-file-name)))))

;; Define rails-server function if not available
(unless (fboundp 'rails-server)
  (defun rails-server ()
    "Start a Rails server in a new compilation buffer."
    (interactive)
    (let ((default-directory (or (locate-dominating-file default-directory "Gemfile") default-directory)))
      (compile "bundle exec rails server"))))

(provide 'lang-ruby-config)

;;; lang-ruby-config.el ends here
