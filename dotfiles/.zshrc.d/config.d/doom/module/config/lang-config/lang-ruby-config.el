;;; module/config/lang-config/lang-ruby-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Ruby and Rails configuration for Doom Emacs.
;; Enables Rails minor mode, sets indentation, and defines leader keybindings
;; for running scripts, Rails server, jumping to definitions, and running tests.

;;; Code:

(after! ruby-mode
  ;; Enable Rails minor mode
  (add-hook 'ruby-mode-hook #'ruby-on-rails-mode)

  ;; Optional: set indentation level
  (setq ruby-indent-level 2))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("r" . "ruby")
;;       :desc "Run current file" "r" #'ruby-run-file
;;       :desc "Run Rails server" "s" #'rails-server
;;       :desc "Go to definition" "d" #'robe-jump
;;       :desc "Run tests" "t" #'rspec-run-single-file))

(provide 'lang-ruby-config)

;;; lang-ruby-config.el ends here
