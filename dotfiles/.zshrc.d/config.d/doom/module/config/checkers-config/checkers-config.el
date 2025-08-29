;;; module/config/checkers-config/checkers-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Spell checking configuration using Hunspell.
;; Ensures Emacs uses Hunspell with English (US) dictionary if available.

;;; Code:

(when (executable-find "hunspell")
  ;; Set Hunspell as the default spell checker
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US"
        ispell-local-dictionary "en_US"
        ;; Define dictionary behavior
        ispell-dictionary-alist
        '(("en_US"
           "[[:alpha:]]"       ;; word characters
           "[^[:alpha:]]"      ;; non-word characters
           "[']"               ;; additional characters
           t                   ;; case-insensitive
           ("-d" "en_US")      ;; Hunspell dictionary
           nil                 ;; affix file (nil = default)
           utf-8))))           ;; encoding

(provide 'checkers-config)

;;; checkers-config.el ends here
