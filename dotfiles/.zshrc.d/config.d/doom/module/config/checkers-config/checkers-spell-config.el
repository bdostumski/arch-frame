;;; module/config/checkers-config/checkers-spell-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Spell checking setup using Flyspell.
;; Enables automatic spell-checking in text and programming modes.
;; Provides convenient leader key shortcuts.

;;; Code:

;; Enable spell-checking automatically
(add-hook 'text-mode-hook #'flyspell-mode)       ;; check all text
(add-hook 'prog-mode-hook #'flyspell-prog-mode)  ;; check comments and strings only

;; Set dictionary (hunspell or aspell)
(setq ispell-dictionary "en_US")

;; Keybindings for spell checking
(map! :leader
      :desc "Spell check word"   "S c" #'flyspell-correct-word-before-point
      :desc "Spell check buffer" "S b" #'flyspell-buffer)

(provide 'checkers-spell-config)

;;; checkers-spell-config.el ends here
