;;; module/checkers-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module loads configurations for various checkers in Emacs.
;; It includes:
;;   - General checker setup
;;   - Grammar checking
;;   - Spell checking
;;   - Syntax checking (Flycheck, Flymake, etc.)

;;; Code:

;; General configuration for all checkers
(load! "config/checkers-config/checkers-config.el")

;; Grammar checking (e.g., LanguageTool integration)
(load! "config/checkers-config/checkers-grammar-config.el")

;; Spell checking (aspell, hunspell, ispell, etc.)
(load! "config/checkers-config/checkers-spell-config.el")

;; Syntax checking (Flycheck or Flymake)
(load! "config/checkers-config/checkers-syntax-config.el")

(provide 'checkers-module)

;;; checkers-module.el ends here
