;;; module/config/checkers-config/checkers-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Spell checking configuration using Hunspell.
;; Ensures Emacs uses Hunspell with English (US) dictionary if available.
;; Enhanced with fallback to spell and better error handling.

;;; Code:

;; Function to detect available a spell checkers
(defun checkers--detect-spell-checker ()
  "Detect and configure the best available spell checker."
  (cond
   ;; Prefer Hunspell if available
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US"
          ispell-local-dictionary "en_US"
          ispell-hunspell-dict-paths-alist
          '(("en_US" "/usr/share/hunspell/en_US.aff"))
          ispell-dictionary-alist
          '(("en_US"
             "[[:alpha:]]"       ;; word characters
             "[^[:alpha:]]"      ;; non-word characters
             "[']"               ;; additional characters (apostrophes)
             t                   ;; case-insensitive
             ("-d" "en_US")      ;; Hunspell dictionary
             nil                 ;; affix file (nil = default)
             utf-8)))            ;; encoding
    (message "Hunspell configured for spell checking"))
   
   ;; Fallback to a spell
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-dictionary "en_US"
          ispell-local-dictionary "en_US"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
    (message "Aspell configured for spell checking"))
   
   ;; No spell checker found
   (t
    (message "Warning: No spell checker (hunspell/aspell) found. Please install one."))))

;; Initialize spell checker detection
(checkers--detect-spell-checker)

;; Performance optimization: set personal dictionary
(setq ispell-personal-dictionary (expand-file-name "~/.emacs.d/.aspell.en.pws"))

;; Create personal dictionary file if it doesn't exist
(unless (file-exists-p ispell-personal-dictionary)
  (with-temp-buffer
    (insert "personal_ws-1.1 en 0\n")
    (write-file ispell-personal-dictionary)))

(provide 'checkers-config)

;;; checkers-config.el ends here
