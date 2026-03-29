;;; module/config/checkers-config/checkers-spell-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Spell checking setup using Flyspell.
;; Enables automatic spell-checking in text and programming modes.
;; Provides convenient leader key shortcuts with enhanced functionality.

;;; Code:

;; Custom function to toggle flyspell in programming modes
(defun checkers-spell-toggle-prog-mode ()
  "Toggle flyspell-prog-mode in programming buffers."
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1)
    (flyspell-prog-mode)))

;; Enhanced flyspell configuration
(use-package! flyspell
  :hook ((text-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (git-commit-mode . flyspell-mode))
  :config
  ;; Performance optimizations
  (setq flyspell-issue-message-flag nil         ;; Don't print messages
        flyspell-issue-welcome-flag nil         ;; Don't show welcome message
        flyspell-consider-dash-as-word-delimiter-flag t  ;; Treat dash as word delimiter
        flyspell-abbrev-p t                     ;; Use abbrev mode
        flyspell-use-global-abbrev-table-p t)   ;; Use global abbrev table

  ;; Better word boundaries for programming
  (add-to-list 'flyspell-prog-text-faces 'font-lock-doc-face)

  ;; Custom correction function with better interface
  (defun checkers-spell-correct-dwim ()
    "Correct word at point or before point intelligently."
    (interactive)
    (cond
     ;; If on a misspelled word, correct it
     ((flyspell-overlay-p (overlays-at (point)))
      (flyspell-correct-at-point))
     ;; If previous word is misspelled, correct it
     ((save-excursion
        (backward-word)
        (flyspell-overlay-p (overlays-at (point))))
      (save-excursion
        (backward-word)
        (flyspell-correct-at-point)))
     ;; Otherwise, correct word before point
     (t (flyspell-correct-word-before-point))))

  ;; Auto-correct common typos
  (defun checkers-spell-auto-correct-word ()
    "Auto-correct word if there's only one suggestion."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (when word
        (let ((suggestions (ispell-get-word nil "\\*")))
          (when (and suggestions (= (length (nth 2 suggestions)) 1))
            (ispell-command-loop suggestions nil (nth 2 suggestions) nil))))))

  ;; Bind double-click to correct word
  (define-key flyspell-mouse-map [double-mouse-1] #'flyspell-correct-at-point))

;; Set dictionary with fallback
(setq ispell-dictionary "en_US")

;; Language switching function
(defun checkers-spell-switch-language (lang)
  "Switch spell checking language."
  (interactive
   (list (completing-read "Language: "
                          '("en_US" "en_GB" "es" "fr" "de" "it"))))
  (setq ispell-dictionary lang)
  (when flyspell-mode
    (flyspell-mode -1)
    (flyspell-mode 1))
  (message "Switched to %s dictionary" lang))

;; Optional: Add visual feedback for corrections
(use-package! flyspell-correct
  :after flyspell
  :config
  (setq flyspell-correct-interface #'flyspell-correct-completing-read))

(provide 'checkers-spell-config)

;;; checkers-spell-config.el ends here
