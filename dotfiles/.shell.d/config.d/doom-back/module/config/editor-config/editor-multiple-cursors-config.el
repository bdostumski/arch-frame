;;; module/config/editor-config/editor-multiple-cursors-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive multiple cursor editing configuration for Doom Emacs with Evil mode.
;; Includes evil-multiedit, multiple-cursors, and iedit for different use cases.

;;; Code:

;;; evil-multiedit - Primary multiple cursor solution for Evil mode
(use-package! evil-multiedit
  :after evil
  :commands (evil-multiedit-match-all
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-restore
             evil-multiedit-ex-match
             evil-multiedit-toggle-marker-here
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-toggle-or-restrict-region)
  :init
  ;; Highlighting
  (setq evil-multiedit-use-symbols t
        evil-multiedit-smart-match-boundaries t
        evil-multiedit-ignore-indent-and-trailing t
        evil-multiedit-scope 'visible)

  :config
  ;; Enable default keybindings
  (evil-multiedit-default-keybinds)

  ;; Custom faces for better visibility
  (custom-set-faces!
    '(evil-multiedit-face :background "#3e4451" :foreground "#e06c75" :weight bold)
    '(evil-multiedit-insert-face :background "#98c379" :foreground "#282c34" :weight bold))

  ;; Hooks for enhanced functionality
  (add-hook 'evil-multiedit-mode-hook
            (lambda ()
              (when evil-multiedit-mode
                (evil-multiedit-state))))

  ;; Integration with company-mode
  (add-hook 'company-completion-started-hook #'evil-multiedit-abort)
  (add-hook 'company-completion-finished-hook #'evil-multiedit-abort))

;;; multiple-cursors - Alternative/complementary solution
(use-package! multiple-cursors
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             mc/mark-all-words-like-this
             mc/mark-all-symbols-like-this
             mc/add-cursor-on-click
             mc/mark-more-like-this-extended
             mc/mark-all-in-region
             mc/edit-lines
             mc/edit-ends-of-lines
             mc/edit-beginnings-of-lines
             mc/mark-sgml-tag-pair
             mc/mark-pop)
  :init
  (setq mc/always-run-for-all t
        mc/always-repeat-command t
        mc/insert-numbers-default 1)

  :config
  ;; Commands to run for all cursors automatically
  (setq mc/cmds-to-run-for-all
        '(evil-next-line
          evil-previous-line
          evil-forward-char
          evil-backward-char
          evil-delete-char
          evil-delete-backward-char
          evil-append
          evil-insert
          evil-open-below
          evil-open-above
          evil-substitute
          evil-change
          evil-delete
          evil-yank
          forward-sentence
          backward-sentence
          kill-sentence
          upcase-word
          downcase-word
          capitalize-word
          transpose-words))

  ;; Commands to run only once
  (setq mc/cmds-to-run-once
        '(evil-mouse-drag-region
          mouse-set-point
          mouse-set-region
          evil-visual-char
          evil-visual-line
          evil-visual-block)))

;;; iedit - For simultaneous editing of identical text
(use-package! iedit
  :commands (iedit-mode
             iedit-mode-toggle-on-function
             iedit-expand-down-to-occurrence
             iedit-expand-up-to-occurrence
             iedit-restrict-function
             iedit-restrict-current-line)
  :init
  (setq iedit-toggle-key-default nil
        iedit-case-sensitive-default t
        iedit-unmatched-lines-invisible-default nil)

  :config
  ;; Custom faces
  (custom-set-faces!
    '(iedit-occurrence :background "#61afef" :foreground "#282c34" :weight bold)
    '(iedit-read-only-occurrence :background "#5c6370" :foreground "#abb2bf")))

;;; Visual enhancements and utilities
(use-package! expand-region
  :commands (er/expand-region er/contract-region)
  :config
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

;;; Additional utility functions
(defun +multiedit/mark-word-at-point ()
  "Mark the word at point for multiedit."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (evil-multiedit-toggle-or-restrict-region))))

(defun +multiedit/smart-match ()
  "Smart matching based on context."
  (interactive)
  (cond
   ((use-region-p)
    (evil-multiedit-match-all))
   ((thing-at-point 'symbol)
    (evil-multiedit-match-symbol-and-next))
   (t
    (call-interactively #'evil-multiedit-toggle-marker-here))))

(defun +multiedit/cleanup-markers ()
  "Clean up all multiedit markers."
  (interactive)
  (when (bound-and-true-p evil-multiedit-mode)
    (evil-multiedit-abort))
  (when (bound-and-true-p multiple-cursors-mode)
    (mc/keyboard-quit))
  (when (bound-and-true-p iedit-mode)
    (iedit-mode -1)))

;;; Hydra for quick access (optional but recommended)
(when (modulep! :ui hydra)
  (defhydra hydra-multiedit (:color pink :hint nil)
    "
^Multiedit^         ^Multiple Cursors^    ^Navigation^     ^Actions^
^^^^^^^^------------------------------------------------------------------
_n_: next symbol    _c n_: mc next        _j_: next        _a_: match all
_p_: prev symbol    _c p_: mc prev        _k_: prev        _r_: restore
_t_: toggle here    _c a_: mc all         _h_: first       _q_: quit
_e_: ex match       _c l_: edit lines     _l_: last        _RET_: done
_R_: in region      _c r_: mc in region   ^ ^              _ESC_: abort
"
    ("n" evil-multiedit-match-symbol-and-next)
    ("p" evil-multiedit-match-symbol-and-prev)
    ("t" evil-multiedit-toggle-marker-here)
    ("e" evil-multiedit-ex-match)
    ("R" evil-multiedit-toggle-or-restrict-region)
    ("a" evil-multiedit-match-all)
    ("r" evil-multiedit-restore)

    ("c n" mc/mark-next-like-this)
    ("c p" mc/mark-previous-like-this)
    ("c a" mc/mark-all-like-this)
    ("c l" mc/edit-lines)
    ("c r" mc/mark-all-in-region)

    ("j" evil-multiedit-next)
    ("k" evil-multiedit-prev)
    ("h" evil-multiedit-toggle-marker-here)
    ("l" evil-multiedit-toggle-marker-here)

    ("RET" evil-multiedit-toggle-or-restrict-region :exit t)
    ("ESC" +multiedit/cleanup-markers :exit t)
    ("q" +multiedit/cleanup-markers :exit t))

  (map! :leader
        (:prefix-map ("e" . "editor")
         :desc "Multiedit hydra" "M" #'hydra-multiedit/body)))

;;; Integration with Doom modules
(after! evil-snipe
  ;; Disable evil-snipe when multiedit is active
  (add-hook 'evil-multiedit-mode-hook
            (lambda ()
              (if evil-multiedit-mode
                  (evil-snipe-local-mode -1)
                (evil-snipe-local-mode 1)))))

(after! company
  ;; Better company integration
  (add-hook 'evil-multiedit-insert-state-entry-hook
            (lambda () (company-mode -1)))
  (add-hook 'evil-multiedit-insert-state-exit-hook
            (lambda () (company-mode 1))))

;;; Performance optimizations
(setq evil-multiedit-follow-matches t
      evil-multiedit-dwim-motion-keys t)

;; Advice to improve performance with large files
(defadvice evil-multiedit-match-all (around performance-boost activate)
  "Improve performance for large files."
  (let ((inhibit-modification-hooks t)
        (inhibit-point-motion-hooks t))
    ad-do-it))

;;; Custom commands for specific workflows
(evil-define-command +multiedit/substitute-all (pattern replacement)
  "Substitute all occurrences using multiedit."
  :repeat nil
  (interactive "<a><a>")
  (save-excursion
    (goto-char (point-min))
    (when (search-forward pattern nil t)
      (goto-char (match-beginning 0))
      (evil-multireplace pattern)
      (evil-multiedit-match-all)
      (when evil-multiedit-mode
        (evil-multiedit-state)
        (evil-delete-char 0 (length pattern))
        (evil-insert-state)
        (insert replacement)
        (evil-multiedit-state)))))

;;; Documentation and help
(defun +multiedit/help ()
  "Show multiedit help."
  (interactive)
  (message "Multiedit Help:
SPC e m n/p - next/prev symbol
SPC e m a - all matches
SPC e m e - regex match
SPC e m r - restore
gz[n/p/a/t] - quick access
Visual R - match selection
C-n/C-p in visual - mc next/prev"))

(provide 'editor-multiple-cursors-config)

;;; editor-multiple-cursors-config.el ends here
