;;; module/config/editor-config/editor-multiple-cursors-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized multiple cursor editing for Doom Emacs with Evil mode.
;; evil-multiedit is lightweight, fully Vim-compatible, and integrates with Evil states.

;;; Code:

(use-package! evil-multiedit
  :after evil
  :commands (evil-multiedit-match-all
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-restore
             evil-multiedit-ex-match
             evil-multiedit-toggle-marker-here)
  :config
  ;; Enable default keybindings (M-d, C-n, C-p, etc.)
  (evil-multiedit-default-keybinds)

  ;; Tweaks
  (setq evil-multiedit-smart-match-boundaries t
        evil-multiedit-ignore-indent-and-trailing t))

;; Leader keybindings for Doom
(map! :leader
      (:prefix-map ("e" . "editor")
               (:prefix-map ("e" . "emacs")
                (:prefix-map ("m" . "multiedit")
                :desc "Mark symbol + next" "n" #'evil-multiedit-match-symbol-and-next
                :desc "Mark symbol + prev" "p" #'evil-multiedit-match-symbol-and-prev
                :desc "Mark all matches" "a" #'evil-multiedit-match-all
                :desc "Ex match (regex)" "e" #'evil-multiedit-ex-match
                :desc "Restore session" "r" #'evil-multiedit-restore))))

(provide 'editor-multiple-cursors-config)

;;; editor-multiple-cursors-config.el ends here
