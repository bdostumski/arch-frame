;;; module/config/emacs-config/ranger-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal Ranger configuration for manual invocation with Doom Emacs.
;; Provides a leader keybinding for quick access.

;;; Code:

(use-package! ranger
  :defer t
  :commands ranger ;; manually call with M-x ranger
  :config
  ;; Optional: customize Ranger behavior
  (setq ranger-show-hidden t
        ranger-cleanup-on-disable t
        ranger-parent-depth 1))

;; Leader keybinding for quick access
(map! :leader
      :desc "Open Ranger" "o r" #'ranger)

(provide 'file-managers-config)

;;; ranger-config.el ends here
