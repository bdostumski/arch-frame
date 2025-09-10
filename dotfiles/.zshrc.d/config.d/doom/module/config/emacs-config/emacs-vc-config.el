;;; module/config/emacs-config/emacs-vc-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Version Control integration for Emacs.
;; Provides auto-refresh of VC status, Git/Mercurial support, and useful keybindings.

;;; Code:

;; Auto-refresh VC status in buffers
(global-auto-revert-mode 1)

;; Configure VC
(after! vc
  ;; Automatically follow symlinks in repositories
  (setq vc-follow-symlinks t)
  ;; Only handle Git and Mercurial
  (setq vc-handled-backends '(Git Hg)))

;; Optional keybindings for VC actions
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("e" . "emacs")
                                (:prefix-map ("v" . "version control")
                                 :desc "VC Status"       "s" #'vc-dir
                                 :desc "Next VC Hunk"    "n" #'vc-next-action
                                 :desc "Previous VC Hunk" "b" #'vc-previous-action))))

(provide 'emacs-vc-config)

;;; emacs-vc-config.el ends here
