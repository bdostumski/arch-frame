;;; module/config/tools-config/tools-tmux-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Tmux integration for Doom Emacs.
;; Provides leader keybindings for creating, switching, killing sessions,
;; and sending commands to tmux panes. Optionally sets a default session name.

;;; Code:

(after! tmux
  ;; Default tmux session name
  (setq +tmux-session-name "main"))

;; ----------------------------
;; Leader keybindings for tmux
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("t" . "tmux")
;;       :desc "Create new session" "n" #'+tmux/new-session
;;       :desc "Switch session" "s" #'+tmux/switch-session
;;       :desc "Kill session" "k" #'+tmux/kill-session
;;       :desc "Send command to pane" "c" #'+tmux/send-command))

(provide 'tools-tmux-config)

;;; tools-tmux-config.el ends here
