;;; config/app-config/app-irc-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic rcirc (IRC client) setup for Doom Emacs.
;; - Defines default server and user information
;; - Auto-joins specified channels
;; - Provides convenient leader keybindings

;;; Code:

(after! rcirc
  ;; Default IRC server configuration
  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :port 6667
           :nick "your-nick"
           :user-name "your-username"
           :full-name "Your Name")))

  ;; Auto-join channels on connection
  (setq rcirc-default-chan-alist
        '(("irc.libera.chat" "#emacs" "#doom-emacs"))))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("i" . "irc")
;;       :desc "Connect to IRC"      "c" #'rcirc
;;       :desc "Switch IRC buffer"   "b" #'rcirc-switch-to-buffer))

(provide 'app-irc-config)

;;; app-irc-config.el ends here
