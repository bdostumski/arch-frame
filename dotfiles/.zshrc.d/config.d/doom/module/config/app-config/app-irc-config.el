;;; app-irc-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full rcirc (IRC client) setup for Doom Emacs.
;; - Defines default server and user information
;; - Auto-joins specified channels
;; - Adds basic TLS support
;; - Enables logging and highlights
;; - Provides convenient leader keybindings

;;; Code:

(use-package! rcirc
  :defer t
  :init
  ;; For TLS support, ensure openssl/gnutls is installed and use port 6697
  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :port 6697
           :encryption tls
           :nick "your-nick"
           :user-name "your-username"
           :full-name "Your Name")))

  (setq rcirc-default-chan-alist
        '(("irc.libera.chat" "#emacs" "#doom-emacs")))

  ;; Optional: Logging
  (setq rcirc-log-directory "~/.emacs.d/rcirc-logs/"
        rcirc-log-flag t
        rcirc-log-directory-per-server t)

  ;; Optional: Highlight nicknames
  (setq rcirc-prompt "➤ ")

  :config
  ;; Enable logging of IRC chats
  (unless (file-directory-p rcirc-log-directory)
    (make-directory rcirc-log-directory t))
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (add-hook 'rcirc-print-hooks
            (lambda (process sender response target text)
              (when rcirc-log-flag
                (rcirc-write-log process sender response target text))))
  ;; Set UTF-8 encoding for best compatibility
  (setq rcirc-decode-coding-system 'utf-8
        rcirc-encoding 'utf-8)
  )

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;(:prefix-map ("i" . "irc")
;;:desc "Connect to IRC"      "c" #'rcirc
;;:desc "Switch IRC buffer"   "b" #'rcirc-switch-to-buffer
;;:desc "Disconnect"          "q" #'rcirc-cmd-quit
;;:desc "Reconnect"           "r" #'rcirc-reconnect-all
;;:desc "List channels"       "l" #'rcirc-cmd-list))

(provide 'app-irc-config)

;;; app-irc-config.el ends here
