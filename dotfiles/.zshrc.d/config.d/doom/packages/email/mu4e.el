(require 'mu4e)

;; Use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( (:maildir "/INBOX"              :key ?i)
       (:maildir "/[Gmail].Sent Mail"  :key ?s)
       (:maildir "/[Gmail].Trash"      :key ?t)
       (:maildir "/[Gmail].All Mail"   :key ?a)))

(add-to-list 'mu4e-bookmarks
    ;; ':favorite t' i.e, use this one for the modeline
   '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "b.dostumski@gmail.com"
   user-full-name  "Borislav Dostumski"
   message-signature
    (concat
      "Borislav Dostumski\n"
      "http://www.github.com/bdostumski\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
;;(setq message-send-mail-function 'smtpmail-send-it
;;   starttls-use-gnutls t
;;   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;   smtpmail-auth-credentials
;;     '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;;   smtpmail-default-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-default-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-service 587)

(setq sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)

;; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Mu4e mail directory
(setq mu4e-maildir "~/Maildir")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
