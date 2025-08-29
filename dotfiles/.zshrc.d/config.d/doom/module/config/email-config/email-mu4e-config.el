;;; module/config/email-config/email-mu4e-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Mu4e email configuration for Gmail using offlineimap and msmtp.
;; Supports bookmarks, shortcuts, and automatic buffer cleanup after sending.

;;; Code:

(require 'mu4e)
(require 'smtpmail)

;; ----------------------------
;; Mail user agent
;; ----------------------------
(setq mail-user-agent 'mu4e-user-agent)

;; ----------------------------
;; Maildir configuration (Gmail)
;; ----------------------------
(setq mu4e-maildir "~/Maildir")  ;; Adjust to your Maildir location
(setq mu4e-drafts-folder   "/[Gmail].Drafts"
      mu4e-sent-folder     "/[Gmail].Sent Mail"
      mu4e-trash-folder    "/[Gmail].Trash")

;; Gmail IMAP handles sent messages, so do not save copies locally
(setq mu4e-sent-messages-behavior 'delete)

;; ----------------------------
;; Maildir shortcuts & bookmarks
;; ----------------------------
(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX"             :key ?i)
         (:maildir "/[Gmail].Sent Mail" :key ?s)
         (:maildir "/[Gmail].Trash"     :key ?t)
         (:maildir "/[Gmail].All Mail"  :key ?a)))

;; Add a bookmark for Inbox and mark it as favorite (shown in modeline)
(add-to-list 'mu4e-bookmarks
             '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

;; ----------------------------
;; Mail fetching
;; ----------------------------
(setq mu4e-get-mail-command "offlineimap")

;; ----------------------------
;; User identity & signature
;; ----------------------------
(setq user-mail-address "b.dostumski@gmail.com"
      user-full-name  "Borislav Dostumski"
      message-signature
      (concat
       "Borislav Dostumski\n"
       "http://www.github.com/bdostumski\n"))

;; ----------------------------
;; Sending mail via msmtp or smtpmail
;; ----------------------------
(setq sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)

;; Optional SMTP via smtpmail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;         '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; ----------------------------
;; Additional options
;; ----------------------------
(setq message-kill-buffer-on-exit t) ;; Automatically kill message buffer after sending

;; Optional authentication sources
;; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

(provide 'email-mu4e-config)

;;; email-mu4e-config.el ends here
