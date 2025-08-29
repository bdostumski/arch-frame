;;; configurations/email-config.el --- Email Configuration with mu4e -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for mu4e email client with Gmail integration and msmtp sending.

;;; Code:

(require 'mu4e)

;; Use mu4e as the mail user agent in Emacs
(setq mail-user-agent 'mu4e-user-agent)

;; ----------------------------------------
;; Maildir folders (Gmail-specific)
;; ----------------------------------------

(setq mu4e-maildir "~/Maildir")  ;; Adjust this to your Maildir location

(setq mu4e-drafts-folder   "/[Gmail].Drafts"
      mu4e-sent-folder     "/[Gmail].Sent Mail"
      mu4e-trash-folder    "/[Gmail].Trash")

;; Gmail IMAP handles sent messages, so do not save copies locally
(setq mu4e-sent-messages-behavior 'delete)

;; ----------------------------------------
;; Maildir shortcuts for quick navigation
;; ----------------------------------------

(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX"             :key ?i)
         (:maildir "/[Gmail].Sent Mail" :key ?s)
         (:maildir "/[Gmail].Trash"     :key ?t)
         (:maildir "/[Gmail].All Mail"  :key ?a)))

;; Add a bookmark for Inbox and mark it as favorite (shown in modeline)
(add-to-list 'mu4e-bookmarks
             '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

;; ----------------------------------------
;; Mail fetching command
;; ----------------------------------------

(setq mu4e-get-mail-command "offlineimap")

;; ----------------------------------------
;; User identity and signature
;; ----------------------------------------

(setq user-mail-address "b.dostumski@gmail.com"
      user-full-name  "Borislav Dostumski"
      message-signature
      (concat
       "Borislav Dostumski\n"
       "http://www.github.com/bdostumski\n"))

;; ----------------------------------------
;; Sending mail configuration
;; ----------------------------------------

(require 'smtpmail)

;; Use msmtp for sending mail
(setq sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)

;; Uncomment and configure if you want to use SMTP directly via smtpmail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;         '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; ----------------------------------------
;; Additional options
;; ----------------------------------------

;; Don't keep message buffers around after sending
(setq message-kill-buffer-on-exit t)

;; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")) ;; Uncomment to configure authentication sources

;;; email-config.el ends here
