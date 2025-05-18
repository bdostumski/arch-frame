(after! mu4e
  (setq
   ;; Maildir root directory - point directly to the 'gmail' subfolder
   mu4e-maildir (expand-file-name "~/Documents/doom/mail/gmail")

   ;; Folder names inside the maildir - case sensitive
   mu4e-inbox-folder "/INBOX"
   mu4e-sent-folder "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder "/Trash"
   mu4e-refile-folder "/Archive"

   ;; Command to fetch mail (mbsync syncs all accounts)
   mu4e-get-mail-command "mbsync -a"

   ;; Update interval for mail index in seconds (5 minutes)
   mu4e-update-interval 300
   mu4e-index-update-inhibit nil

   ;; Viewing settings
   mu4e-view-show-images t
   mu4e-view-show-addresses t

   ;; Composing settings
   mu4e-compose-format-flowed t
   mu4e-compose-signature-auto-include t

   ;; Your identity
   user-full-name "Borislav Dostumski"
   user-mail-address "b.dostumski@gmail.com"
   mu4e-user-mail-address-list '("b.dostumski@gmail.com")

   ;; Make mu4e rename files when moving (avoids copy+delete)
   mu4e-change-filenames-when-moving t

   ;; Maildir shortcuts for quick navigation in mu4e
   mu4e-maildir-shortcuts
   '(("/INBOX"    . ?i)
     ("/Sent"     . ?s)
     ("/Drafts"   . ?d)
     ("/Trash"    . ?t)
     ("/Archive"  . ?a))))

;; Use w3m to render HTML mails
(setq mu4e-html2text-command "w3m -T text/html")

;; Configure sending mail using msmtp
(setq sendmail-program "/usr/bin/msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))

;; Auth sources for msmtp and IMAP (encrypted credentials)
(setq auth-sources '("~/.authinfo.gpg"))

;; Enable signing outgoing mail with your GPG key
(setq mml-secure-openpgp-sign-with-sender t)
