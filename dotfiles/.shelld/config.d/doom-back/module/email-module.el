;;; module/email-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module centralizes all email-related configurations in Emacs.
;;
;; Currently, it focuses on Mu4e, a lightweight email client for Emacs.
;; It is designed to allow reading, composing, and managing emails directly
;; inside Emacs.
;;
;; Safety/load order rationale:
;; - Mu4e configuration is loaded first because it may depend on Emacs' network,
;;   org-mode (for org-capture/email links), and GPG/encryption setup.
;; - Additional email backends or enhancements (if added later) should be
;;   loaded after Mu4e to ensure the base email functionality is available.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Mu4e email client
;; Provides full-featured email management inside Emacs.
;; Features include:
;;   - Reading and composing emails
;;   - Folder management (IMAP/Local)
;;   - Integration with GPG/Encryption
;;   - Org-mode links, captures, and attachments
;; ---------------------------------------------------------------------------
;;(load! "config/email-config/email-mu4e-config.el")

(provide 'email-module)

;;; email-module.el ends here
