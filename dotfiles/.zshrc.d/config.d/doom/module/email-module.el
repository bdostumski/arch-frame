;;; module/email-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module handles email integration in Emacs.
;; Currently, it loads and configures Mu4e for email reading,
;; composing, and managing directly from Emacs.

;;; Code:

;; Mu4e email client configuration
(load! "config/email-config/email-mu4e-config.el")

(provide 'email-module)

;;; email-module.el ends here
