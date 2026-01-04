;;; module/config/lang-config/lang-rest-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Restclient configuration for Doom Emacs.
;; Sets default response buffer and leader keybindings for sending requests
;; and viewing responses.

;;; Code:

(after! restclient
  ;; Set default response buffer
  (setq restclient-same-buffer-response-name "*REST Response*"))

(provide 'lang-rest-config)

;;; lang-rest-config.el ends here
