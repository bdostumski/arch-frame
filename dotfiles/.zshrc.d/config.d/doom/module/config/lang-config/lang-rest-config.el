;;; module/config/lang-config/lang-rest-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Restclient configuration for Doom Emacs.
;; Sets default response buffer and leader keybindings for sending requests
;; and viewing responses.

;;; Code:

(after! restclient
  ;; Set default response buffer
  (setq restclient-same-buffer-response-name "*REST Response*"))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("r" . "rest")
;;       :desc "Send request at point" "s" #'restclient-http-send-current
;;       :desc "View last response" "v" #'restclient-show-response))

(provide 'lang-rest-config)

;;; lang-rest-config.el ends here
