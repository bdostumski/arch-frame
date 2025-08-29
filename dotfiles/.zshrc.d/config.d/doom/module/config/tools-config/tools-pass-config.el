;;; module/config/tools-config/tools-pass-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Password management integration for Doom Emacs using `pass`.
;; Provides leader keybindings for searching, inserting, and editing password entries.

;;; Code:

(after! pass
  ;; Optional: set default password length for new entries
  (setq password-store-password-length 20))

;; ----------------------------
;; Leader keybindings for Pass
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("p" . "passwords")
;;       :desc "Search password" "s" #'password-store-copy
;;       :desc "Insert password" "i" #'password-store-insert
;;       :desc "Edit password entry" "e" #'password-store-edit))

(provide 'tools-pass-config)

;;; tools-pass-config.el ends here
