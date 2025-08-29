;;; config/app-config/app-everywhere-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable Emacs server to allow editing files via `emacsclient`.
;; Provides a leader keybinding for quick access.

;;; Code:

(after! server
  ;; Start Emacs server if not already running
  (unless (server-running-p)
    (server-start)))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("e" . "everywhere")
;;       :desc "Edit file with emacsclient" "f" #'server-edit))

(provide 'app-everywhere-config)

;;; app-everywhere-config.el ends here
