;;; module/config/tools-config/tools-upload-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for remote file and project upload in Doom Emacs.
;; Supports SFTP and other upload methods with leader keybindings.

;;; Code:

(after! upload
  ;; Define remote mappings for projects
  (setq +upload-remote-mappings
        '(("my-project"
           :local "~/Workspace/my-project/"
           :remote "/var/www/my-project/"
           :method sftp
           :host "example.com"
           :user "username"))))

;; ----------------------------
;; Leader keybindings for upload
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("u" . "upload")
;;       :desc "Upload project" "p" #'+upload/upload-project
;;       :desc "Upload current file" "f" #'+upload/upload-current-file
;;       :desc "Sync project" "s" #'+upload/sync-project))

(provide 'tools-upload-config)

;;; tools-upload-config.el ends here
