;;; module/config/tools-config/tools-upload-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Simplified upload and deployment configuration for Doom Emacs

;;; Code:

;; ----------------------------
;; Core Upload Configuration
;; ----------------------------

(defvar +upload-remote-mappings
  '(;; GitHub Pages deployment
    ("bdostumski.github.io"
     :local "~/Workspace/bdostumski.github.io/"
     :remote "origin/main"
     :method git
     :description "Personal GitHub Pages site")
    
    ;; Profile repository
    ("bdostumski"
     :local "~/Workspace/bdostumski/"
     :remote "origin/main"
     :method git
     :description "GitHub profile repository")
    
    ;; Learning projects
    ("learning-javascript"
     :local "~/Workspace/learning-javascript/"
     :remote "origin/main"
     :method git
     :description "JavaScript learning project")
    
    ;; Systems project
    ("arch-frame"
     :local "~/Workspace/arch-frame/"
     :remote "origin/main"
     :method git
     :description "Arch Frame project")
    
    ;; Learning workspace
    ("learning-workspace"
     :local "~/Workspace/learning-workspace/"
     :remote "origin/main"
     :method git
     :description "Learning workspace"))
  "Upload target mappings for projects.")

;; ----------------------------
;; Basic Upload Functions
;; ----------------------------

(defun +upload/upload-project ()
  "Upload current project with git."
  (interactive)
  (when-let ((project-root (projectile-project-root)))
    (let ((default-directory project-root))
      (message "🚀 Uploading project...")
      (async-shell-command
       (format "git add . && git commit -m 'Update: %s' && git push"
               (format-time-string "%Y-%m-%d %H:%M"))
       "*Upload: Git Deploy*"))))

(defun +upload/deploy-to-github-pages ()
  "Deploy current repository to GitHub Pages."
  (interactive)
  (when (file-exists-p ".git")
    (let ((default-directory (projectile-project-root)))
      (message "🚀 Deploying to GitHub Pages...")
      (async-shell-command 
       (format "git add . && git commit -m 'Deploy: %s' && git push origin main"
               (format-time-string "%Y-%m-%d %H:%M"))
       "*GitHub Pages Deploy*"))))

(defun +upload/show-deployment-status ()
  "Show basic deployment status."
  (interactive)
  (with-current-buffer (get-buffer-create "*Deployment Status*")
    (erase-buffer)
    (insert "# Deployment Status\n\n")
    (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
    
    (insert "## Configured Projects\n\n")
    (dolist (mapping +upload-remote-mappings)
      (let* ((project-name (car mapping))
             (config (cdr mapping))
             (local (plist-get config :local))
             (exists (file-directory-p local)))
        (insert (format "- **%s**: %s\n" 
                       project-name 
                       (if exists "✅ Available" "❌ Missing")))))
    
    (display-buffer (current-buffer))))

(provide 'tools-upload-config)
;;; tools-upload-config.el ends here
