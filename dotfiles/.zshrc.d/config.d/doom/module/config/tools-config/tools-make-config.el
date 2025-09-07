;;; module/config/tools-config/tools-make-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Make integration for Doom Emacs

;;; Code:

;; ----------------------------
;; Enhanced Make Configuration
;; ----------------------------
(after! compile
  ;; Better compilation experience
  (setq compilation-scroll-output t
        compilation-window-height 15
        compilation-auto-jump-to-first-error t
        compilation-ask-about-save nil))

;; ----------------------------
;; Project-Aware Commands
;; ----------------------------
(defvar +make/project-commands
  '(("arch-frame" . (("install" . "make install")
                    ("backup" . "make backup")
                    ("clean" . "make clean")
                    ("update" . "sudo pacman -Syu")))
    ("learning-javascript" . (("dev" . "npm run dev")
                              ("test" . "npm test")
                              ("build" . "npm run build")
                              ("start" . "npm start")))
    ("learning-workspace" . (("build" . "make build")
                             ("test" . "make test")
                             ("clean" . "make clean")))
    ("bdostumski.github.io" . (("serve" . "bundle exec jekyll serve")
                               ("build" . "bundle exec jekyll build")
                               ("deploy" . "git add . && git commit -m 'update' && git push")))
    ("bdostumski" . (("update" . "git add . && git commit -m 'profile: update' && git push"))))
  "Project-specific commands for user's repositories.")

(defun +make/get-project-commands ()
  "Get commands for current project."
  (when-let ((project-name (projectile-project-name)))
    (alist-get project-name +make/project-commands nil nil #'string-match-p)))

;; ----------------------------
;; Core Make Functions
;; ----------------------------
(defun +make/run-command (command)
  "Run COMMAND in compilation buffer."
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile command)))

(defun +make/run ()
  "Run make in current project."
  (interactive)
  (let* ((makefile (or (locate-dominating-file default-directory "Makefile")
                      (locate-dominating-file default-directory "makefile")))
         (command (if makefile "make" "echo 'No Makefile found'")))
    (+make/run-command command)))

(defun +make/smart-command ()
  "Run smart command based on current project."
  (interactive)
  (let* ((project-commands (+make/get-project-commands))
         (command-names (mapcar #'car project-commands)))
    (if command-names
        (let* ((selected (if (= (length command-names) 1)
                            (car command-names)
                          (completing-read "Command: " command-names)))
               (command (alist-get selected project-commands nil nil #'string=)))
          (when command
            (+make/run-command command)))
      (+make/run))))

(defun +make/run-last ()
  "Re-run the last command."
  (interactive)
  (if (bound-and-true-p compile-command)
      (+make/run-command compile-command)
    (message "No previous command found")
    (+make/smart-command)))

(defun +make/view-buffer ()
  "View compilation buffer."
  (interactive)
  (if-let ((buffer (get-buffer "*compilation*")))
      (display-buffer buffer)
    (message "No compilation buffer found")))

;; ----------------------------
;; Project-Specific Quick Commands
;; ----------------------------
(defun +make/arch-install ()
  "Install arch-frame configuration."
  (interactive)
  (+make/run-command "make install")
  (message "Installing arch-frame configuration..."))

(defun +make/js-dev ()
  "Start JavaScript development server."
  (interactive)
  (if (file-exists-p "package.json")
      (+make/run-command "npm run dev")
    (+make/run-command "python -m http.server 8000"))
  (message "Starting development server..."))

(defun +make/serve-site ()
  "Serve GitHub Pages site locally."
  (interactive)
  (if (file-exists-p "Gemfile")
      (+make/run-command "bundle exec jekyll serve")
    (+make/run-command "python -m http.server 4000"))
  (message "Starting local server..."))

;; ----------------------------
;; Build Information
;; ----------------------------
(defun +make/show-project-info ()
  "Show build information for current project."
  (interactive)
  (let* ((project-name (projectile-project-name))
         (project-commands (+make/get-project-commands))
         (has-makefile (or (file-exists-p "Makefile") (file-exists-p "makefile")))
         (has-package-json (file-exists-p "package.json"))
         (has-gemfile (file-exists-p "Gemfile")))
    
    (with-current-buffer (get-buffer-create "*Build Info*")
      (erase-buffer)
      (insert (format "# Build Info: %s\n\n" (or project-name "Unknown")))
      (insert (format "**Date:** %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "**Path:** %s\n\n" default-directory))
      
      (insert "## Build Systems\n\n")
      (when has-makefile (insert "- ✅ Makefile\n"))
      (when has-package-json (insert "- ✅ NPM (package.json)\n"))
      (when has-gemfile (insert "- ✅ Ruby/Jekyll (Gemfile)\n"))
      (unless (or has-makefile has-package-json has-gemfile)
        (insert "- ❌ No build system detected\n"))
      (insert "\n")
      
      (insert "## Available Commands\n\n")
      (if project-commands
          (dolist (cmd project-commands)
            (insert (format "- **%s:** `%s`\n" (car cmd) (cdr cmd))))
        (insert "- No project-specific commands\n"))
      (insert "\n")
      
      (insert "## Quick Actions\n\n")
      (insert "- `SPC e t m r` - Run smart command\n")
      (insert "- `SPC e t m m` - Run make\n")
      (insert "- `SPC e t m l` - Run last command\n")
      (insert "- `SPC e t m v` - View output\n")
      
      (markdown-mode)
      (display-buffer (current-buffer)))))

;; ----------------------------
;; Makefile Template Generator
;; ----------------------------
(defun +make/create-makefile ()
  "Create a Makefile template for current project."
  (interactive)
  (let* ((project-name (or (projectile-project-name) "project"))
         (makefile-path (expand-file-name "Makefile" default-directory)))
    
    (when (and (file-exists-p makefile-path)
               (not (y-or-n-p "Makefile exists. Overwrite? ")))
      (user-error "Cancelled"))
    
    (with-temp-file makefile-path
      (insert (format "# Makefile for %s\n" project-name))
      (insert (format "# Generated: %s\n\n" (format-time-string "%Y-%m-%d")))
      
      (cond
       ((string-match-p "arch-frame" project-name)
        (insert ".PHONY: install backup clean update\n\n")
        (insert "install:\n\t@echo \"Installing configuration...\"\n\n")
        (insert "backup:\n\t@echo \"Creating backup...\"\n\n")
        (insert "clean:\n\t@echo \"Cleaning up...\"\n\n")
        (insert "update:\n\tsudo pacman -Syu\n"))
       
       ((string-match-p "learning" project-name)
        (insert ".PHONY: build test clean examples\n\n")
        (insert "build:\n\t@echo \"Building examples...\"\n\n")
        (insert "test:\n\t@echo \"Running tests...\"\n\n")
        (insert "examples:\n\t@echo \"Running examples...\"\n\n")
        (insert "clean:\n\t@echo \"Cleaning up...\"\n"))
       
       (t
        (insert ".PHONY: all build test clean\n\n")
        (insert "all: build\n\n")
        (insert "build:\n\t@echo \"Building...\"\n\n")
        (insert "test:\n\t@echo \"Testing...\"\n\n")
        (insert "clean:\n\t@echo \"Cleaning...\"\n"))))
    
    (find-file makefile-path)
    (message "Created Makefile template")))

;; ----------------------------
;; Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix-map ("m" . "make")
                                 :desc "Smart command" "r" #'+make/smart-command
                                 :desc "Run make" "m" #'+make/run
                                 :desc "Run last" "l" #'+make/run-last
                                 :desc "View buffer" "v" #'+make/view-buffer
                                 :desc "Project info" "i" #'+make/show-project-info
                                 :desc "Create Makefile" "c" #'+make/create-makefile))))

;; Project-specific keybindings (only show when in relevant projects)
(when (and (featurep 'projectile) (projectile-project-name))
  (let ((project (projectile-project-name)))
    (cond
     ((string-match-p "arch-frame" project)
      (map! :leader
            (:prefix "c m"
             :desc "Install config" "I" #'+make/arch-install)))
     
     ((string-match-p "learning-javascript" project)
      (map! :leader
            (:prefix "c m"
             :desc "Dev server" "d" #'+make/js-dev)))
     
     ((string-match-p "bdostumski\\.github\\.io" project)
      (map! :leader
            (:prefix "c m"
             :desc "Serve site" "s" #'+make/serve-site))))))

;; ----------------------------
;; Project Setup Hook
;; ----------------------------
(defun +make/setup-project ()
  "Setup make environment for current project."
  (when-let ((project-name (projectile-project-name)))
    (pcase project-name
      ((pred (string-match-p "arch-frame"))
       (message "🐧 Arch-frame detected. Use SPC c m for system commands."))
      ((pred (string-match-p "learning-javascript"))
       (message "📚 JavaScript learning detected. Use SPC c m d for dev server."))
      ((pred (string-match-p "bdostumski\\.github\\.io"))
       (message "🌐 GitHub Pages detected. Use SPC c m s to serve locally."))
      ((pred (string-match-p "learning"))
       (message "📖 Learning project detected. Use SPC c m for build commands.")))))

(add-hook 'projectile-after-switch-project-hook #'+make/setup-project)

(provide 'tools-make-config)

;;; tools-make-config.el ends here
