;;; module/config/tools-config/tools-projectile-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Projectile configuration for Doom Emacs
;; Enhanced project management with intelligent discovery, caching, and workflow integration
;; Generic configuration for modern development workflows

;;; Code:

;; ----------------------------
;; Enhanced Projectile Configuration
;; ----------------------------
(use-package! projectile
  :demand t
  :init
  ;; Intelligent project search paths
  (setq projectile-project-search-path 
        '("~/Workspace/" 
          "~/.config/"
          "~/Documents/"))
  
  ;; Performance optimizations
  (setq projectile-enable-caching t
        projectile-indexing-method 'hybrid
        projectile-use-native-indexing t
        projectile-fuzzy-match t
        projectile-completion-system 'vertico)
  
  ;; Enhanced ignored patterns
  (setq projectile-globally-ignored-directories
        '(".git" ".svn" ".hg" "_darcs" "CVS"
          "node_modules" "bower_components"
          "vendor" "composer"
          ".bundle" "gems"
          "__pycache__" ".pytest_cache" ".tox"
          "target" ".gradle"
          "build" "dist" "out"
          ".stack-work" ".cabal-sandbox"
          "elm-stuff"
          ".cargo" "target"
          ".vs" ".vscode"
          ".idea" "*.iml"
          ".vagrant" ".docker"
          ".terraform" ".pulumi"
          "coverage" ".nyc_output"
          "logs" "tmp" "temp"))
  
  (setq projectile-globally-ignored-files
        '("TAGS" "GTAGS" "GRTAGS" "GPATH"
          "*.pyc" "*.pyo" "*.class"
          "*.o" "*.so" "*.dll" "*.exe"
          "*.log" "*.tmp" "*.swp" "*.swo"
          "*.min.js" "*.min.css"
          ".DS_Store" "Thumbs.db"
          "*.orig" "*.rej"
          "package-lock.json" "yarn.lock"
          "Cargo.lock" "poetry.lock"))
  
  ;; Project type detection improvements
  (setq projectile-project-root-files
        '(".projectile" ".git" ".hg" ".svn" "_darcs"
          "package.json" "Cargo.toml" "go.mod" "poetry.lock"
          "requirements.txt" "Pipfile" "setup.py"
          "Gemfile" "mix.exs" "deps.edn" "project.clj"
          "pom.xml" "build.gradle" "CMakeLists.txt" "Makefile"
          "composer.json" "elm.json" "stack.yaml"))
  
  (setq projectile-project-root-files-bottom-up
        '(".projectile" ".git" ".hg" ".svn"))
  
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-generic-command "find . -type f -print0")
  
  :config
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  (setq projectile-project-name-function #'projectile-default-project-name))

;; Enable Projectile globally
(projectile-mode +1)

;; ----------------------------
;; Enhanced Project Discovery
;; ----------------------------
(defun +projectile/discover-projects ()
  "Discover and add projects from search paths."
  (interactive)
  (let ((discovered 0))
    (dolist (search-path projectile-project-search-path)
      (when (file-directory-p search-path)
        (dolist (dir (directory-files search-path t "^[^.]"))
          (when (and (file-directory-p dir)
                     (projectile-project-p dir)
                     (not (projectile-known-project-p dir))) 
            (projectile-add-known-project dir)
            (setq discovered (1+ discovered))))))
    (message "✅ Discovered %d new projects" discovered)
    (projectile-cleanup-known-projects)))

(defun +projectile/cleanup-stale-projects ()
  "Remove non-existent projects from known projects list."
  (interactive)
  (let ((removed 0))
    (dolist (project (projectile-relevant-known-projects))
      (unless (file-directory-p project)
        (projectile-remove-known-project project)
        (setq removed (1+ removed))))
    (message "🧹 Removed %d stale projects" removed)))

;; ----------------------------
;; Project Information and Analysis
;; ----------------------------
(defun +projectile/project-size-in-mb ()
  "Calculate project size in MB."
  (when-let ((project-root (projectile-project-root)))
    (let ((size-output (shell-command-to-string 
                       (format "du -sm %s 2>/dev/null | cut -f1" (shell-quote-argument project-root)))))
      (if (string-empty-p (string-trim size-output))
          0
        (string-to-number (string-trim size-output))))))

(defun +projectile/project-info ()
  "Show comprehensive project information."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let* ((project-name (projectile-project-name))
             (project-type (projectile-project-type))
             (project-files (length (projectile-current-project-files)))
             (project-size (+projectile/project-size-in-mb))
             (git-branch (when (file-directory-p (concat project-root ".git"))
                          (string-trim (shell-command-to-string "git branch --show-current 2>/dev/null")))))
        
        (with-current-buffer (get-buffer-create "*Project Info*")
          (erase-buffer)
          (insert (format "# Project Information: %s\n\n" project-name))
          (insert (format "**Date:** %s\n" (format-time-string "%Y-%m-%d %H:%M")))
          (insert (format "**Path:** %s\n\n" project-root))
          
          (insert "## Overview\n\n")
          (insert (format "- **Name:** %s\n" project-name))
          (insert (format "- **Type:** %s\n" (or project-type "Generic")))
          (insert (format "- **Files:** %d\n" project-files))
          (insert (format "- **Size:** %.2f MB\n" project-size))
          (when (and git-branch (not (string-empty-p git-branch)))
            (insert (format "- **Git Branch:** %s\n" git-branch)))
          (insert "\n")
          
          (insert "## Project Structure\n\n")
          (let ((dirs (directory-files project-root nil "^[^.]" t)))
            (dolist (dir (seq-take (sort dirs #'string<) 10))
              (when (file-directory-p (expand-file-name dir project-root))
                (insert (format "- 📁 `%s/`\n" dir)))))
          (insert "\n")
          
          (insert "## Recent Activity\n\n")
          (when (file-directory-p (concat project-root ".git"))
            (let ((recent-commits (shell-command-to-string 
                                  "git log --oneline -5 --format='%h %s (%cr)' 2>/dev/null")))
              (unless (string-empty-p recent-commits)
                (insert "```\n")
                (insert recent-commits)
                (insert "```\n"))))
          
          (insert "## Quick Actions\n\n")
          (insert "- `SPC e t p f` - Find file in project\n")
          (insert "- `SPC e t p s` - Search in project\n")
          (insert "- `SPC e t p t` - Toggle between files\n")
          (insert "- `SPC e t p p` - Switch to other project\n")
          (insert "- `SPC e t p r` - Recent files\n")
          
          (markdown-mode)
          (display-buffer (current-buffer))))
    (message "Not in a project")))

;; ----------------------------
;; Enhanced Project Switching
;; ----------------------------
(defun +projectile/recent-projects ()
  "Show recently accessed projects with quick switching."
  (interactive)
  (if projectile-known-projects
      (with-current-buffer (get-buffer-create "*Recent Projects*")
        (erase-buffer)
        (insert "# Recent Projects\n\n")
        (insert (format "**Total Projects:** %d\n\n" (length projectile-known-projects)))
        
        (insert "## Quick Switch\n\n")
        (let ((counter 1))
          (dolist (project (seq-take projectile-known-projects 10))
            (let ((name (file-name-nondirectory (directory-file-name project)))
                  (exists (file-directory-p project)))
              (insert (format "%d. %s **%s**\n   `%s`\n\n"
                             counter
                             (if exists "✅" "❌")
                             name
                             project))
              (setq counter (1+ counter)))))
        
        (insert "## Actions\n\n")
        (insert "- `SPC e t p p` - Switch to project\n")
        (insert "- `SPC e t p D` - Discover new projects\n")
        (insert "- `SPC e t p C` - Cleanup stale projects\n")
        
        (markdown-mode)
        (display-buffer (current-buffer)))
    (message "No known projects")))

;; ----------------------------
;; Project Templates and Initialization
;; ----------------------------
(defun +projectile/create-project ()
  "Create new project with template structure."
  (interactive)
  (let* ((project-types '("Generic" "Web Frontend" "Node.js" "Python" "Rust" "Go" "Documentation"))
         (project-type (completing-read "Project type: " project-types))
         (project-name (read-string "Project name: "))
         (base-dir (read-directory-name "Create in directory: " "~/Workspace/"))
         (project-dir (expand-file-name project-name base-dir)))
    
    (when (file-exists-p project-dir)
      (user-error "Project directory already exists"))
    
    (make-directory project-dir t)
    
    ;; Create basic structure based on type
    (pcase project-type
      ("Web Frontend"
       (make-directory (expand-file-name "src" project-dir))
       (make-directory (expand-file-name "assets" project-dir))
       (with-temp-file (expand-file-name "package.json" project-dir)
         (insert (format "{\n  \"name\": \"%s\",\n  \"version\": \"1.0.0\",\n  \"description\": \"\"\n}\n" project-name)))
       (with-temp-file (expand-file-name "index.html" project-dir)
         (insert "<!DOCTYPE html>\n<html>\n<head>\n  <title>" project-name "</title>\n</head>\n<body>\n  <h1>" project-name "</h1>\n</body>\n</html>\n")))
      
      ("Node.js"
       (make-directory (expand-file-name "src" project-dir))
       (make-directory (expand-file-name "test" project-dir))
       (with-temp-file (expand-file-name "package.json" project-dir)
         (insert (format "{\n  \"name\": \"%s\",\n  \"version\": \"1.0.0\",\n  \"main\": \"src/index.js\",\n  \"scripts\": {\n    \"start\": \"node src/index.js\"\n  }\n}\n" project-name)))
       (with-temp-file (expand-file-name "src/index.js" project-dir)
         (insert "console.log('Hello from " project-name "!');\n")))
      
      ("Python"
       (make-directory (expand-file-name "src" project-dir))
       (make-directory (expand-file-name "tests" project-dir))
       (with-temp-file (expand-file-name "requirements.txt" project-dir)
         (insert "# Add your dependencies here\n"))
       (with-temp-file (expand-file-name "src/main.py" project-dir)
         (insert "#!/usr/bin/env python3\n\ndef main():\n    print('Hello from " project-name "!')\n\nif __name__ == '__main__':\n    main()\n")))
      
      ("Rust"
       (when (executable-find "cargo")
         (shell-command (format "cd %s && cargo init --name %s" (shell-quote-argument base-dir) project-name))))
      
      ("Go"
       (with-temp-file (expand-file-name "go.mod" project-dir)
         (insert (format "module %s\n\ngo 1.21\n" project-name)))
       (with-temp-file (expand-file-name "main.go" project-dir)
         (insert "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello from " project-name "!\")\n}\n")))
      
      ("Documentation"
       (make-directory (expand-file-name "docs" project-dir))
       (with-temp-file (expand-file-name "README.md" project-dir)
         (insert (format "# %s\n\n## Overview\n\nDescription of your project.\n\n## Usage\n\n## Contributing\n\n## License\n" project-name))))
      
      (_ ;; Generic
       (make-directory (expand-file-name "src" project-dir))
       (make-directory (expand-file-name "docs" project-dir))))
    
    ;; Always create these files
    (unless (file-exists-p (expand-file-name "README.md" project-dir))
      (with-temp-file (expand-file-name "README.md" project-dir)
        (insert (format "# %s\n\nProject description.\n" project-name))))
    
    (with-temp-file (expand-file-name ".gitignore" project-dir)
      (insert "# Dependencies\nnode_modules/\n__pycache__/\ntarget/\n\n# IDE\n.vscode/\n.idea/\n\n# OS\n.DS_Store\nThumbs.db\n\n# Logs\n*.log\n"))
    
    ;; Add to known projects
    (projectile-add-known-project project-dir)
    (projectile-switch-project-by-name project-dir)
    
    (message "✅ Created %s project: %s" project-type project-name)))

;; ----------------------------
;; Enhanced Search Integration
;; ----------------------------
(use-package! deadgrep
  :after projectile
  :commands (deadgrep)
  :config
  (setq deadgrep-project-root-function #'projectile-project-root
        deadgrep-display-buffer-function #'switch-to-buffer-other-window))

(defun +projectile/search-project ()
  "Enhanced project search with fallback options."
  (interactive)
  (cond
   ((executable-find "rg")
    (call-interactively #'deadgrep))
   ((executable-find "ag")
    (call-interactively #'projectile-ag))
   ((executable-find "grep")
    (call-interactively #'projectile-grep))
   (t
    (call-interactively #'projectile-search-in-project))))

;; ----------------------------
;; Project Bookmarks and Favorites
;; ----------------------------
(defvar +projectile-favorite-projects nil
  "List of favorite project paths.")

(defun +projectile/add-to-favorites ()
  "Add current project to favorites."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (progn
        (add-to-list '+projectile-favorite-projects project-root)
        (message "⭐ Added %s to favorites" (projectile-project-name)))
    (message "Not in a project")))

(defun +projectile/remove-from-favorites ()
  "Remove current project from favorites."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (progn
        (setq +projectile-favorite-projects
              (remove project-root +projectile-favorite-projects))
        (message "🗑️ Removed %s from favorites" (projectile-project-name)))
    (message "Not in a project")))

(defun +projectile/switch-to-favorite ()
  "Switch to favorite project."
  (interactive)
  (if +projectile-favorite-projects
      (let ((project (completing-read "Switch to favorite: "
                                     (mapcar (lambda (p)
                                              (file-name-nondirectory (directory-file-name p)))
                                            +projectile-favorite-projects))))
        (projectile-switch-project-by-name
         (seq-find (lambda (p) (string= (file-name-nondirectory (directory-file-name p)) project))
                  +projectile-favorite-projects)))
    (message "No favorite projects")))

;; ----------------------------
;; Enhanced Leader Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix-map ("p" . "project")
                                 ;; Core projectile commands
                                 :desc "Switch project" "p" #'projectile-switch-project
                                 :desc "Find file in project" "f" #'projectile-find-file
                                 :desc "Recent files" "r" #'projectile-recentf
                                 :desc "Search in project" "s" #'+projectile/search-project
                                 :desc "Replace in project" "R" #'projectile-replace
                                 :desc "Toggle implementation/test" "t" #'projectile-toggle-between-implementation-and-test
                                 :desc "Run project" "c" #'projectile-compile-project
                                 :desc "Test project" "T" #'projectile-test-project
                                 
                                 ;; Enhanced project management
                                 :desc "Project info" "i" #'+projectile/project-info
                                 :desc "Recent projects" "P" #'+projectile/recent-projects
                                 :desc "Create new project" "n" #'+projectile/create-project
                                 
                                 ;; Project discovery and cleanup
                                 :desc "Discover projects" "D" #'+projectile/discover-projects
                                 :desc "Cleanup stale projects" "C" #'+projectile/cleanup-stale-projects
                                 
                                 ;; Favorites
                                 (:prefix-map ("F" . "favorites")
                                  :desc "Add to favorites" "a" #'+projectile/add-to-favorites
                                  :desc "Remove from favorites" "r" #'+projectile/remove-from-favorites
                                  :desc "Switch to favorite" "f" #'+projectile/switch-to-favorite)
                                 
                                 ;; Git integration
                                 :desc "Magit status" "g" #'projectile-vc
                                 :desc "Git grep" "G" #'projectile-grep
                                 
                                 ;; Utilities
                                 :desc "Find directory" "d" #'projectile-find-dir
                                 :desc "Find other file" "o" #'projectile-find-other-file
                                 :desc "Kill project buffers" "k" #'projectile-kill-buffers
                                 :desc "Save project buffers" "S" #'projectile-save-project-buffers
                                 :desc "Invalidate cache" "I" #'projectile-invalidate-cache))))

;; ----------------------------
;; Project Hooks and Automation
;; ----------------------------
(defun +projectile/setup-project-environment ()
  "Setup project-specific environment when switching projects."
  (when-let ((project-root (projectile-project-root)))
    (let ((project-name (projectile-project-name)))
      (setenv "PROJECT_ROOT" project-root)
      (run-with-timer 1 nil
                      (lambda ()
                        (message "📁 Switched to %s | Use SPC e t p i for info" project-name))))))

(add-hook 'projectile-after-switch-project-hook #'+projectile/setup-project-environment)
(add-hook 'after-save-hook #'projectile-cleanup-known-projects)

;; Auto-discover projects on startup (delayed) - SAFE VERSION
(defun +projectile/safe-auto-discover ()
  "Safely auto-discover projects after Emacs loads."
  (when (and (featurep 'projectile) (fboundp 'projectile-known-project-p))
    (+projectile/discover-projects)))

(run-with-timer 15 nil #'+projectile/safe-auto-discover) ; Increased delay for safety

;; ----------------------------
;; Performance Optimizations
;; ----------------------------
(defun +projectile/optimize-cache ()
  "Optimize projectile cache performance."
  (setq projectile-file-exists-local-cache-expire (* 5 60))
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  
  (run-with-timer 3600 3600
                  (lambda ()
                    (when (fboundp 'projectile-cleanup-known-projects)
                      (projectile-cleanup-known-projects)))))

(+projectile/optimize-cache)

(provide 'tools-projectile-config)

;;; tools-projectile-config.el ends here
