;;; module/config/tools-config/tools-taskrunner-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive task runner configuration for Doom Emacs

;; TODO add more tasks like mvn, npm 

;;; Code:

;; ----------------------------
;; Enhanced Taskrunner Configuration
;; ----------------------------
(after! taskrunner
  ;; Intelligent default runner selection
  (setq +taskrunner-default-runner 'auto)
  
  ;; Performance and behavior settings
  (setq +taskrunner-auto-refresh t
        +taskrunner-cache-timeout 300
        +taskrunner-async-execution t
        +taskrunner-save-before-run t))

;; ----------------------------
;; Multi-Language Task Detection
;; ----------------------------
(defvar +taskrunner/runners-priority-list
  '((make . ("Makefile" "makefile" "GNUmakefile"))
    (npm . ("package.json"))
    (yarn . ("yarn.lock" "package.json"))
    (cargo . ("Cargo.toml"))
    (rake . ("Rakefile" "rakefile"))
    (gradle . ("build.gradle" "build.gradle.kts" "gradlew"))
    (maven . ("pom.xml"))
    (go . ("go.mod" "go.sum"))
    (poetry . ("pyproject.toml" "poetry.lock"))
    (cmake . ("CMakeLists.txt"))
    (just . ("justfile" "Justfile"))
    (composer . ("composer.json")))
  "Priority list for task runner detection.")

(defun +taskrunner/detect-project-runner ()
  "Intelligently detect the best task runner for current project."
  (when-let ((project-root (or (projectile-project-root) default-directory)))
    (catch 'found
      (dolist (runner +taskrunner/runners-priority-list)
        (let ((runner-name (car runner))
              (files (cdr runner)))
          (dolist (file files)
            (when (file-exists-p (expand-file-name file project-root))
              (throw 'found runner-name))))))))

;; ----------------------------
;; Task Discovery and Parsing
;; ----------------------------
(defun +taskrunner/discover-tasks ()
  "Discover available tasks for current project."
  (interactive)
  (let ((runner (+taskrunner/detect-project-runner))
        (project-root (or (projectile-project-root) default-directory)))
    
    (with-current-buffer (get-buffer-create "*Available Tasks*")
      (erase-buffer)
      (insert "# Available Tasks\n\n")
      (insert (format "**Project:** %s\n" (if (projectile-project-root) 
                                              (projectile-project-name) 
                                              "Current Directory")))
      (insert (format "**Runner:** %s\n" (or runner "None detected")))
      (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      
      (when runner
        (pcase runner
          ('make (+taskrunner/parse-makefile-tasks project-root))
          ('npm (+taskrunner/parse-npm-tasks project-root))
          ('yarn (+taskrunner/parse-npm-tasks project-root))
          ('cargo (+taskrunner/parse-cargo-tasks project-root))
          ('go (+taskrunner/parse-go-tasks project-root))
          ('just (+taskrunner/parse-just-tasks project-root))
          (_ (insert "## No task parser available for this runner\n\n"))))
      
      (insert "## Quick Actions\n\n")
      (insert "- `SPC e t r r` - Run task\n")
      (insert "- `SPC e t r l` - Run last task\n")
      (insert "- `SPC e t r d` - Discover tasks\n")
      (insert "- `SPC e t r e` - Edit task file\n")
      
      (markdown-mode)
      (display-buffer (current-buffer)))))

;; ----------------------------
;; Task Parser Functions
;; ----------------------------
(defun +taskrunner/parse-makefile-tasks (project-root)
  "Parse Makefile targets."
  (let ((makefile (or (expand-file-name "Makefile" project-root)
                     (expand-file-name "makefile" project-root))))
    (when (file-exists-p makefile)
      (insert "## Makefile Targets\n\n")
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
          (let ((target (match-string 1)))
            (unless (string-prefix-p "." target)
              (with-current-buffer "*Available Tasks*"
                (insert (format "- **%s**\n" target)))))))
      (insert "\n"))))

(defun +taskrunner/parse-npm-tasks (project-root)
  "Parse npm scripts from package.json."
  (let ((package-file (expand-file-name "package.json" project-root)))
    (when (file-exists-p package-file)
      (insert "## NPM Scripts\n\n")
      (condition-case nil
          (let* ((json-object-type 'hash-table)
                 (json-contents (json-read-file package-file))
                 (scripts (gethash "scripts" json-contents)))
            (when scripts
              (maphash (lambda (key value)
                        (insert (format "- **%s**: `%s`\n" key value)))
                      scripts)))
        (error (insert "- Error parsing package.json\n")))
      (insert "\n"))))

(defun +taskrunner/parse-cargo-tasks (project-root)
  "Parse Cargo tasks from Cargo.toml."
  (when (file-exists-p (expand-file-name "Cargo.toml" project-root))
    (insert "## Cargo Commands\n\n")
    (let ((common-tasks '(("build" "Build the project")
                         ("run" "Run the project")
                         ("test" "Run tests")
                         ("check" "Check compilation")
                         ("clean" "Clean build artifacts")
                         ("doc" "Build documentation")
                         ("fmt" "Format code")
                         ("clippy" "Run clippy linter"))))
      (dolist (task common-tasks)
        (insert (format "- **%s**: %s\n" (car task) (cadr task)))))
    (insert "\n")))

(defun +taskrunner/parse-go-tasks (project-root)
  "Parse Go tasks."
  (when (file-exists-p (expand-file-name "go.mod" project-root))
    (insert "## Go Commands\n\n")
    (let ((common-tasks '(("build" "Build the project")
                         ("run" "Run the project")
                         ("test" "Run tests")
                         ("mod tidy" "Clean up dependencies")
                         ("fmt" "Format code")
                         ("vet" "Examine code"))))
      (dolist (task common-tasks)
        (insert (format "- **%s**: %s\n" (car task) (cadr task)))))
    (insert "\n")))

(defun +taskrunner/parse-just-tasks (project-root)
  "Parse Just tasks from justfile."
  (let ((justfile (or (expand-file-name "justfile" project-root)
                     (expand-file-name "Justfile" project-root))))
    (when (file-exists-p justfile)
      (insert "## Just Recipes\n\n")
      (with-temp-buffer
        (insert-file-contents justfile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
          (let ((recipe-name (match-string 1)))
            (with-current-buffer "*Available Tasks*"
              (insert (format "- **%s**\n" recipe-name))))))
      (insert "\n"))))

;; ----------------------------
;; Enhanced Task Execution
;; ----------------------------
(defun +taskrunner/run-task ()
  "Run a task with intelligent runner detection."
  (interactive)
  (let* ((runner (+taskrunner/detect-project-runner))
         (tasks (+taskrunner/get-available-tasks runner))
         (task (completing-read "Run task: " tasks)))
    
    (when task
      (+taskrunner/execute-task runner task))))

(defun +taskrunner/get-available-tasks (runner)
  "Get list of available tasks for runner."
  (pcase runner
    ('make (+taskrunner/get-makefile-targets))
    ('npm (+taskrunner/get-npm-scripts))
    ('yarn (+taskrunner/get-npm-scripts))
    ('cargo '("build" "run" "test" "check" "clean" "doc" "fmt" "clippy"))
    ('go '("build" "run" "test" "mod tidy" "fmt" "vet"))
    ('just (+taskrunner/get-just-recipes))
    (_ '("build" "test" "clean" "install"))))

(defun +taskrunner/get-makefile-targets ()
  "Extract targets from Makefile."
  (let ((targets '())
        (makefile (or "Makefile" "makefile")))
    (when (file-exists-p makefile)
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
          (let ((target (match-string 1)))
            (unless (string-prefix-p "." target)
              (push target targets))))))
    (reverse targets)))

(defun +taskrunner/get-npm-scripts ()
  "Extract scripts from package.json."
  (let ((scripts '()))
    (when (file-exists-p "package.json")
      (condition-case nil
          (let* ((json-object-type 'hash-table)
                 (json-contents (json-read-file "package.json"))
                 (script-obj (gethash "scripts" json-contents)))
            (when script-obj
              (maphash (lambda (key _value) (push key scripts)) script-obj)))
        (error nil)))
    (reverse scripts)))

(defun +taskrunner/get-just-recipes ()
  "Extract recipes from justfile."
  (let ((recipes '())
        (justfile (or "justfile" "Justfile")))
    (when (file-exists-p justfile)
      (with-temp-buffer
        (insert-file-contents justfile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\)" nil t)
          (push (match-string 1) recipes))))
    (reverse recipes)))

(defun +taskrunner/execute-task (runner task)
  "Execute TASK using RUNNER."
  (let* ((project-root (or (projectile-project-root) default-directory))
         (default-directory project-root)
         (command (pcase runner
                   ('make (format "make %s" task))
                   ('npm (format "npm run %s" task))
                   ('yarn (format "yarn %s" task))
                   ('cargo (format "cargo %s" task))
                   ('go (format "go %s" task))
                   ('just (format "just %s" task))
                   (_ task))))
    
    (message "🚀 Running: %s" command)
    (compile command)
    (+taskrunner/save-task-to-history command)))

;; ----------------------------
;; Task History and Management
;; ----------------------------
(defvar +taskrunner/task-history '()
  "History of executed tasks.")

(defvar +taskrunner/favorite-tasks '()
  "List of favorite tasks.")

(defun +taskrunner/run-last-task ()
  "Run the last executed task."
  (interactive)
  (if +taskrunner/task-history
      (let ((last-task (car +taskrunner/task-history)))
        (message "🔄 Re-running: %s" last-task)
        (compile last-task))
    (message "No previous task to run")
    (+taskrunner/run-task)))

(defun +taskrunner/save-task-to-history (command)
  "Save executed task to history."
  (push command +taskrunner/task-history)
  (when (> (length +taskrunner/task-history) 10)
    (setq +taskrunner/task-history (seq-take +taskrunner/task-history 10))))

(defun +taskrunner/add-to-favorites ()
  "Add current task to favorites."
  (interactive)
  (when +taskrunner/task-history
    (let ((task (car +taskrunner/task-history)))
      (unless (member task +taskrunner/favorite-tasks)
        (push task +taskrunner/favorite-tasks)
        (message "⭐ Added '%s' to favorites" task)))))

(defun +taskrunner/run-favorite ()
  "Run a favorite task."
  (interactive)
  (if +taskrunner/favorite-tasks
      (let ((task (completing-read "Run favorite: " +taskrunner/favorite-tasks)))
        (when task
          (compile task)
          (+taskrunner/save-task-to-history task)))
    (message "No favorite tasks saved")))

;; ----------------------------
;; Template Creation
;; ----------------------------
(defun +taskrunner/create-taskfile ()
  "Create a task file template for current project."
  (interactive)
  (let ((template-choice (completing-read "Create task file: " 
                                        '("Makefile" "justfile" "package.json"))))
    (pcase template-choice
      ("Makefile" (+taskrunner/create-makefile-template))
      ("justfile" (+taskrunner/create-justfile-template))
      ("package.json" (+taskrunner/create-package-json-template)))))

(defun +taskrunner/create-makefile-template ()
  "Create a Makefile template."
  (let ((makefile-path "Makefile"))
    (when (or (not (file-exists-p makefile-path))
              (y-or-n-p "Makefile exists. Overwrite? "))
      (with-temp-file makefile-path
        (insert ".PHONY: all build test clean install help\n\n")
        (insert "all: build\n\n")
        (insert "build:\n\t@echo \"Building project...\"\n\n")
        (insert "test:\n\t@echo \"Running tests...\"\n\n")
        (insert "clean:\n\t@echo \"Cleaning...\"\n\n")
        (insert "install:\n\t@echo \"Installing dependencies...\"\n\n")
        (insert "help:\n\t@echo \"Available targets: build test clean install\"\n"))
      (find-file makefile-path)
      (message "✅ Created Makefile template"))))

(defun +taskrunner/create-justfile-template ()
  "Create a justfile template."
  (let ((justfile-path "justfile"))
    (when (or (not (file-exists-p justfile-path))
              (y-or-n-p "justfile exists. Overwrite? "))
      (with-temp-file justfile-path
        (insert "# justfile for project automation\n\n")
        (insert "default: build\n\n")
        (insert "build:\n    echo \"Building project...\"\n\n")
        (insert "test:\n    echo \"Running tests...\"\n\n")
        (insert "clean:\n    echo \"Cleaning...\"\n\n")
        (insert "install:\n    echo \"Installing dependencies...\"\n\n")
        (insert "help:\n    just --list\n"))
      (find-file justfile-path)
      (message "✅ Created justfile template"))))

(defun +taskrunner/create-package-json-template ()
  "Create a package.json template."
  (let ((package-path "package.json"))
    (when (or (not (file-exists-p package-path))
              (y-or-n-p "package.json exists. Overwrite? "))
      (with-temp-file package-path
        (insert "{\n")
        (insert "  \"name\": \"project\",\n")
        (insert "  \"version\": \"1.0.0\",\n")
        (insert "  \"scripts\": {\n")
        (insert "    \"build\": \"echo 'Building project...'\",\n")
        (insert "    \"test\": \"echo 'Running tests...'\",\n")
        (insert "    \"clean\": \"echo 'Cleaning...'\",\n")
        (insert "    \"dev\": \"echo 'Starting development server...'\"\n")
        (insert "  }\n")
        (insert "}\n"))
      (find-file package-path)
      (message "✅ Created package.json template"))))

;; ----------------------------
;; Utility Functions
;; ----------------------------
(defun +taskrunner/quick-task (task-name)
  "Run a common task quickly."
  (let ((runner (+taskrunner/detect-project-runner)))
    (if runner
        (+taskrunner/execute-task runner task-name)
      (message "No task runner detected for this project"))))

(defun +taskrunner/edit-task-file ()
  "Edit the task file for current project."
  (interactive)
  (let ((runner (+taskrunner/detect-project-runner)))
    (when runner
      (let ((file (pcase runner
                    ('make (or "Makefile" "makefile"))
                    ('npm "package.json")
                    ('cargo "Cargo.toml")
                    ('just (or "justfile" "Justfile"))
                    (_ nil))))
        (if (and file (file-exists-p file))
            (find-file file)
          (message "No task file found for %s" runner))))))

;; ----------------------------
;; Project Integration
;; ----------------------------
(defun +taskrunner/setup-project-tasks ()
  "Setup task environment when switching projects."
  (when-let ((runner (+taskrunner/detect-project-runner)))
    (run-with-timer 1 nil
                    (lambda ()
                      (message "🔧 Task runner detected: %s | Use SPC e t r d to discover tasks" runner)))))

(when (featurep 'projectile)
  (add-hook 'projectile-after-switch-project-hook #'+taskrunner/setup-project-tasks))

(provide 'tools-taskrunner-config)

;;; tools-taskrunner-config.el ends here
