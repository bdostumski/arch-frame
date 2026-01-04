;;; module/config/tools-config/tools-tmux-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive tmux integration for Doom Emacs
;; Enhanced session management with project-aware workflows
;; Tailored for development environments and multi-project workflows

;;; Code:

;; ----------------------------
;; Enhanced Tmux Configuration
;; ----------------------------
(after! tmux
  ;; Default tmux session configuration
  (setq +tmux-session-name "main"
        +tmux-default-command "zsh"
        +tmux-kill-unattached t))

;; ----------------------------
;; Project-Aware Session Management
;; ----------------------------
(defvar +tmux/project-sessions
  '(("arch-frame" . "arch")
    ("learning-workspace" . "learning")
    ("learning-javascript" . "js")
    ("bdostumski.github.io" . "blog")
    ("bdostumski" . "profile")
    ("doom-config" . "emacs"))
  "Mapping of project names to tmux session names.")

(defvar +tmux/common-sessions
  '(("main" . "Main development session")
    ("system" . "System administration")
    ("monitoring" . "System monitoring")
    ("scratch" . "Quick experiments")
    ("docs" . "Documentation work"))
  "Common tmux sessions for general use.")

(defun +tmux/get-session-name (&optional project-name)
  "Get appropriate tmux session name for project."
  (if project-name
      (or (alist-get project-name +tmux/project-sessions nil nil #'string=)
          (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" project-name))
    +tmux-session-name))

(defun +tmux/current-project-session ()
  "Get session name for current project."
  (when-let ((project-root (projectile-project-root)))
    (let ((project-name (projectile-project-name)))
      (+tmux/get-session-name project-name))))

;; ----------------------------
;; Enhanced Session Management
;; ----------------------------
(defun +tmux/new-session (&optional session-name directory)
  "Create new tmux session with optional name and directory."
  (interactive)
  (let* ((name (or session-name
                  (read-string "Session name: " 
                              (+tmux/current-project-session))))
         (dir (or directory
                 (if (projectile-project-root)
                     (projectile-project-root)
                   (read-directory-name "Start directory: " default-directory))))
         (command (format "tmux new-session -d -s %s -c %s" 
                         (shell-quote-argument name)
                         (shell-quote-argument dir))))
    
    (if (= 0 (shell-command command))
        (progn
          (message "✅ Created tmux session: %s" name)
          (+tmux/setup-project-session name dir))
      (message "❌ Failed to create session: %s" name))))

(defun +tmux/switch-session ()
  "Switch to tmux session with completion."
  (interactive)
  (let* ((sessions (+tmux/get-sessions))
         (session (completing-read "Switch to session: " sessions)))
    (when session
      (let ((command (format "tmux switch-client -t %s" (shell-quote-argument session))))
        (if (= 0 (shell-command command))
            (message "🔄 Switched to session: %s" session)
          (message "❌ Failed to switch to session: %s" session))))))

(defun +tmux/kill-session ()
  "Kill tmux session with confirmation."
  (interactive)
  (let* ((sessions (+tmux/get-sessions))
         (session (completing-read "Kill session: " sessions)))
    (when (and session
               (y-or-n-p (format "Kill tmux session '%s'? " session)))
      (let ((command (format "tmux kill-session -t %s" (shell-quote-argument session))))
        (if (= 0 (shell-command command))
            (message "🗑️ Killed session: %s" session)
          (message "❌ Failed to kill session: %s" session))))))

(defun +tmux/get-sessions ()
  "Get list of active tmux sessions."
  (let ((output (shell-command-to-string "tmux list-sessions -F '#{session_name}' 2>/dev/null")))
    (if (string-empty-p output)
        nil
      (split-string (string-trim output) "\n"))))

;; ----------------------------
;; Project Integration
;; ----------------------------
(defun +tmux/create-project-session ()
  "Create tmux session for current project."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let* ((project-name (projectile-project-name))
             (session-name (+tmux/get-session-name project-name)))
        (+tmux/new-session session-name project-root))
    (message "Not in a project")))

(defun +tmux/setup-project-session (session-name project-dir)
  "Setup project-specific tmux session layout."
  (let ((base-cmd (format "tmux -t %s" (shell-quote-argument session-name))))
    ;; Rename first window to 'editor'
    (shell-command (format "%s rename-window editor" base-cmd))
    
    ;; Create additional windows based on project type
    (when (file-exists-p (expand-file-name "package.json" project-dir))
      ;; JavaScript/Node.js project
      (shell-command (format "%s new-window -n 'dev-server'" base-cmd))
      (shell-command (format "%s new-window -n 'tests'" base-cmd)))
    
    (when (file-exists-p (expand-file-name "Cargo.toml" project-dir))
      ;; Rust project
      (shell-command (format "%s new-window -n 'cargo'" base-cmd))
      (shell-command (format "%s new-window -n 'tests'" base-cmd)))
    
    (when (or (file-exists-p (expand-file-name "Makefile" project-dir))
              (file-exists-p (expand-file-name "justfile" project-dir)))
      ;; Build system project
      (shell-command (format "%s new-window -n 'build'" base-cmd)))
    
    ;; Always create a 'shell' window for general commands
    (shell-command (format "%s new-window -n 'shell'" base-cmd))
    
    ;; Go back to editor window
    (shell-command (format "%s select-window -t editor" base-cmd))))

;; ----------------------------
;; Advanced Pane Management
;; ----------------------------
(defun +tmux/send-command ()
  "Send command to tmux pane."
  (interactive)
  (let* ((sessions (+tmux/get-sessions))
         (session (completing-read "Session: " sessions))
         (windows (+tmux/get-windows session))
         (window (completing-read "Window: " windows))
         (command (read-string "Command: ")))
    
    (when (and session window command)
      (let ((tmux-cmd (format "tmux send-keys -t %s:%s '%s' Enter"
                             (shell-quote-argument session)
                             (shell-quote-argument window)
                             command)))
        (shell-command tmux-cmd)
        (message "📤 Sent command to %s:%s: %s" session window command)))))

(defun +tmux/get-windows (session)
  "Get list of windows for tmux session."
  (let ((command (format "tmux list-windows -t %s -F '#{window_name}' 2>/dev/null" 
                        (shell-quote-argument session))))
    (let ((output (shell-command-to-string command)))
      (if (string-empty-p output)
          nil
        (split-string (string-trim output) "\n")))))

(defun +tmux/send-region-to-pane ()
  "Send selected region to tmux pane."
  (interactive)
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (sessions (+tmux/get-sessions))
             (session (completing-read "Session: " sessions))
             (windows (+tmux/get-windows session))
             (window (completing-read "Window: " windows)))
        
        (when (and session window)
          (let ((tmux-cmd (format "tmux send-keys -t %s:%s '%s'"
                                 (shell-quote-argument session)
                                 (shell-quote-argument window)
                                 text)))
            (shell-command tmux-cmd)
            (message "📤 Sent region to %s:%s" session window))))
    (message "No region selected")))

;; ----------------------------
;; Workflow Automation
;; ----------------------------
(defun +tmux/development-workflow ()
  "Start complete development workflow in tmux."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let* ((project-name (projectile-project-name))
             (session-name (+tmux/get-session-name project-name)))
        
        ;; Create main session
        (+tmux/new-session session-name project-root)
        
        ;; Setup project-specific workflow
        (pcase project-name
          ("learning-javascript"
           (+tmux/setup-javascript-workflow session-name project-root))
          ("arch-frame"
           (+tmux/setup-systems-workflow session-name project-root))
          ("bdostumski.github.io"
           (+tmux/setup-documentation-workflow session-name project-root))
          (_
           (+tmux/setup-generic-workflow session-name project-root)))
        
        (message "🚀 Started development workflow for %s" project-name))
    (message "Not in a project")))

(defun +tmux/setup-javascript-workflow (session project-dir)
  "Setup JavaScript development workflow."
  (let ((base-cmd (format "tmux -t %s" (shell-quote-argument session))))
    ;; Editor window
    (shell-command (format "%s rename-window 'editor'" base-cmd))
    
    ;; Development server
    (shell-command (format "%s new-window -n 'dev-server'" base-cmd))
    (shell-command (format "%s send-keys -t dev-server 'npm run dev' Enter" base-cmd))
    
    ;; Testing
    (shell-command (format "%s new-window -n 'tests'" base-cmd))
    (shell-command (format "%s send-keys -t tests 'npm run test:watch' Enter" base-cmd))
    
    ;; Git/shell
    (shell-command (format "%s new-window -n 'git'" base-cmd))
    
    ;; Back to editor
    (shell-command (format "%s select-window -t editor" base-cmd))))

(defun +tmux/setup-systems-workflow (session project-dir)
  "Setup systems development workflow."
  (let ((base-cmd (format "tmux -t %s" (shell-quote-argument session))))
    ;; Editor
    (shell-command (format "%s rename-window 'editor'" base-cmd))
    
    ;; Build/compile
    (shell-command (format "%s new-window -n 'build'" base-cmd))
    
    ;; Testing
    (shell-command (format "%s new-window -n 'tests'" base-cmd))
    
    ;; System monitoring
    (shell-command (format "%s new-window -n 'monitor'" base-cmd))
    (shell-command (format "%s send-keys -t monitor 'htop' Enter" base-cmd))
    
    ;; Shell
    (shell-command (format "%s new-window -n 'shell'" base-cmd))
    
    (shell-command (format "%s select-window -t editor" base-cmd))))

(defun +tmux/setup-documentation-workflow (session project-dir)
  "Setup documentation workflow."
  (let ((base-cmd (format "tmux -t %s" (shell-quote-argument session))))
    ;; Editor
    (shell-command (format "%s rename-window 'editor'" base-cmd))
    
    ;; Local server
    (shell-command (format "%s new-window -n 'server'" base-cmd))
    (shell-command (format "%s send-keys -t server 'bundle exec jekyll serve --drafts' Enter" base-cmd))
    
    ;; Git
    (shell-command (format "%s new-window -n 'git'" base-cmd))
    
    (shell-command (format "%s select-window -t editor" base-cmd))))

(defun +tmux/setup-generic-workflow (session project-dir)
  "Setup generic development workflow."
  (let ((base-cmd (format "tmux -t %s" (shell-quote-argument session))))
    (shell-command (format "%s rename-window 'editor'" base-cmd))
    (shell-command (format "%s new-window -n 'build'" base-cmd))
    (shell-command (format "%s new-window -n 'shell'" base-cmd))
    (shell-command (format "%s select-window -t editor" base-cmd))))

;; ----------------------------
;; Session Information and Status
;; ----------------------------
(defun +tmux/session-info ()
  "Show tmux session information."
  (interactive)
  (let ((sessions (+tmux/get-sessions)))
    (with-current-buffer (get-buffer-create "*Tmux Sessions*")
      (erase-buffer)
      (insert "# Tmux Sessions\n\n")
      (insert (format "**Date:** %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "**User:** %s\n" (user-login-name)))
      (insert (format "**Total Sessions:** %d\n\n" (length sessions)))
      
      (if sessions
          (progn
            (insert "## Active Sessions\n\n")
            (dolist (session sessions)
              (let ((windows (+tmux/get-windows session))
                    (session-info (+tmux/get-session-info session)))
                (insert (format "### %s\n" session))
                (insert (format "- **Windows:** %d\n" (length windows)))
                (insert (format "- **Info:** %s\n" session-info))
                (when windows
                  (insert "- **Window List:**\n")
                  (dolist (window windows)
                    (insert (format "  - %s\n" window))))
                (insert "\n"))))
        (insert "## No Active Sessions\n\n"))
      
      (insert "## Quick Actions\n\n")
      (insert "- `SPC T n` - Create new session\n")
      (insert "- `SPC T s` - Switch session\n")
      (insert "- `SPC T p` - Create project session\n")
      (insert "- `SPC T w` - Start development workflow\n")
      
      (markdown-mode)
      (display-buffer (current-buffer)))))

(defun +tmux/get-session-info (session)
  "Get detailed information about tmux session."
  (let ((command (format "tmux display-message -t %s -p '#{session_created} #{session_attached}'" 
                        (shell-quote-argument session))))
    (string-trim (shell-command-to-string command))))

;; ----------------------------
;; Quick Commands and Utilities
;; ----------------------------
(defun +tmux/quick-command ()
  "Send quick command to current project session."
  (interactive)
  (if-let ((session (+tmux/current-project-session)))
      (let ((command (read-string "Quick command: ")))
        (when command
          (let ((tmux-cmd (format "tmux send-keys -t %s '%s' Enter"
                                 (shell-quote-argument session)
                                 command)))
            (shell-command tmux-cmd)
            (message "📤 Sent to %s: %s" session command))))
    (message "No project session found")))

(defun +tmux/attach-to-session ()
  "Attach to tmux session (for use in terminal)."
  (interactive)
  (let* ((sessions (+tmux/get-sessions))
         (session (completing-read "Attach to session: " sessions)))
    (when session
      (let ((command (format "tmux attach-session -t %s" (shell-quote-argument session))))
        (if (display-graphic-p)
            (progn
              (kill-new command)
              (message "📋 Command copied to clipboard: %s" command))
          (shell-command command))))))

(defun +tmux/list-all-panes ()
  "List all tmux panes across sessions."
  (interactive)
  (let ((output (shell-command-to-string "tmux list-panes -a -F '#{session_name}:#{window_name}.#{pane_index} #{pane_current_command}'")))
    (with-current-buffer (get-buffer-create "*Tmux Panes*")
      (erase-buffer)
      (insert "# All Tmux Panes\n\n")
      (insert output)
      (display-buffer (current-buffer)))))

;; ----------------------------
;; Project Integration Hooks
;; ----------------------------
(defun +tmux/setup-project-tmux ()
  "Setup tmux environment when switching projects."
  (when-let ((session-name (+tmux/current-project-session)))
    (run-with-timer 1 nil
                    (lambda ()
                      (message "🖥️ Tmux session available: %s | Use SPC T p to create" session-name)))))

(when (featurep 'projectile)
  (add-hook 'projectile-after-switch-project-hook #'+tmux/setup-project-tmux))

;; ----------------------------
;; Productivity Enhancements
;; ----------------------------
(defun +tmux/save-session-layout ()
  "Save current tmux session layout."
  (interactive)
  (let* ((sessions (+tmux/get-sessions))
         (session (completing-read "Save layout for session: " sessions))
         (layout-file (expand-file-name (format ".tmux-layout-%s" session) "~")))
    (when session
      (let ((command (format "tmux list-windows -t %s -F '#{window_index}:#{window_name}:#{window_layout}'" 
                            (shell-quote-argument session))))
        (with-temp-file layout-file
          (insert (shell-command-to-string command)))
        (message "💾 Saved layout for session %s to %s" session layout-file)))))

(defun +tmux/restore-session-layout ()
  "Restore saved tmux session layout."
  (interactive)
  (let* ((layout-files (directory-files "~" nil "^\\.tmux-layout-"))
         (sessions (mapcar (lambda (f) (replace-regexp-in-string "^\\.tmux-layout-" "" f)) layout-files))
         (session (completing-read "Restore layout for session: " sessions)))
    (when session
      (let ((layout-file (expand-file-name (format ".tmux-layout-%s" session) "~")))
        (when (file-exists-p layout-file)
          (message "🔄 Restoring layout for session %s" session)
          ;; Implementation would require parsing and applying layout
          )))))

(provide 'tools-tmux-config)

;;; tools-tmux-config.el ends here
