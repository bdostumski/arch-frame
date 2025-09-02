;;; module/config/tools-config/tools-editorconfig-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive EditorConfig integration for Doom Emacs
;;
;; FEATURES:
;; - Automatic code style application from .editorconfig files
;; - Template system for common .editorconfig setups
;; - Project-wide consistency enforcement
;; - Visual feedback and debugging tools
;; - Integration with project management
;;
;; KEYBINDINGS:
;; Leader Key Bindings (SPC):
;;   SPC e c     - Show current EditorConfig settings
;;   SPC f .     - Find/edit .editorconfig file
;;   
;; EditorConfig-specific (SPC E):
;;   SPC E e     - Edit .editorconfig file
;;   SPC E c     - Show current settings
;;   SPC E r     - Reload EditorConfig for buffer
;;   SPC E t     - Create .editorconfig from template

;;; Code:

;; ----------------------------
;; State tracking and configuration
;; ----------------------------
(defvar bdostumski/editorconfig-initialized nil
  "Track if EditorConfig has been properly initialized.")

(defvar bdostumski/editorconfig-debug-mode nil
  "When t, enables verbose EditorConfig debugging.")

(defvar bdostumski/editorconfig-auto-apply t
  "When t, automatically applies EditorConfig settings to new buffers.")

(defvar bdostumski/editorconfig-templates
  '(("web-frontend" . 
     "# EditorConfig for Web Frontend Projects\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\n\n[*.{js,ts,jsx,tsx,vue,svelte}]\nindent_style = space\nindent_size = 2\nmax_line_length = 100\n\n[*.{html,css,scss,sass,less}]\nindent_style = space\nindent_size = 2\n\n[*.{json,yaml,yml}]\nindent_style = space\nindent_size = 2\n\n[*.md]\ntrim_trailing_whitespace = false\nmax_line_length = 120\n")
    
    ("backend-api" .
     "# EditorConfig for Backend API Projects\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\n\n[*.{py,rb,php}]\nindent_style = space\nindent_size = 4\nmax_line_length = 88\n\n[*.{js,ts}]\nindent_style = space\nindent_size = 2\nmax_line_length = 100\n\n[*.{java,kt,scala}]\nindent_style = space\nindent_size = 4\nmax_line_length = 120\n\n[*.{go}]\nindent_style = tab\ntab_width = 4\nmax_line_length = 100\n")
    
    ("python-data" .
     "# EditorConfig for Python/Data Science Projects\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\n\n[*.py]\nindent_style = space\nindent_size = 4\nmax_line_length = 88\n\n[*.{yml,yaml}]\nindent_style = space\nindent_size = 2\n\n[requirements*.txt]\ninsert_final_newline = true\n")
    
    ("minimal" .
     "# Minimal EditorConfig\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\nindent_style = space\nindent_size = 2\n"))
  "Templates for common EditorConfig configurations.")

(defvar bdostumski/editorconfig-current-settings nil
  "Cache of current EditorConfig settings for quick reference.")

;; ----------------------------
;; Helper functions (defined before use)
;; ----------------------------
(defun bdostumski/editorconfig-get-properties-safe ()
  "Safely get EditorConfig properties for current buffer."
  (when (buffer-file-name)
    (condition-case err
        (editorconfig-get-properties (buffer-file-name))
      (error 
       (when bdostumski/editorconfig-debug-mode
         (message "EditorConfig error: %s" (error-message-string err)))
       nil))))

(defun bdostumski/editorconfig-maybe-enable ()
  "Conditionally enable EditorConfig for appropriate buffers."
  (when (and bdostumski/editorconfig-auto-apply
             (buffer-file-name)
             (not (file-remote-p (buffer-file-name)))
             (not (string-match-p "\\*.*\\*" (buffer-name))))
    ;; Cache settings for quick access
    (setq bdostumski/editorconfig-current-settings 
          (bdostumski/editorconfig-get-properties-safe))))

(defun bdostumski/editorconfig-after-apply (props)
  "Hook function called after EditorConfig properties are applied."
  (when bdostumski/editorconfig-debug-mode
    (message "EditorConfig applied: %s properties" 
             (if (hash-table-p props) (hash-table-count props) "unknown")))
  
  ;; Update cached settings
  (setq bdostumski/editorconfig-current-settings props))

(defun bdostumski/editorconfig-setup-custom-properties ()
  "Setup custom EditorConfig property handlers for enhanced functionality."
  ;; Max line length integration with fill-column
  (when (boundp 'editorconfig-indentation-alist)
    (add-to-list 'editorconfig-indentation-alist
                 '(fill-column . max_line_length))))

;; ----------------------------
;; Enhanced EditorConfig package configuration
;; ----------------------------
(use-package! editorconfig
  :diminish editorconfig-mode
  :hook ((prog-mode text-mode) . bdostumski/editorconfig-maybe-enable)
  
  :init
  ;; Pre-configuration settings
  (when (boundp 'editorconfig-trim-whitespaces-mode)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))  ; Use ws-butler for whitespace
  
  :config
  ;; Enable EditorConfig globally
  (editorconfig-mode 1)
  
  ;; Core EditorConfig settings
  (when (boundp 'editorconfig-exclude-modes)
    (setq editorconfig-exclude-modes '(emacs-lisp-mode       ; Modes to exclude
                                       lisp-mode
                                       scheme-mode)))

  (when (boundp 'editorconfig-exclude-regexps)
    (setq editorconfig-exclude-regexps '("\\.log$"           ; Files to exclude
                                         "\\.tmp$"
                                         "/node_modules/"
                                         "/vendor/"
                                         "\\.min\\.")))

  ;; Set up hooks for enhanced integration
  (add-hook 'editorconfig-after-apply-functions #'bdostumski/editorconfig-after-apply)
  
  ;; Custom property extensions
  (bdostumski/editorconfig-setup-custom-properties)
  
  (setq bdostumski/editorconfig-initialized t)
  (message "✓ EditorConfig integration enabled"))

;; ----------------------------
;; File management functions
;; ----------------------------
(defun bdostumski/editorconfig-find-file ()
  "Find and open the nearest .editorconfig file."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (config-file (locate-dominating-file project-root ".editorconfig")))
    (if config-file
        (find-file (expand-file-name ".editorconfig" config-file))
      (if (y-or-n-p "No .editorconfig found. Create one? ")
          (bdostumski/editorconfig-create-file)
        (message "No .editorconfig file found")))))

(defun bdostumski/editorconfig-create-file ()
  "Create a new .editorconfig file with template selection."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (template-name (completing-read "EditorConfig template: " 
                                         (mapcar #'car bdostumski/editorconfig-templates)
                                         nil t "minimal"))
         (template-content (cdr (assoc template-name bdostumski/editorconfig-templates)))
         (config-path (expand-file-name ".editorconfig" project-root)))
    
    (if (file-exists-p config-path)
        (if (y-or-n-p ".editorconfig already exists. Overwrite? ")
            (bdostumski/editorconfig-write-file config-path template-content)
          (message "Cancelled"))
      (bdostumski/editorconfig-write-file config-path template-content))
    
    (find-file config-path)))

(defun bdostumski/editorconfig-write-file (path content)
  "Write .editorconfig file with given content."
  (with-temp-file path
    (insert content)
    (insert (format "\n# Generated by %s on %s\n" 
                    user-login-name 
                    (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (message "✓ Created .editorconfig: %s" (abbreviate-file-name path)))

;; ----------------------------
;; Settings and debugging functions
;; ----------------------------
(defun bdostumski/editorconfig-show-current-settings ()
  "Show current EditorConfig settings for the buffer."
  (interactive)
  (let ((props (or bdostumski/editorconfig-current-settings
                   (bdostumski/editorconfig-get-properties-safe))))
    (if (and props (hash-table-p props) (> (hash-table-count props) 0))
        (with-current-buffer (get-buffer-create "*EditorConfig Settings*")
          (erase-buffer)
          (insert (format "EditorConfig Settings for: %s\n" 
                          (or (buffer-file-name) (buffer-name))))
          (insert "=========================================\n\n")
          
          ;; Show active settings
          (insert "Active Settings:\n")
          (insert "----------------\n")
          (maphash (lambda (key value)
                     (insert (format "%-20s = %s\n" key value)))
                   props)
          
          ;; Show derived Emacs variables
          (insert "\nDerived Emacs Variables:\n")
          (insert "------------------------\n")
          (insert (format "indent-tabs-mode     = %s\n" indent-tabs-mode))
          (insert (format "tab-width           = %s\n" tab-width))
          (insert (format "fill-column         = %s\n" fill-column))
          (insert (format "require-final-newline = %s\n" require-final-newline))
          
          (display-buffer (current-buffer)))
      (message "No EditorConfig settings found for this buffer"))))

(defun bdostumski/editorconfig-reload-buffer ()
  "Reload EditorConfig settings for the current buffer."
  (interactive)
  (when (buffer-file-name)
    (condition-case err
        (progn
          (editorconfig-apply)
          (setq bdostumski/editorconfig-current-settings 
                (bdostumski/editorconfig-get-properties-safe))
          (message "✓ EditorConfig settings reloaded"))
      (error
       (message "Error reloading EditorConfig: %s" (error-message-string err))))))

(defun bdostumski/editorconfig-apply-to-project ()
  "Apply EditorConfig settings to all open project buffers."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (project-buffers (if project-root
                              (projectile-project-buffers)
                            (buffer-list)))
         (count 0))
    
    (dolist (buffer project-buffers)
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (string-prefix-p (or project-root default-directory) 
                                    (buffer-file-name)))
          (condition-case err
              (progn
                (editorconfig-apply)
                (setq count (1+ count)))
            (error
             (when bdostumski/editorconfig-debug-mode
               (message "Error applying EditorConfig to %s: %s" 
                        (buffer-name) (error-message-string err))))))))
    
    (message "✓ Applied EditorConfig to %d buffer(s)" count)))

(defun bdostumski/editorconfig-validate-file ()
  "Validate .editorconfig file syntax."
  (interactive)
  (let ((config-file (locate-dominating-file default-directory ".editorconfig")))
    (if config-file
        (let ((config-path (expand-file-name ".editorconfig" config-file)))
          ;; Basic syntax validation
          (with-temp-buffer
            (insert-file-contents config-path)
            (let ((issues '())
                  (line-num 1))
              (goto-char (point-min))
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties 
                             (line-beginning-position) (line-end-position))))
                  ;; Check for common syntax issues
                  (cond
                   ((and (string-match "^\\[.*\\]$" line)
                         (string-match "\\[.*[^]]+$" line))
                    (push (format "Line %d: Unclosed section bracket" line-num) issues))
                   ((and (string-match "=" line)
                         (not (string-match "^[[:space:]]*[^=]+=[^=]*$" line)))
                    (push (format "Line %d: Invalid property format" line-num) issues)))
                  (forward-line 1)
                  (setq line-num (1+ line-num))))
              
              ;; Report results
              (if issues
                  (with-current-buffer (get-buffer-create "*EditorConfig Validation*")
                    (erase-buffer)
                    (insert "EditorConfig Validation Issues\n")
                    (insert "===============================\n\n")
                    (dolist (issue (reverse issues))
                      (insert "⚠ " issue "\n"))
                    (display-buffer (current-buffer)))
                (message "✓ .editorconfig file syntax is valid")))))
      (message "No .editorconfig file found"))))

(defun bdostumski/editorconfig-show-debug-info ()
  "Show detailed EditorConfig debug information."
  (interactive)
  (with-current-buffer (get-buffer-create "*EditorConfig Debug*")
    (erase-buffer)
    (insert "EditorConfig Debug Information\n")
    (insert "==============================\n\n")
    
    ;; System information
    (insert "System Info:\n")
    (insert "------------\n")
    (insert (format "EditorConfig mode: %s\n" 
                    (if (bound-and-true-p editorconfig-mode) "enabled" "disabled")))
    (insert (format "Auto-apply: %s\n" 
                    (if bdostumski/editorconfig-auto-apply "enabled" "disabled")))
    (insert (format "Debug mode: %s\n" 
                    (if bdostumski/editorconfig-debug-mode "enabled" "disabled")))
    
    ;; File information
    (insert "\nCurrent Buffer:\n")
    (insert "---------------\n")
    (insert (format "File: %s\n" (or (buffer-file-name) "No file")))
    (insert (format "Mode: %s\n" major-mode))
    
    ;; Configuration file location
    (let ((config-file (locate-dominating-file default-directory ".editorconfig")))
      (insert (format "Config file: %s\n" 
                      (if config-file 
                          (expand-file-name ".editorconfig" config-file)
                        "Not found"))))

    ;; Current settings
    (when (and bdostumski/editorconfig-current-settings
               (hash-table-p bdostumski/editorconfig-current-settings))
      (insert "\nCurrent Settings:\n")
      (insert "-----------------\n")
      (maphash (lambda (key value)
                 (insert (format "%s = %s\n" key value)))
               bdostumski/editorconfig-current-settings))
    
    (display-buffer (current-buffer))))

;; ----------------------------
;; Toggle and utility functions
;; ----------------------------
(defun bdostumski/editorconfig-toggle-debug-mode ()
  "Toggle EditorConfig debug mode."
  (interactive)
  (setq bdostumski/editorconfig-debug-mode (not bdostumski/editorconfig-debug-mode))
  (message "EditorConfig debug mode: %s" 
           (if bdostumski/editorconfig-debug-mode "enabled" "disabled")))

(defun bdostumski/editorconfig-toggle-auto-apply ()
  "Toggle automatic EditorConfig application."
  (interactive)
  (setq bdostumski/editorconfig-auto-apply (not bdostumski/editorconfig-auto-apply))
  (message "EditorConfig auto-apply: %s" 
           (if bdostumski/editorconfig-auto-apply "enabled" "disabled")))

;; ----------------------------
;; Comprehensive keybinding setup
;; ----------------------------
(map! :leader
      ;; Quick access (existing binding enhanced)
      :desc "Show EditorConfig settings" "e c" #'bdostumski/editorconfig-show-current-settings
      
      ;; File management integration
      (:prefix-map ("f" . "file")
       :desc "Find .editorconfig"       "." #'bdostumski/editorconfig-find-file)
      
      ;; Main EditorConfig prefix
      (:prefix-map ("E" . "EditorConfig")
       ;; File operations
       :desc "Edit .editorconfig"       "e" #'bdostumski/editorconfig-find-file
       :desc "Create from template"     "t" #'bdostumski/editorconfig-create-file
       :desc "Validate syntax"          "v" #'bdostumski/editorconfig-validate-file
       
       ;; Settings and application
       :desc "Show current settings"    "c" #'bdostumski/editorconfig-show-current-settings
       :desc "Reload buffer settings"   "r" #'bdostumski/editorconfig-reload-buffer
       :desc "Apply to project"         "a" #'bdostumski/editorconfig-apply-to-project
       
       ;; Information and debugging
       :desc "Debug information"        "d" #'bdostumski/editorconfig-show-debug-info
       :desc "Toggle debug mode"        "D" #'bdostumski/editorconfig-toggle-debug-mode
       
       ;; Configuration
       :desc "Toggle auto-apply"        "A" #'bdostumski/editorconfig-toggle-auto-apply
       
       ;; Help and documentation
       :desc "Help"                     "h" (lambda () (interactive)
                                              (browse-url "https://editorconfig.org/"))))

;; ----------------------------
;; Integration with other packages
;; ----------------------------
(after! projectile
  ;; Add .editorconfig to project root indicators
  (add-to-list 'projectile-project-root-files ".editorconfig"))

(after! which-key
  (which-key-add-key-based-replacements
    "SPC E" "EditorConfig"))

;; Integration with ws-butler for better whitespace handling
(after! ws-butler
  (when (boundp 'ws-butler-keep-whitespace-indent-mode)
    (setq ws-butler-keep-whitespace-indent-mode t)))

(provide 'tools-editorconfig-config)

;;; tools-editorconfig-config.el ends here
