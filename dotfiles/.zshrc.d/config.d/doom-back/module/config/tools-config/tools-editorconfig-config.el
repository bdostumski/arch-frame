;;; module/config/tools-config/tools-editorconfig-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; EditorConfig helps maintain consistent coding styles for multiple developers working on the same project across various editors and IDEs.
;; The EditorConfig project consists of a file format for defining coding styles and a collection of text editor plugins that enable editors to read the file format and adhere to defined styles.
;; EditorConfig files are easily readable and they work nicely with version control systems.

;;; Code:

;; Core deps
(require 'cl-lib)
(when (not (fboundp 'string-trim))
  (require 'subr-x)) ;; string-trim, string-empty-p for older Emacs

(defgroup +editorconfig/editorconfig nil
  "EditorConfig helpers."
  :group 'convenience)

;; ----------------------------
;; State tracking and configuration
;; ----------------------------
(defvar +editorconfig/editorconfig-initialized nil
  "Non-nil when EditorConfig integration has been initialized.")

(defvar +editorconfig/editorconfig-debug-mode nil
  "When non-nil, prints verbose EditorConfig debug messages.")

(defvar +editorconfig/editorconfig-auto-apply t
  "When non-nil, automatically applies EditorConfig settings to suitable buffers.")

(defvar +editorconfig/editorconfig-templates
  '(("web-frontend" .
     "# EditorConfig for Web Frontend Projects\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\n\n[*.{js,ts,jsx,tsx,vue,svelte}]\nindent_style = space\nindent_size = 2\nmax_line_length = 100\n\n[*.{html,css,scss,sass,less}]\nindent_style = space\nindent_size = 2\n\n[*.{json,yaml,yml}]\nindent_style = space\nindent_size = 2\n\n[*.md]\ntrim_trailing_whitespace = false\nmax_line_length = 120\n")

    ("backend-api" .
     "# EditorConfig for Backend API Projects\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\n\n[*.{py,rb,php}]\nindent_style = space\nindent_size = 4\nmax_line_length = 88\n\n[*.{js,ts}]\nindent_style = space\nindent_size = 2\nmax_line_length = 100\n\n[*.{java,kt,scala}]\nindent_style = space\nindent_size = 4\nmax_line_length = 120\n\n[*.go]\nindent_style = tab\ntab_width = 4\nmax_line_length = 100\n")

    ("python-data" .
     "# EditorConfig for Python/Data Science Projects\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\n\n[*.py]\nindent_style = space\nindent_size = 4\nmax_line_length = 88\n\n[*.{yml,yaml}]\nindent_style = space\nindent_size = 2\n\n[requirements*.txt]\ninsert_final_newline = true\n")

    ("minimal" .
     "# Minimal EditorConfig\nroot = true\n\n[*]\ncharset = utf-8\ninsert_final_newline = true\ntrim_trailing_whitespace = true\nend_of_line = lf\nindent_style = space\nindent_size = 2\n"))
  "Templates for common EditorConfig configurations.")

(defvar +editorconfig/editorconfig-current-settings nil
  "Cache (hash table) of last-read EditorConfig properties for the current buffer.")

;; ----------------------------
;; Small helpers
;; ----------------------------
(defun +editorconfig/editorconfig--msg (fmt &rest args)
  "Log an editorconfig message if debug mode or always for important messages.
FMT and ARGS like `message'."
  (when (or +editorconfig/editorconfig-debug-mode t)
    (apply #'message (concat "[editorconfig] " fmt) args)))

;; Safe wrappers around editorconfig functions (only call if available)
(defun +editorconfig/editorconfig-get-properties-safe ()
  "Return EditorConfig properties for current buffer or nil.
Wraps `editorconfig-get-properties' safely (checks for function)."
  (when (and (buffer-file-name) (fboundp 'editorconfig-get-properties))
    (condition-case err
        (editorconfig-get-properties (buffer-file-name))
      (error
       (when +editorconfig/editorconfig-debug-mode
         (message "[editorconfig] error in get-properties: %s" (error-message-string err)))
       nil))))

(defun +editorconfig/editorconfig-apply-safe ()
  "Call `editorconfig-apply' if available, return t on success, nil otherwise."
  (when (fboundp 'editorconfig-apply)
    (condition-case err
        (progn (editorconfig-apply) t)
      (error
       (when +editorconfig/editorconfig-debug-mode
         (message "[editorconfig] error in apply: %s" (error-message-string err)))
       nil))))

;; ----------------------------
;; Syncing variables
;; ----------------------------
(defun +editorconfig/editorconfig-sync-vars (props)
  "Sync Emacs buffer-local vars with EditorConfig PROPS (hash table)."
  (when (hash-table-p props)
    (let ((indent (gethash 'indent_size props))
          (style  (gethash 'indent_style props))
          (len    (gethash 'max_line_length props)))
      (when indent
        (setq-local tab-width (if (stringp indent) (string-to-number indent) indent)))
      (when style
        (setq-local indent-tabs-mode (string= (if (stringp style) style (format "%s" style)) "tab")))
      (when len
        (setq-local fill-column (if (stringp len) (string-to-number len) len))))))

(defun +editorconfig/editorconfig-after-apply (props)
  "Hook: called after editorconfig properties are applied with PROPS."
  (when +editorconfig/editorconfig-debug-mode
    (message "[editorconfig] after-apply: %s properties"
             (if (hash-table-p props) (hash-table-count props) "unknown")))
  (setq +editorconfig/editorconfig-current-settings props)
  (+editorconfig/editorconfig-sync-vars props))

(defun +editorconfig/editorconfig-maybe-enable ()
  "Auto-load .editorconfig settings for buffers when appropriate."
  (when (and +editorconfig/editorconfig-auto-apply
             (buffer-file-name)
             (not (file-remote-p (buffer-file-name)))
             (not (string-prefix-p "*" (buffer-name))))
    (setq +editorconfig/editorconfig-current-settings
          (+editorconfig/editorconfig-get-properties-safe))))

(defun +editorconfig/editorconfig-setup-custom-properties ()
  "Register custom EditorConfig property handlers, if supported."
  (when (boundp 'editorconfig-indentation-alist)
    (add-to-list 'editorconfig-indentation-alist '(fill-column . max_line_length))))

;; ----------------------------
;; Use-package for the real package (defensive)
;; ----------------------------
(use-package! editorconfig
  :diminish editorconfig-mode
  :hook (find-file . +editorconfig/editorconfig-maybe-enable)
  :init
  ;; prefer ws-butler if present for trimming behaviour
  (when (boundp 'editorconfig-trim-whitespaces-mode)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))
  :config
  ;; enable mode if possible
  (condition-case err
      (editorconfig-mode 1)
    (error
     (message "[editorconfig] failed to enable editorconfig-mode: %s" (error-message-string err))))

  ;; safe registration of after-apply hook (some editorconfig versions define this)
  (with-eval-after-load 'editorconfig
    (when (boundp 'editorconfig-after-apply-functions)
      (add-hook 'editorconfig-after-apply-functions #'+editorconfig/editorconfig-after-apply)))

  (+editorconfig/editorconfig-setup-custom-properties)
  (setq +editorconfig/editorconfig-initialized t)
  (+editorconfig/editorconfig--msg "EditorConfig integration enabled"))

;; If the package is not present on load, warn the user (but do not fail)
(unless (or (featurep 'editorconfig) (locate-library "editorconfig"))
  (+editorconfig/editorconfig--msg "EditorConfig package not found; install `editorconfig' package for full behaviour."))

;; ----------------------------
;; File management functions (safe)
;; ----------------------------
(defun +editorconfig/editorconfig-find-file ()
  "Find and open the nearest .editorconfig file."
  (interactive)
  (let* ((project-root (or (when (fboundp 'projectile-project-root) (projectile-project-root))
                           default-directory))
         (config-dir (locate-dominating-file project-root ".editorconfig")))
    (if config-dir
        (find-file (expand-file-name ".editorconfig" config-dir))
      (if (y-or-n-p "No .editorconfig found. Create one? ")
          (+editorconfig/editorconfig-create-file)
        (message "No .editorconfig file found")))))

(defun +editorconfig/editorconfig-create-file ()
  "Create a new .editorconfig file from a template."
  (interactive)
  (let* ((project-root (or (when (fboundp 'projectile-project-root) (projectile-project-root))
                           default-directory))
         (template-name (completing-read "EditorConfig template: "
                                         (mapcar #'car +editorconfig/editorconfig-templates)
                                         nil t "minimal"))
         (template-content (cdr (assoc template-name +editorconfig/editorconfig-templates)))
         (config-path (expand-file-name ".editorconfig" project-root)))
    (if (file-exists-p config-path)
        (if (y-or-n-p ".editorconfig exists. Overwrite? ")
            (+editorconfig/editorconfig-write-file config-path template-content)
          (message "Cancelled"))
      (+editorconfig/editorconfig-write-file config-path template-content))
    (find-file config-path)))

(defun +editorconfig/editorconfig-write-file (path content)
  "Write PATH with CONTENT and append a generator footer."
  (with-temp-file path
    (insert content)
    (insert (format "\n# Generated by %s on %s\n"
                    user-login-name
                    (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (message "Created .editorconfig: %s" (abbreviate-file-name path)))

;; ----------------------------
;; Commands & utilities
;; ----------------------------
(defun +editorconfig/editorconfig-show-current-settings ()
  "Show current EditorConfig settings (cached or freshly-read)."
  (interactive)
  (let ((props (or +editorconfig/editorconfig-current-settings
                   (+editorconfig/editorconfig-get-properties-safe))))
    (if (and props (hash-table-p props) (> (hash-table-count props) 0))
        (with-current-buffer (get-buffer-create "*EditorConfig Settings*")
          (erase-buffer)
          (insert (format "EditorConfig Settings for: %s\n"
                          (or (buffer-file-name) (buffer-name))))
          (insert "=========================================\n\n")
          (insert "Active Settings:\n----------------\n")
          (maphash (lambda (k v) (insert (format "%-20s = %s\n" k v))) props)
          (insert "\nDerived Emacs Variables:\n------------------------\n")
          (insert (format "indent-tabs-mode       = %s\n" indent-tabs-mode))
          (insert (format "tab-width              = %s\n" tab-width))
          (insert (format "fill-column            = %s\n" fill-column))
          (insert (format "require-final-newline  = %s\n" require-final-newline))
          (display-buffer (current-buffer)))
      (message "No EditorConfig settings found for this buffer"))))

(defun +editorconfig/editorconfig-reload-buffer ()
  "Reload EditorConfig settings for the current buffer."
  (interactive)
  (if (+editorconfig/editorconfig-apply-safe)
      (progn
        (setq +editorconfig/editorconfig-current-settings (+editorconfig/editorconfig-get-properties-safe))
        (message "EditorConfig settings reloaded"))
    (message "EditorConfig apply not available (is the editorconfig package installed?)")))

(defun +editorconfig/editorconfig-apply-to-project ()
  "Apply EditorConfig to all open buffers in the current projectile project, or all buffers."
  (interactive)
  (let* ((project-root (and (fboundp 'projectile-project-root) (projectile-project-root)))
         (buffers (if project-root (when (fboundp 'projectile-project-buffers) (projectile-project-buffers)) (buffer-list)))
         (count 0))
    (dolist (buf (or buffers (buffer-list)))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (string-prefix-p (or project-root default-directory) (buffer-file-name)))
          (when (+editorconfig/editorconfig-apply-safe)
            (cl-incf count)))))
    (message "Applied EditorConfig to %d buffer(s)" count)))

(defun +editorconfig/editorconfig-validate-file ()
  "Validate nearest .editorconfig file with simple checks or external checker."
  (interactive)
  (let ((config-dir (locate-dominating-file default-directory ".editorconfig")))
    (if config-dir
        (let ((path (expand-file-name ".editorconfig" config-dir)))
          (if (executable-find "editorconfig-checker")
              (compile (format "editorconfig-checker %s" path))
            (with-temp-buffer
              (insert-file-contents path)
              (let ((issues '()) (ln 1))
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                    (when (and (not (string-empty-p line))
                               (string-match-p "\\[.*[^]]+$" line))
                      (push (format "Line %d: Unclosed section bracket" ln) issues))
                    (when (and (string-match "=" line)
                               (not (string-match "^[[:space:]]*[^=]+=[^=]*$" line)))
                      (push (format "Line %d: Invalid property format" ln) issues)))
                  (forward-line 1)
                  (setq ln (1+ ln)))
                (if issues
                    (with-current-buffer (get-buffer-create "*EditorConfig Validation*")
                      (erase-buffer)
                      (insert "EditorConfig Validation Issues\n===============================\n\n")
                      (dolist (i (reverse issues)) (insert "⚠ " i "\n"))
                      (display-buffer (current-buffer)))
                  (message "✓ .editorconfig syntax appears valid"))))))
      (message "No .editorconfig file found"))))

(defun +editorconfig/editorconfig-show-debug-info ()
  "Show detailed debug information about editorconfig integration."
  (interactive)
  (with-current-buffer (get-buffer-create "*EditorConfig Debug*")
    (erase-buffer)
    (insert "EditorConfig Debug Info\n=======================\n\n")
    (insert (format "Buffer file: %s\n" (or (buffer-file-name) "none")))
    (insert (format "Major mode: %s\n" major-mode))
    (insert (format "editorconfig package present: %s\n" (if (or (featurep 'editorconfig) (locate-library "editorconfig")) "yes" "no")))
    (insert (format "editorconfig-mode: %s\n" (if (bound-and-true-p editorconfig-mode) "enabled" "disabled")))
    (insert (format "Auto-apply: %s\n" (if +editorconfig/editorconfig-auto-apply "enabled" "disabled")))
    (insert (format "Debug mode: %s\n" (if +editorconfig/editorconfig-debug-mode "enabled" "disabled")))
    (when +editorconfig/editorconfig-current-settings
      (insert "\nCurrent settings:\n-----------------\n")
      (maphash (lambda (k v) (insert (format "%s = %s\n" k v))) +editorconfig/editorconfig-current-settings)))
  (display-buffer (current-buffer)))

(defun +editorconfig/editorconfig-toggle-debug-mode ()
  "Toggle debug mode for this module."
  (interactive)
  (setq +editorconfig/editorconfig-debug-mode (not +editorconfig/editorconfig-debug-mode))
  (message "EditorConfig debug mode: %s" (if +editorconfig/editorconfig-debug-mode "enabled" "disabled")))

(defun +editorconfig/editorconfig-toggle-auto-apply ()
  "Toggle automatic application of EditorConfig settings."
  (interactive)
  (setq +editorconfig/editorconfig-auto-apply (not +editorconfig/editorconfig-auto-apply))
  (message "EditorConfig auto-apply: %s" (if +editorconfig/editorconfig-auto-apply "enabled" "disabled")))

;; Integrations
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files ".editorconfig"))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC E" "EditorConfig"))

(with-eval-after-load 'ws-butler
  (setq ws-butler-keep-whitespace-indent-mode t))

(provide 'tools-editorconfig-config)
;;; tools-editorconfig-config.el ends here
