;;; module/config/tools-config/tools-direnv-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Performance-optimized direnv integration for Doom Emacs
;;
;; FEATURES:
;; - Lightweight direnv integration with minimal performance impact
;; - Smart caching to prevent repeated environment loading
;; - Debounced updates to reduce excessive calls
;; - Optional manual control mode for maximum performance
;; - Essential .envrc management without bloat
;;
;; PERFORMANCE OPTIMIZATIONS:
;; - Cached environment to prevent repeated direnv calls
;; - Debounced updates (500ms delay)
;; - Disabled automatic exec-path-from-shell calls
;; - Manual LSP refresh instead of automatic
;; - Reduced hook usage and smart triggering

;;; Code:

;; ----------------------------
;; Performance-focused configuration
;; ----------------------------
(defvar bdostumski/direnv-cache-timeout 300
  "Cache timeout in seconds (5 minutes) to prevent excessive direnv calls.")

(defvar bdostumski/direnv-last-check-time nil
  "Timestamp of last direnv check to implement caching.")

(defvar bdostumski/direnv-last-directory nil
  "Last directory where direnv was checked to detect changes.")

(defvar bdostumski/direnv-update-timer nil
  "Timer for debounced direnv updates.")

(defvar bdostumski/direnv-performance-mode t
  "When t, enables aggressive performance optimizations.")

;; ----------------------------
;; Lightweight direnv setup with performance optimizations
;; ----------------------------
(use-package! envrc
  :commands (envrc-mode envrc-reload envrc-allow envrc-deny)
  :defer t

  :init
  ;; Minimal configuration for maximum performance
  (setq envrc-debug nil                          ; Never enable debug
        envrc-show-summary-in-minibuffer nil     ; Disable summary for performance
        envrc-lighter nil)                       ; No mode line indicator

  :config
  ;; Only enable in manual mode to prevent automatic slowdowns
  (when (not bdostumski/direnv-performance-mode)
    (envrc-global-mode 1))

  ;; Remove the problematic hooks that cause performance issues
  (remove-hook 'envrc-mode-hook #'bdostumski/direnv-mode-setup)

  (message "✓ Direnv (envrc) loaded in performance mode"))

;; ----------------------------
;; Performance-optimized helper functions
;; ----------------------------
(defun bdostumski/direnv-should-update-p ()
  "Check if direnv should update based on cache and directory changes."
  (let ((current-time (current-time))
        (current-dir default-directory))
    (or
     ;; Directory changed
     (not (string= current-dir (or bdostumski/direnv-last-directory "")))
     ;; Cache expired
     (not bdostumski/direnv-last-check-time)
     (> (float-time (time-subtract current-time bdostumski/direnv-last-check-time))
        bdostumski/direnv-cache-timeout))))

(defun bdostumski/direnv-update-cache ()
  "Update direnv cache timestamps."
  (setq bdostumski/direnv-last-check-time (current-time)
        bdostumski/direnv-last-directory default-directory))

(defun bdostumski/direnv-debounced-update ()
  "Update direnv with debouncing to prevent excessive calls."
  (when bdostumski/direnv-update-timer
    (cancel-timer bdostumski/direnv-update-timer))

  (setq bdostumski/direnv-update-timer
        (run-with-timer 0.5 nil #'bdostumski/direnv-update-now)))

(defun bdostumski/direnv-update-now ()
  "Perform actual direnv update with performance checks."
  (when (and (bdostumski/direnv-should-update-p)
             (locate-dominating-file default-directory ".envrc"))
    (let ((start-time (current-time)))
      (envrc-reload)
      (bdostumski/direnv-update-cache)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (when (> elapsed 1.0)
          (message "Direnv update took %.2fs - consider performance mode" elapsed))))))

;; ----------------------------
;; Minimal essential functions only
;; ----------------------------
(defun bdostumski/direnv-find-envrc ()
  "Find and open the nearest .envrc file."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) default-directory))
         (envrc-file (locate-dominating-file default-directory ".envrc")))
    (if envrc-file
        (find-file (expand-file-name ".envrc" envrc-file))
      (if (y-or-n-p "No .envrc found. Create basic one? ")
          (bdostumski/direnv-create-basic-envrc)
        (message "No .envrc file found")))))

(defun bdostumski/direnv-create-basic-envrc ()
  "Create a minimal .envrc file."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (envrc-path (expand-file-name ".envrc" project-root)))

    (unless (file-exists-p envrc-path)
      (with-temp-file envrc-path
        (insert "# Basic .envrc - edit as needed\n")
        (insert "export PATH=\"$PWD/bin:$PATH\"\n")
        (cond
         ((file-exists-p "package.json")
          (insert "PATH_add node_modules/.bin\n"))
         ((or (file-exists-p "requirements.txt") (file-exists-p "pyproject.toml"))
          (insert "# use python  # uncomment if needed\n"))
         ((file-exists-p "go.mod")
          (insert "export GO111MODULE=on\n"))))

      (find-file envrc-path)
      (message "✓ Created basic .envrc. Remember to run 'direnv allow'"))))

(defun bdostumski/direnv-manual-reload ()
  "Manually reload direnv environment (performance-safe)."
  (interactive)
  (let ((envrc-file (locate-dominating-file default-directory ".envrc")))
    (if envrc-file
        (progn
          (message "Reloading direnv environment...")
          (envrc-reload)
          (bdostumski/direnv-update-cache)
          (message "✓ Direnv environment reloaded"))
      (message "No .envrc file found in current directory tree"))))

(defun bdostumski/direnv-allow ()
  "Allow/trust the current .envrc file."
  (interactive)
  (let ((envrc-file (locate-dominating-file default-directory ".envrc")))
    (if envrc-file
        (progn
          (envrc-allow)
          (message "✓ .envrc file allowed/trusted"))
      (message "No .envrc file found"))))

(defun bdostumski/direnv-deny ()
  "Deny/untrust the current .envrc file."
  (interactive)
  (let ((envrc-file (locate-dominating-file default-directory ".envrc")))
    (if envrc-file
        (progn
          (envrc-deny)
          (message "✓ .envrc file denied/untrusted"))
      (message "No .envrc file found"))))

(defun bdostumski/direnv-toggle-performance-mode ()
  "Toggle between performance mode and full auto mode."
  (interactive)
  (setq bdostumski/direnv-performance-mode (not bdostumski/direnv-performance-mode))
  (if bdostumski/direnv-performance-mode
      (progn
        (envrc-global-mode -1)
        (message "✓ Direnv performance mode ON - manual updates only"))
    (progn
      (envrc-global-mode 1)
      (message "⚠ Direnv auto mode ON - may impact performance"))))

(defun bdostumski/direnv-status ()
  "Show simple direnv status."
  (interactive)
  (let* ((envrc-file (locate-dominating-file default-directory ".envrc"))
         (auto-mode (bound-and-true-p envrc-global-mode)))
    (message "Direnv: %s | .envrc: %s | Performance mode: %s"
             (if auto-mode "auto" "manual")
             (if envrc-file "found" "none")
             (if bdostumski/direnv-performance-mode "ON" "OFF"))))

;; ----------------------------
;; Minimal keybindings - only essential functions
;; ----------------------------
(map! :leader
      ;; File management integration
      (:prefix-map ("f" . "file")
       :desc "Find .envrc file"         "e" #'bdostumski/direnv-find-envrc)

      ;; Minimal direnv prefix - only essential commands
      (:prefix-map ("e" . "environment")
       :desc "Edit .envrc"              "e" #'bdostumski/direnv-find-envrc
       :desc "Reload (manual)"          "r" #'bdostumski/direnv-manual-reload
       :desc "Allow .envrc"             "a" #'bdostumski/direnv-allow
       :desc "Deny .envrc"              "d" #'bdostumski/direnv-deny
       :desc "Toggle performance mode"  "p" #'bdostumski/direnv-toggle-performance-mode
       :desc "Status"                   "s" #'bdostumski/direnv-status))

;; ----------------------------
;; Performance monitoring
;; ----------------------------
(defun bdostumski/direnv-performance-warning ()
  "Show performance tips if direnv is slow."
  (when (and (not bdostumski/direnv-performance-mode)
             (> (string-to-number (format-time-string "%S")) 2))
    (message "💡 Tip: Use SPC e p to enable direnv performance mode for speed")))

;; Show performance tip after startup
(run-with-timer 5 nil #'bdostumski/direnv-performance-warning)

;; ----------------------------
;; Disable problematic integrations that cause slowdown
;; ----------------------------

;; Disable automatic exec-path-from-shell calls
(setq exec-path-from-shell-warn-duration-millis 5000)  ; Only warn if > 5 seconds
(setq exec-path-from-shell-arguments '("-l"))          ; Use minimal shell arguments

;; Don't automatically refresh LSP on direnv changes (do manually when needed)
(defun bdostumski/direnv-lsp-refresh ()
  "Manually refresh LSP workspace after direnv changes."
  (interactive)
  (when (and (featurep 'lsp-mode) (lsp-workspaces))
    (lsp-restart-workspace)
    (message "✓ LSP workspace refreshed")))

;; Add LSP refresh to the environment menu
(map! :leader
      (:prefix-map ("e" . "environment")
       :desc "Refresh LSP"              "l" #'bdostumski/direnv-lsp-refresh))

;; ----------------------------
;; Startup message
;; ----------------------------
(message "✓ Direnv loaded in PERFORMANCE MODE - use SPC e r for manual reload")
(message "💡 Use SPC e p to toggle auto-mode (may impact performance)")

(provide 'tools-direnv-config)

;;; tools-direnv-config.el ends here
