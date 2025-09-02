;;; module/config/tools-config/tools-debugger-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive debugging configuration for Doom Emacs using DAP (Debug Adapter Protocol)
;;
;; FEATURES:
;; - Multi-language debugging support (Java, Node.js, Python, C/C++, Go, Rust, etc.)
;; - Enhanced UI with breakpoint management and variable inspection
;; - Session management with templates and configurations
;; - Integration with LSP and project management
;; - Custom debugging workflows and automation
;; - Performance monitoring and profiling integration
;; - Remote debugging capabilities
;; - Test debugging integration
;;
;; SUPPORTED LANGUAGES:
;; - Java (via Eclipse JDT Language Server)
;; - JavaScript/TypeScript/Node.js (via VS Code JS Debug)
;; - Python (via debugpy)
;; - C/C++ (via GDB/LLDB)
;; - Go (via Delve)
;; - Rust (via CodeLLDB)
;; - PHP (via Xdebug)
;; - .NET (via netcoredbg)
;; - Ruby (via rdbg)
;;
;; KEYBINDINGS:
;; Leader Key Bindings (SPC):
;;   SPC d     - Main debug prefix
;;   SPC d d   - Start debugging
;;   SPC d l   - Debug last/recent session
;;   SPC d r   - Restart current session
;;   SPC d q   - Quit debugging session
;;   SPC d k   - Kill all debug sessions
;;
;;   SPC d b   - Breakpoint management
;;   SPC d s   - Step operations (over, into, out)
;;   SPC d e   - Expression evaluation
;;   SPC d v   - Variable inspection
;;   SPC d w   - Watch expressions
;;   SPC d u   - UI toggles and windows
;;   SPC d t   - Test debugging
;;   SPC d c   - Configuration management

;;; Code:

;; ----------------------------
;; State tracking and configuration
;; ----------------------------
(defvar bdostumski/dap-initialized nil
  "Track if DAP has been properly initialized.")

(defvar bdostumski/dap-debug-templates nil
  "List of custom debug templates for quick access.")

(defvar bdostumski/dap-last-session-config nil
  "Configuration of the last debug session for quick restart.")

(defvar bdostumski/dap-ui-layout-saved nil
  "Saved window layout before debugging session.")

(defvar bdostumski/dap-auto-configure-breakpoints t
  "Whether to automatically configure common breakpoints.")

;; ----------------------------
;; Language adapter loading (defined before use-package)
;; ----------------------------
(defun bdostumski/dap-load-language-adapters ()
  "Load debug adapters for supported languages."
  (message "Loading DAP language adapters...")

  ;; Core adapters - using condition-case for each to handle missing adapters gracefully
  (condition-case err
      (require 'dap-java nil t)
    (error (message "Java DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-node nil t)
    (error (message "Node.js DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-python nil t)
    (error (message "Python DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-gdb-lldb nil t)
    (error (message "GDB/LLDB DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-go nil t)
    (error (message "Go DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-netcore nil t)
    (error (message ".NET DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-php nil t)
    (error (message "PHP DAP adapter not available: %s" (error-message-string err))))

  (condition-case err
      (require 'dap-ruby nil t)
    (error (message "Ruby DAP adapter not available: %s" (error-message-string err))))

  (message "✓ DAP language adapters loading completed"))

;; ----------------------------
;; Debug templates and configurations
;; ----------------------------
(defun bdostumski/dap-setup-debug-templates ()
  "Setup custom debug templates for common scenarios."
  (setq bdostumski/dap-debug-templates
        '(;; Java templates
          (:java-main
           :type "java"
           :request "launch"
           :name "Java Main Class"
           :mainClass nil  ; Will prompt
           :projectName nil ; Will auto-detect
           :args ""
           :vmArgs "-Xmx512m")

          (:java-test
           :type "java"
           :request "launch"
           :name "Java Test"
           :mainClass "org.junit.runner.JUnitCore"
           :args ""
           :vmArgs "-Xmx256m")

          ;; Node.js templates
          (:node-file
           :type "node"
           :request "launch"
           :name "Node.js File"
           :program nil  ; Will prompt
           :cwd nil      ; Will use project root
           :console "integratedTerminal"
           :internalConsoleOptions "neverOpen")

          (:node-attach
           :type "node"
           :request "attach"
           :name "Node.js Attach"
           :port 9229
           :address "localhost"
           :localRoot nil
           :remoteRoot nil)

          ;; Python templates
          (:python-file
           :type "python"
           :request "launch"
           :name "Python File"
           :program nil  ; Will prompt
           :console "integratedTerminal"
           :justMyCode t)

          (:python-module
           :type "python"
           :request "launch"
           :name "Python Module"
           :module nil   ; Will prompt
           :console "integratedTerminal")

          ;; Go templates
          (:go-main
           :type "go"
           :request "launch"
           :name "Go Main"
           :mode "debug"
           :program nil) ; Will prompt

          ;; C/C++ templates
          (:cpp-gdb
           :type "gdb"
           :request "launch"
           :name "C++ GDB"
           :target nil   ; Will prompt
           :cwd nil))))   ; Will use project root

;; ----------------------------
;; Helper functions (defined before use)
;; ----------------------------
(defun bdostumski/dap-detect-project-type ()
  "Detect project type for smart debugging."
  (cond
   ((file-exists-p "pom.xml") "java")
   ((file-exists-p "build.gradle") "java")
   ((file-exists-p "package.json") "node")
   ((file-exists-p "requirements.txt") "python")
   ((file-exists-p "Pipfile") "python")
   ((file-exists-p "pyproject.toml") "python")
   ((file-exists-p "go.mod") "go")
   ((file-exists-p "Cargo.toml") "rust")
   ((or (file-exists-p "CMakeLists.txt")
        (file-exists-p "Makefile")) "cpp")
   (t nil)))

(defun bdostumski/dap-mode-setup ()
  "Setup hook for DAP mode activation."
  (message "DAP debugging mode activated")
  (bdostumski/dap-save-window-layout)

  ;; Auto-configure common breakpoints if enabled
  (when bdostumski/dap-auto-configure-breakpoints
    (bdostumski/dap-auto-set-breakpoints)))

(defun bdostumski/dap-ui-setup ()
  "Setup DAP UI when activated."
  (message "DAP UI activated"))

(defun bdostumski/dap-save-window-layout ()
  "Save current window layout before debugging."
  (setq bdostumski/dap-ui-layout-saved (current-window-configuration)))

(defun bdostumski/dap-restore-window-layout ()
  "Restore window layout from before debugging."
  (when bdostumski/dap-ui-layout-saved
    (set-window-configuration bdostumski/dap-ui-layout-saved)
    (setq bdostumski/dap-ui-layout-saved nil)
    (message "Window layout restored")))

;; ----------------------------
;; Enhanced DAP-mode configuration
;; ----------------------------
(use-package! dap-mode
  :after lsp-mode
  :commands (dap-debug
             dap-debug-last
             dap-breakpoint-toggle
             dap-ui-mode
             dap-ui-many-windows-mode)

  :hook ((dap-mode . bdostumski/dap-mode-setup)
         (dap-ui-mode . bdostumski/dap-ui-setup))

  :init
  ;; Pre-configuration settings
  (setq dap-print-io t                         ; Print debug I/O
        dap-auto-configure-features            ; Auto-configure features
        '(sessions locals controls tooltip))

  :config
  ;; Core DAP configuration
  (setq dap-breakpoints-file (expand-file-name "dap-breakpoints" doom-cache-dir)
        dap-utils-extension-path (expand-file-name "dap-extension" doom-cache-dir))

  ;; Enable enhanced UI components
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  ;; Load language-specific adapters (now defined before this point)
  (bdostumski/dap-load-language-adapters)

  ;; Setup custom debug templates
  (bdostumski/dap-setup-debug-templates)

  ;; Mark as initialized
  (setq bdostumski/dap-initialized t))

;; ----------------------------
;; Session management
;; ----------------------------
(defun bdostumski/dap-debug-smart ()
  "Smart debug function that chooses appropriate method based on context."
  (interactive)
  (cond
   ;; If there's a recent session, offer to restart
   (bdostumski/dap-last-session-config
    (if (y-or-n-p "Restart last debug session? ")
        (dap-debug bdostumski/dap-last-session-config)
      (call-interactively #'dap-debug)))

   ;; If in a known project type, suggest appropriate template
   ((bdostumski/dap-detect-project-type)
    (let ((project-type (bdostumski/dap-detect-project-type)))
      (if (y-or-n-p (format "Debug as %s project? " project-type))
          (bdostumski/dap-debug-with-template (intern (format ":%s-main" project-type)))
        (call-interactively #'dap-debug))))

   ;; Default to interactive debug
   (t (call-interactively #'dap-debug))))

(defun bdostumski/dap-debug-with-template (template-key)
  "Start debugging with a specific template."
  (interactive
   (list (intern (completing-read "Debug template: "
                                  (mapcar (lambda (tmpl)
                                            (symbol-name (car tmpl)))
                                          (seq-partition bdostumski/dap-debug-templates 2))
                                  nil t))))
  (let ((template (plist-get bdostumski/dap-debug-templates template-key)))
    (if template
        (progn
          ;; Fill in missing values interactively
          (bdostumski/dap-configure-template template)
          (setq bdostumski/dap-last-session-config template)
          (dap-debug template))
      (message "Template not found: %s" template-key))))

(defun bdostumski/dap-configure-template (template)
  "Interactively configure a debug template."
  (when (null (plist-get template :program))
    (plist-put template :program
               (read-file-name "Program to debug: "
                               (or (projectile-project-root) default-directory))))

  (when (null (plist-get template :cwd))
    (plist-put template :cwd (or (projectile-project-root) default-directory)))

  ;; Language-specific configuration
  (cond
   ((string= (plist-get template :type) "java")
    (when (null (plist-get template :mainClass))
      (plist-put template :mainClass
                 (read-string "Main class: "))))

   ((string= (plist-get template :type) "python")
    (when (and (null (plist-get template :program))
               (null (plist-get template :module)))
      (if (y-or-n-p "Debug as module? ")
          (plist-put template :module
                     (read-string "Module name: "))
        (plist-put template :program
                   (read-file-name "Python file: ")))))

   ((string= (plist-get template :type) "go")
    (when (null (plist-get template :program))
      (plist-put template :program
                 (read-directory-name "Go package directory: "
                                      (or (projectile-project-root) default-directory)))))))

(defun bdostumski/dap-kill-all-sessions ()
  "Kill all active debug sessions."
  (interactive)
  (if (and (fboundp 'dap--get-sessions) (dap--get-sessions))
      (progn
        (dap-delete-all-sessions)
        (bdostumski/dap-restore-window-layout)
        (message "✓ All debug sessions terminated"))
    (message "No active debug sessions")))

;; ----------------------------
;; Breakpoint management
;; ----------------------------
(defun bdostumski/dap-toggle-breakpoint-with-condition ()
  "Toggle breakpoint with optional condition."
  (interactive)
  (if current-prefix-arg
      (let ((condition (read-string "Breakpoint condition: ")))
        (dap-breakpoint-condition condition))
    (dap-breakpoint-toggle)))

(defun bdostumski/dap-clear-all-breakpoints ()
  "Clear all breakpoints in current session."
  (interactive)
  (when (y-or-n-p "Clear all breakpoints? ")
    (dap-breakpoint-delete-all)
    (message "✓ All breakpoints cleared")))

;; ----------------------------
;; Expression evaluation
;; ----------------------------
(defun bdostumski/dap-eval-region-or-symbol ()
  "Evaluate region if selected, otherwise symbol at point."
  (interactive)
  (if (use-region-p)
      (dap-eval-region (region-beginning) (region-end))
    (dap-eval-thing-at-point)))

(defun bdostumski/dap-add-watch-expression ()
  "Add expression to watch list."
  (interactive)
  (let ((expression (if (use-region-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (read-string "Watch expression: "))))
    (dap-ui-expressions-add expression)
    (message "Added to watch: %s" expression)))

;; ----------------------------
;; UI management
;; ----------------------------
(defun bdostumski/dap-ui-many-windows ()
  "Open debug UI with many windows layout."
  (interactive)
  (dap-ui-many-windows-mode 1)
  (message "DAP UI many windows mode enabled"))

(defun bdostumski/dap-ui-hide-all ()
  "Hide all debug UI windows."
  (interactive)
  (dap-ui-many-windows-mode -1)
  (message "DAP UI windows hidden"))

;; ----------------------------
;; Stepping and execution control
;; ----------------------------
(defun bdostumski/dap-continue-or-start ()
  "Continue debugging if active, otherwise start debugging."
  (interactive)
  (if (and (fboundp 'dap--get-sessions) (dap--get-sessions))
      (dap-continue)
    (bdostumski/dap-debug-smart)))

;; ----------------------------
;; Auto-breakpoint configuration
;; ----------------------------
(defun bdostumski/dap-auto-set-breakpoints ()
  "Automatically set common breakpoints based on file type."
  (cond
   ;; Java: Set breakpoint on main method
   ((eq major-mode 'java-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "public static void main" nil t)
        (dap-breakpoint-toggle)
        (message "Auto-breakpoint set on main method"))))

   ;; Python: Set breakpoint on if __name__ == "__main__"
   ((eq major-mode 'python-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "if __name__ == ['\"]__main__['\"]:" nil t)
        (dap-breakpoint-toggle)
        (message "Auto-breakpoint set on main block"))))

   ;; JavaScript/Node.js: Set breakpoint on first function
   ((or (eq major-mode 'js-mode) (eq major-mode 'typescript-mode))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "function\\|const.*=" nil t)
        (dap-breakpoint-toggle)
        (message "Auto-breakpoint set on first function"))))))

;; ----------------------------
;; Comprehensive keybinding setup
;; ----------------------------
(map! :leader
      (:prefix-map ("d" . "debug")
       ;; Core debugging operations
       :desc "Debug smart/start"        "d" #'bdostumski/dap-debug-smart
       :desc "Debug with template"      "t" #'bdostumski/dap-debug-with-template
       :desc "Debug last session"       "l" #'dap-debug-last
       :desc "Restart session"          "r" #'dap-debug-restart
       :desc "Quit/disconnect"          "q" #'dap-disconnect
       :desc "Kill all sessions"        "k" #'bdostumski/dap-kill-all-sessions
       :desc "Continue execution"       "c" #'bdostumski/dap-continue-or-start

       ;; Breakpoint management
       (:prefix ("b" . "breakpoints")
        :desc "Toggle breakpoint"       "b" #'dap-breakpoint-toggle
        :desc "Conditional breakpoint"  "c" #'bdostumski/dap-toggle-breakpoint-with-condition
        :desc "Delete all breakpoints"  "D" #'bdostumski/dap-clear-all-breakpoints
        :desc "List breakpoints"        "l" #'dap-ui-breakpoints)

       ;; Stepping operations
       (:prefix ("s" . "step")
        :desc "Step over"               "o" #'dap-next
        :desc "Step into"               "i" #'dap-step-in
        :desc "Step out"                "u" #'dap-step-out
        :desc "Continue"                "c" #'dap-continue)

       ;; Expression evaluation
       (:prefix ("e" . "eval")
        :desc "Eval at point"           "e" #'dap-eval-thing-at-point
        :desc "Eval region/symbol"      "r" #'bdostumski/dap-eval-region-or-symbol
        :desc "Eval expression"         "E" #'dap-eval)

       ;; Watch expressions
       (:prefix ("w" . "watch")
        :desc "Add watch"               "a" #'bdostumski/dap-add-watch-expression
        :desc "Show expressions"        "w" #'dap-ui-expressions)

       ;; UI and windows
       (:prefix ("u" . "ui")
        :desc "Show many windows"       "m" #'bdostumski/dap-ui-many-windows
        :desc "Hide windows"            "h" #'bdostumski/dap-ui-hide-all
        :desc "Show locals"             "l" #'dap-ui-locals
        :desc "Show breakpoints"        "b" #'dap-ui-breakpoints
        :desc "Show expressions"        "e" #'dap-ui-expressions
        :desc "Show sessions"           "s" #'dap-ui-sessions
        :desc "Restore layout"          "R" #'bdostumski/dap-restore-window-layout)))

;; Global quick access for debugging
(map! "C-<f5>"   #'bdostumski/dap-debug-smart
      "<f5>"     #'dap-continue
      "S-<f5>"   #'dap-disconnect
      "<f9>"     #'dap-breakpoint-toggle
      "<f10>"    #'dap-next
      "<f11>"    #'dap-step-in
      "S-<f11>"  #'dap-step-out)

;; ----------------------------
;; Cleanup and state management
;; ----------------------------
(defun bdostumski/dap-cleanup-on-exit ()
  "Clean up DAP sessions on Emacs exit."
  (when (and (fboundp 'dap--get-sessions) (dap--get-sessions))
    (dap-delete-all-sessions)
    (message "DAP sessions cleaned up on exit")))

;; Register cleanup hook
(add-hook 'kill-emacs-hook #'bdostumski/dap-cleanup-on-exit)

(provide 'tools-debugger-config)

;;; tools-debugger-config.el ends here
