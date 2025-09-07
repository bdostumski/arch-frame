;;; module/config/tools-config/tools-lsp-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive LSP configuration for Doom Emacs with advanced features:
;; 
;; FEATURES:
;; - Deferred loading for faster startup times
;; - Smart performance optimizations with automatic tuning
;; - Enhanced UI with better error handling and visual feedback
;; - Restart-safe state management with cleanup hooks
;; - Extensive keybinding system using leader and localleader
;; - Advanced workspace management and debugging tools
;; - Integration with Treemacs, Ivy, Flycheck, and Company
;; - Smart LSP activation based on project detection

;;; Code:

;; ----------------------------
;; State tracking variables
;; ----------------------------
(defvar +lsp/lsp-initialized nil
  "Track if LSP has been properly initialized to prevent double-setup.")

(defvar +lsp/lsp-performance-applied nil
  "Track if performance optimizations have been applied.")

(defvar +lsp/lsp-gc-cons-threshold-original nil
  "Original gc-cons-threshold value to restore on cleanup.")

;; Store original values for restoration
(unless +lsp/lsp-gc-cons-threshold-original
  (setq +lsp/lsp-gc-cons-threshold-original gc-cons-threshold))

;; ----------------------------
;; Performance optimization functions
;; ----------------------------
(defun +lsp/lsp-apply-performance-settings ()
  "Apply LSP-specific performance optimizations.
  
Increases garbage collection threshold and process output buffer size
to improve LSP responsiveness during heavy operations."
  (unless +lsp/lsp-performance-applied
    (setq read-process-output-max (* 1024 1024)  ; 1MB for faster LSP communication
          gc-cons-threshold (* 100 1024 1024)    ; 100MB during LSP sessions
          +lsp/lsp-performance-applied t)
    (message "✓ LSP performance optimizations applied")))

(defun +lsp/lsp-restore-performance-settings ()
  "Restore original Emacs performance settings.
  
Called automatically on Emacs exit or when LSP is disabled."
  (when +lsp/lsp-performance-applied
    (setq gc-cons-threshold +lsp/lsp-gc-cons-threshold-original
          +lsp/lsp-performance-applied nil)
    (message "✓ Original performance settings restored")))

;; ----------------------------
;; Smart LSP activation logic
;; ----------------------------
(defun +lsp/lsp-maybe-enable ()
  "Conditionally enable LSP based on project context and file type.
  
Only activates LSP for:
- Files with proper file paths (not temporary buffers)
- Non-remote files (local development)
- Buffers that pass our activation criteria"
  (when (and (buffer-file-name)
             (not (file-remote-p default-directory))
             (+lsp/lsp-should-activate-p))
    (lsp-deferred)))

(defun +lsp/lsp-should-activate-p ()
  "Determine if LSP should be activated for the current buffer.
  
Returns t if:
- Buffer is in a programming mode (not fundamental-mode)
- Buffer name doesn't match system buffer patterns
- Current directory contains project indicators"
  (and (not (derived-mode-p 'fundamental-mode))
       (not (string-match-p "\\*.*\\*" (buffer-name)))
       (or (projectile-project-p)
           (vc-root-dir)
           ;; Language-specific project indicators
           (file-exists-p "package.json")      ; Node.js/JavaScript
           (file-exists-p "Cargo.toml")        ; Rust
           (file-exists-p "go.mod")            ; Go
           (file-exists-p "requirements.txt")  ; Python
           (file-exists-p "Pipfile")           ; Python (Pipenv)
           (file-exists-p "pyproject.toml")    ; Python (Poetry)
           (file-exists-p "pom.xml")           ; Java (Maven)
           (file-exists-p "build.gradle")      ; Java/Kotlin (Gradle)
           (file-exists-p "Gemfile")           ; Ruby
           (file-exists-p "composer.json")     ; PHP
           (file-exists-p "mix.exs")           ; Elixir
           (file-exists-p "dub.json")          ; D
           (file-exists-p "stack.yaml"))))     ; Haskell

;; ----------------------------
;; Core LSP mode configuration
;; ----------------------------
(use-package! lsp-mode
  :defer t
  :commands (lsp lsp-mode lsp-deferred)
  :hook ((prog-mode . +lsp/lsp-maybe-enable)
         (lsp-mode . +lsp/lsp-mode-setup))

  :init
  ;; Essential pre-load settings
  (setq lsp-keymap-prefix "C-c l"              ; Standard LSP prefix
        lsp-auto-guess-root t                  ; Smart project root detection
        lsp-prefer-flymake nil                 ; Use Flycheck instead
        lsp-completion-provider :none)         ; Let Doom handle completion

  :config
  ;; Apply performance optimizations
  (+lsp/lsp-apply-performance-settings)

  ;; Core LSP behavior configuration
  (setq 
   ;; Performance settings
   lsp-idle-delay 0.3                          ; Delay before LSP requests
   lsp-log-io nil                              ; Disable I/O logging
   lsp-log-max 0                               ; No log retention
   lsp-print-performance nil                   ; Disable perf logging
   
   ;; Core features
   lsp-enable-symbol-highlighting t            ; Highlight symbol under cursor
   lsp-enable-snippet t                        ; Enable snippet support
   lsp-signature-auto-activate t               ; Show function signatures
   lsp-signature-doc-lines 2                   ; Max signature doc lines
   lsp-signature-render-documentation t        ; Include docs in signatures
   lsp-lens-enable t                           ; Enable code lens
   
   ;; UI elements
   lsp-headerline-breadcrumb-enable t          ; Show breadcrumb navigation
   lsp-modeline-code-actions-enable t          ; Show code actions in modeline
   lsp-modeline-diagnostics-enable t           ; Show diagnostics in modeline
   lsp-eldoc-enable-hover t                    ; Enable eldoc hover

   ;; Completion settings
   lsp-completion-enable-additional-text-edit t ; Enable auto-imports
   lsp-completion-show-detail t                ; Show completion details
   lsp-completion-show-kind t                  ; Show completion item kinds
   
   ;; Workspace configuration
   lsp-auto-configure t                        ; Auto-configure clients
   lsp-response-timeout 10                     ; Server response timeout
   lsp-restart 'auto-restart                   ; Auto-restart on crash
   lsp-keep-workspace-alive nil                ; Don't keep workspace alive
   
   ;; File and cache locations
   lsp-session-file (expand-file-name "lsp-session" doom-cache-dir)
   lsp-server-install-dir (expand-file-name "lsp/servers/" doom-cache-dir))

  ;; Mark as initialized
  (setq +lsp/lsp-initialized t))

;; In your Doom Emacs config.el
(after! lsp-headerline
  (custom-set-faces!
    '(lsp-headerline-breadcrumb-path-face :underline nil)
    '(lsp-headerline-breadcrumb-path-error-face :underline nil)
    '(lsp-headerline-breadcrumb-path-info-face :underline nil)
    '(lsp-headerline-breadcrumb-path-warning-face :underline nil)
    '(lsp-headerline-breadcrumb-path-hint-face :underline nil)
    '(lsp-headerline-breadcrumb-symbols-face :underline nil)))


;; ----------------------------
;; LSP mode setup hook
;; ----------------------------
(defun +lsp/lsp-mode-setup ()
  "Setup hook for LSP mode activation.
   Configures company completion, which-key integratand buffer-local keybindings."
  ;; Configure company completion for LSP
  (setq-local company-backends
              (cons 'company-capf
                    (remove 'company-capf company-backends)))

  ;; Enable which-key integration for LSP commands
  (when (featurep 'which-key)
    (lsp-enable-which-key-integration))

  ;; Setup buffer-local keybindings
  (+lsp/lsp-setup-buffer-keys))

;; ----------------------------
;; Enhanced LSP-UI configuration
;; ----------------------------
(use-package! lsp-ui
  :after lsp-mode
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode 
             lsp-ui-peek-find-definitions 
             lsp-ui-peek-find-references
             lsp-ui-peek-find-implementation
             lsp-ui-doc-show
             lsp-ui-doc-hide
             lsp-ui-imenu)

  :config
  ;; Documentation display configuration
  (setq lsp-ui-doc-enable t                    ; Enable documentation popups
        lsp-ui-doc-use-childframe t            ; Use child frames
        lsp-ui-doc-position 'at-point          ; Show at cursor position
        lsp-ui-doc-show-with-cursor nil        ; Don't show automatically
        lsp-ui-doc-show-with-mouse t           ; Show on mouse hover
        lsp-ui-doc-delay 0.5                   ; Delay before showing
        lsp-ui-doc-max-height 30               ; Maximum height
        lsp-ui-doc-max-width 120               ; Maximum width
        lsp-ui-doc-include-signature t         ; Include function signature
        lsp-ui-doc-alignment 'window           ; Align with window
        lsp-ui-doc-use-webkit nil)             ; Don't use webkit

  ;; Sideline configuration (inline diagnostics and info)
  (setq lsp-ui-sideline-enable t               ; Enable sideline
        lsp-ui-sideline-show-hover nil         ; Don't show hover info
        lsp-ui-sideline-show-diagnostics t     ; Show diagnostics inline
        lsp-ui-sideline-show-symbol nil        ; Don't show symbol info
        lsp-ui-sideline-show-code-actions t    ; Show available code actions
        lsp-ui-sideline-ignore-duplicate t     ; Ignore duplicate entries
        lsp-ui-sideline-delay 0.3              ; Delay before showing
        lsp-ui-sideline-update-mode 'line)     ; Update per line

  ;; Peek definitions/references configuration
  (setq lsp-ui-peek-enable t                  ; Enable peek windows
        lsp-ui-peek-always-show nil           ; Don't always show
        lsp-ui-peek-fontify 'always           ; Always fontify code
        lsp-ui-peek-peek-height 20            ; Peek window height
        lsp-ui-peek-list-width 50             ; List window width
        lsp-ui-peek-expand-function 'lsp-ui-peek--expand-buffer) ; Expand function

  ;; Imenu/symbols configuration
  (setq lsp-ui-imenu-enable t                 ; Enable imenu integration
        lsp-ui-imenu-auto-refresh 'after-save ; Refresh after save
        lsp-ui-imenu-buffer-position 'right   ; Show on right side
        lsp-ui-imenu-window-width 35))        ; Window width

;; ----------------------------
;; Enhanced Treemacs integration
;; ----------------------------
(use-package! lsp-treemacs
  :after (lsp treemacs)
  :defer t
  :commands (lsp-treemacs-symbols 
             lsp-treemacs-references 
             lsp-treemacs-errors-list
             lsp-treemacs-call-hierarchy
             lsp-treemacs-type-hierarchy)
  :config
  (setq lsp-treemacs-sync-mode 1              ; Enable sync mode
        lsp-treemacs-symbols-position-params   ; Position configuration
        `((side . right)
          (slot . 2)
          (window-width . ,treemacs-width))))

;; ----------------------------
;; Ivy integration for workspace symbols
;; ----------------------------
(use-package! lsp-ivy
  :after (lsp ivy)
  :defer t
  :commands (lsp-ivy-workspace-symbol 
             lsp-ivy-global-workspace-symbol))

;; ----------------------------
;; Enhanced integrations
;; ----------------------------
(after! flycheck
  (when (featurep 'lsp-mode)
    (setq flycheck-indication-mode 'left-fringe    ; Show indicators in fringe
          flycheck-highlighting-mode 'symbols      ; Highlight symbols
          flycheck-check-syntax-automatically       ; When to check
          '(save idle-change mode-enabled)
          flycheck-idle-change-delay 0.5           ; Check delay
          flycheck-display-errors-delay 0.3)))     ; Error display delay

(after! company
  (when (featurep 'lsp-mode)
    (setq company-idle-delay 0.1                   ; Quick completion
          company-minimum-prefix-length 1          ; Start after 1 char
          company-show-quick-access t               ; Show quick access keys
          company-tooltip-align-annotations t      ; Align annotations
          company-tooltip-limit 10                 ; Limit suggestions
          company-tooltip-maximum-width 80         ; Max tooltip width
          company-abort-manual-when-too-short t))) ; Auto-abort short queries

;; Yasnippet integration
(use-package! yasnippet
  :defer t
  :hook ((lsp-mode . yas-minor-mode-on)
         (prog-mode . yas-minor-mode-on))
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 (list (expand-file-name "snippets" doom-user-dir))))
  (yas-reload-all))

;; ----------------------------
;; Advanced LSP workspace management
;; ----------------------------
(defun +lsp/lsp-workspace-restart ()
  "Restart LSP workspace safely with user feedback."
  (interactive)
  (if (lsp-workspaces)
      (progn
        (message "Restarting LSP workspace...")
        (call-interactively #'lsp-workspace-restart)
        (message "✓ LSP workspace restarted successfully"))
    (message "No active LSP workspace to restart")))

(defun +lsp/lsp-workspace-cleanup ()
  "Clean up LSP workspace and session data completely."
  (interactive)
  (if (lsp-workspaces)
      (progn
        (message "Cleaning up LSP workspace...")
        (lsp-workspace-shutdown)
        (lsp-session-save)
        (message "✓ LSP workspace cleaned up"))
    (message "No active LSP workspace to clean up")))

(defun +lsp/lsp-clear-cache ()
  "Clear LSP cache and restart workspace."
  (interactive)
  (let ((cache-dir (expand-file-name "lsp" doom-cache-dir)))
    (when (file-directory-p cache-dir)
      (message "Clearing LSP cache...")
      (delete-directory cache-dir t)
      (message "✓ LSP cache cleared"))
    (when (lsp-workspaces)
      (lsp-workspace-restart))))

;; ----------------------------
;; UI and feature toggles
;; ----------------------------
(defun +lsp/lsp-toggle-symbol-highlighting ()
  "Toggle LSP symbol highlighting with visual feedback."
  (interactive)
  (setq lsp-enable-symbol-highlighting (not lsp-enable-symbol-highlighting))
  (lsp-restart-workspace)
  (message "LSP symbol highlighting: %s"
           (if lsp-enable-symbol-highlighting "✓ enabled" "✗ disabled")))

(defun +lsp/lsp-toggle-lens ()
  "Toggle LSP code lens display."
  (interactive)
  (setq lsp-lens-enable (not lsp-lens-enable))
  (if lsp-lens-enable
      (lsp-lens-refresh t)
    (lsp-lens-hide))
  (message "LSP code lens: %s" 
           (if lsp-lens-enable "✓ enabled" "✗ disabled")))

(defun +lsp/lsp-toggle-breadcrumb ()
  "Toggle LSP headerline breadcrumb navigation."
  (interactive)
  (setq lsp-headerline-breadcrumb-enable (not lsp-headerline-breadcrumb-enable))
  (lsp-headerline-breadcrumb-mode (if lsp-headerline-breadcrumb-enable 1 -1))
  (message "LSP breadcrumb: %s" 
           (if lsp-headerline-breadcrumb-enable "✓ enabled" "✗ disabled")))

(defun +lsp/lsp-toggle-diagnostics ()
  "Toggle LSP diagnostics display in sideline."
  (interactive)
  (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
  (lsp-ui-sideline--run)
  (message "LSP diagnostics: %s"
           (if lsp-ui-sideline-show-diagnostics "✓ shown" "✗ hidden")))

;; ----------------------------
;; Debugging and diagnostics
;; ----------------------------
(defun +lsp/lsp-show-workspace-info ()
  "Show comprehensive LSP workspace information."
  (interactive)
  (if (lsp-workspaces)
      (let* ((workspace (lsp--current-workspace))
             (server-id (lsp--workspace-server-id workspace))
             (root (lsp--workspace-root workspace))
             (status (lsp--workspace-status workspace))
             (buffers-count (length (lsp--workspace-buffers workspace))))
        (message "LSP Info: %s | Root: %s | Status: %s | Buffers: %d"
                 server-id (abbreviate-file-name root) status buffers-count))
    (message "No active LSP workspace")))

(defun +lsp/lsp-show-buffer-diagnostics ()
  "Show diagnostics for the current buffer in a dedicated buffer."
  (interactive)
  (if (lsp-workspaces)
      (lsp-show-flycheck-buffer)
    (message "No active LSP workspace")))

(defun +lsp/lsp-performance-report ()
  "Show LSP performance report and enable logging."
  (interactive)
  (if (lsp-workspaces)
      (progn
        (setq lsp-print-performance t
              lsp-log-io t)
        (message "✓ LSP performance logging enabled. Check *lsp-log* buffer for details.")
        (when (get-buffer "*lsp-log*")
          (display-buffer "*lsp-log*")))
    (message "No active LSP workspace")))

;; ----------------------------
;; State management and cleanup
;; ----------------------------
(defun +lsp/lsp-reset-state ()
  "Reset LSP state for clean restarts."
  (interactive)
  (setq +lsp/lsp-initialized nil)
  
  ;; Restore performance settings
  (+lsp/lsp-restore-performance-settings)
  
  ;; Clean up any LSP processes if needed
  (when (fboundp 'lsp-workspace-shutdown)
    (condition-case nil
        (dolist (workspace (lsp-workspaces))
          (lsp-workspace-shutdown workspace))
      (error nil)))
  
  (message "✓ LSP state reset successfully"))

;; ----------------------------
;; Comprehensive keybinding setup
;; ----------------------------
(defun +lsp/lsp-setup-buffer-keys ()
  "Setup comprehensive buffer-local LSP keybindings using localleader."
  (map! :localleader
        :map lsp-mode-map
        
        ;; Navigation commands (g prefix)
        (:prefix ("g" . "goto")
         :desc "Find definition"         "d" #'lsp-find-definition
         :desc "Find references"         "r" #'lsp-find-references  
         :desc "Find implementation"     "i" #'lsp-find-implementation
         :desc "Find type definition"    "t" #'lsp-find-type-definition
         :desc "Find declaration"        "D" #'lsp-find-declaration
         :desc "Go back"                 "b" #'xref-pop-marker-stack
         :desc "Go forward"              "f" #'xref-go-forward)

        ;; Peek operations (p prefix) 
        (:prefix ("p" . "peek")
         :desc "Peek definition"         "d" #'lsp-ui-peek-find-definitions
         :desc "Peek references"         "r" #'lsp-ui-peek-find-references
         :desc "Peek implementation"     "i" #'lsp-ui-peek-find-implementation)

        ;; Help and documentation (h prefix)
        (:prefix ("h" . "help")
         :desc "Show hover doc"          "h" #'lsp-ui-doc-show
         :desc "Hide documentation"      "H" #'lsp-ui-doc-hide
         :desc "Signature help"          "s" #'lsp-signature-activate
         :desc "Describe at point"       "d" #'lsp-describe-thing-at-point)

        ;; Code actions (a prefix)
        (:prefix ("a" . "actions")  
         :desc "Code actions"            "a" #'lsp-execute-code-action
         :desc "Auto-import"             "i" #'lsp-execute-code-action
         :desc "Quick fix"               "q" #'lsp-execute-code-action)

        ;; Refactoring operations (r prefix)
        (:prefix ("r" . "refactor")
         :desc "Rename symbol"           "r" #'lsp-rename
         :desc "Organize imports"        "o" #'lsp-organize-imports)

        ;; Formatting (f prefix)
        (:prefix ("f" . "format")
         :desc "Format buffer"           "f" #'lsp-format-buffer
         :desc "Format region"           "r" #'lsp-format-region)

        ;; Symbols and search (s prefix)
        (:prefix ("s" . "symbols")
         :desc "Workspace symbols"       "s" #'lsp-ivy-workspace-symbol
         :desc "Imenu symbols"           "i" #'lsp-ui-imenu)

        ;; Diagnostics (d prefix)
        (:prefix ("d" . "diagnostics")
         :desc "Show diagnostics"        "d" #'+lsp/lsp-show-buffer-diagnostics
         :desc "Next diagnostic"         "n" #'flycheck-next-error
         :desc "Previous diagnostic"     "p" #'flycheck-previous-error)

        ;; Toggles (t prefix)
        (:prefix ("t" . "toggles")
         :desc "Toggle lens"             "l" #'+lsp/lsp-toggle-lens
         :desc "Toggle breadcrumb"       "b" #'+lsp/lsp-toggle-breadcrumb
         :desc "Toggle highlighting"     "h" #'+lsp/lsp-toggle-symbol-highlighting
         :desc "Toggle diagnostics"      "d" #'+lsp/lsp-toggle-diagnostics)

        ;; Workspace management (w prefix)
        (:prefix ("w" . "workspace")
         :desc "Restart workspace"       "r" #'+lsp/lsp-workspace-restart
         :desc "Shutdown workspace"      "s" #'+lsp/lsp-workspace-cleanup
         :desc "Workspace info"          "i" #'+lsp/lsp-show-workspace-info
         :desc "Clear cache"             "c" #'+lsp/lsp-clear-cache)))

;; ----------------------------
;; Global keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("s" . "lsp")
                                 ;; Workspace management
                                 :desc "Start LSP"                "s" #'lsp
                                 :desc "Start deferred"           "S" #'lsp-deferred
                                 :desc "Restart workspace"        "r" #'+lsp/lsp-workspace-restart
                                 :desc "Shutdown workspace"       "q" #'+lsp/lsp-workspace-cleanup
                                 :desc "Clear cache"              "c" #'+lsp/lsp-clear-cache
                                 :desc "Workspace info"           "i" #'+lsp/lsp-show-workspace-info
                                 :desc "Performance report"       "p" #'+lsp/lsp-performance-report
                                 
                                 ;; Treemacs integration
                                 :desc "Treemacs symbols"         "t" #'lsp-treemacs-symbols
                                 :desc "Treemacs errors"          "e" #'lsp-treemacs-errors-list
                                 
                                 ;; UI toggles
                                 :desc "Toggle sideline"          "u" #'lsp-ui-sideline-mode
                                 :desc "Toggle doc mode"          "d" #'lsp-ui-doc-mode
                                 :desc "Toggle diagnostics"       "D" #'+lsp/lsp-toggle-diagnostics
                                 :desc "Toggle highlighting"      "h" #'+lsp/lsp-toggle-symbol-highlighting
                                 :desc "Toggle breadcrumb"        "b" #'+lsp/lsp-toggle-breadcrumb
                                 :desc "Toggle lens"              "l" #'+lsp/lsp-toggle-lens))))

;; Additional integration with existing Doom keybindings
(map! :leader
      (:prefix-map ("c" . "code")
       :desc "Start LSP"                "L" #'lsp
       :desc "LSP workspace info"       "I" #'+lsp/lsp-show-workspace-info
       :desc "LSP restart"              "R" #'+lsp/lsp-workspace-restart))

;; Hook for Doom restarts and cleanup
(add-hook 'doom-after-reload-hook #'+lsp/lsp-reset-state)
(add-hook 'kill-emacs-hook #'+lsp/lsp-restore-performance-settings)

(provide 'tools-lsp-config)

;;; tools-lsp-config.el ends here

