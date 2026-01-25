;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ============================================================================
;; IDENTITY
;; ============================================================================
(setq user-full-name "Borislav Alexandrov Dostumski"
      user-mail-address "b.dostumski@gmail.com")

;; ============================================================================
;; APPEARANCE
;; ============================================================================
(use-package! doom-themes
  :init
  (setq

   ;; COMMON
   doom-themes-enable-bold t
   doom-themes-enable-italic t
   doom-themes-padded-modeline 4
   display-line-numbers-type 'relative
   hl-line-mode t

   ;; FILE HANDLING
   auto-save-default t
   make-backup-files t
   create-lockfiles nil

   ;; PROJECTILE
   projectile-enable-caching t
   projectile-indexing-method 'hybrid
   projectile-project-search-path '("~/Workspace" "~/Documents") 

   ;; FONTS
   doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'medium)
   doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16)
   doom-serif-font (font-spec :family "Noto Serif" :size 16)
   doom-symbol-font (font-spec :family "Symbols Nerd Font")
   doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one t))

;; ============================================================================
;; CREATE DIRECTORY IF NOT EXISTS
;; ============================================================================
(defun ensure-dir-exists (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Ensure snippet directory
(ensure-dir-exists (expand-file-name "snippets" doom-user-dir))

;; Ensure undo-tree-history directory
(ensure-dir-exists (expand-file-name "undo-tree-history/" doom-user-dir))

;; Ensure org directory
(ensure-dir-exists (expand-file-name "org/" doom-user-dir))

;; Ensure org/images directory
(ensure-dir-exists (expand-file-name "org/images" doom-user-dir))

;; Ensure org/roam directory
(ensure-dir-exists (expand-file-name "org/roam/" doom-user-dir))

;; Ensure org/notes directory
(ensure-dir-exists (expand-file-name "org/notes/" doom-user-dir))

;; ============================================================================
;; MODELINE
;; ============================================================================
(after! doom-modeline
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 15
        doom-modeline-lsp t
        doom-modeline-modal-icon t
        doom-modeline-battery nil))

(display-time-mode 1)
(display-battery-mode -1)

;; ============================================================================
;; DIRED + DIRVISH
;; ============================================================================
(after! dired
  (setq dired-use-ls-dired t
        dired-listing-switches "-alh --group-directories-first --time-style=long-iso"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t))

(after! dirvish
  :init (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes '(vc-state subtree-state nerd-icons collapse git-msg file-size)))

;; ============================================================================
;; SUDO-EDIT
;; ============================================================================
(use-package! sudo-edit
  :commands (sudo-edit sudo-edit-find-file)
  :config
  (sudo-edit-indicator-mode +1))  ; Show indicator when editing as sudo

;; ============================================================================
;; YAS-SNIPPET
;; ============================================================================
(after! yasnippet
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" doom-user-dir)))

;; ============================================================================
;; WORD-WRAP
;; ============================================================================
(+global-word-wrap-mode) ;; Enable word-wrap almost everywhere

;; ============================================================================
;; PERFORMANCE TWEAKS
;; ============================================================================
(setq gc-cons-threshold (* 50 1000 1000) ;; Set a high threshold of 100MB during startup
      gc-cons-percentage 0.6)           ;; Temporarily increase GC frequency during startup

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; After startup, lower GC threshold
            (setq gc-cons-threshold (* 2 1000 1000) ;; Lower threshold to 2MB after startup
                  gc-cons-percentage 0.1)))

;; Use gcmh-mode to handle garbage collection dynamically
(use-package! gcmh
  :config
  (setq gcmh-idle-delay 5                      ;; Trigger GC after being idle for 5 seconds
        gcmh-high-cons-threshold (* 128 1024 1024) ;; GC reaches threshold of 128MB when idle
        gcmh-low-cons-threshold (* 8 1024 1024))   ;; Reduce to 8MB during normal operations
  (gcmh-mode 1))                               ;; Enable gcmh-mode

;; ============================================================================
;; AUTOREVERT
;; ============================================================================
(after! autorevert
  (setq global-auto-revert-non-file-buffers t ; Revert Dired and other buffers
        auto-revert-verbose nil              ; Silently revert changes
        auto-revert-use-notify t             ; Use inotify if available
        auto-revert-interval 1               ; Check for updates more frequently if needed
        auto-revert-avoid-polling t))        ; Fallback to polling only if absolutely needed

(global-auto-revert-mode 1)             ; Revert buffers when the underlying file has changed)

;; ============================================================================
;; HL-TODO
;; ============================================================================
(setq hl-todo-keyword-faces
      '(("TODO"       . "#ECBE7B")
        ("FIXME"      . "#FF6C6B")
        ("BUG"        .  "#FF6C6B")
        ("HACK"       . "#C678DD")
        ("NOTE"       . "#51AFEF")
        ("REVIEW"     . "#C678DD")
        ("OPTIMIZE"   . "#ECBE7B")
        ("PERF"       . "#ECBE7B")
        ("SECURITY"   . "#FF6C6B")
        ("DEPRECATED" . "#5B6268")))

;; ============================================================================
;; UNDO-TREE
;; ============================================================================
(after! undo-tree
  (setq undo-tree-enable-undo-in-region t        ;; Allow region-based undo
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-history/" doom-user-dir))) ;; Dedicated folder for undo history
        undo-tree-auto-save-history t))          ;; Enable history auto-saving globally

;; ============================================================================
;; FORMAT
;; ============================================================================
(setq +format-on-save-enabled-modes ;; Exclude modes to format on save
      '(not 
        xml-mode
        nxml-mode
        tex-mode
        latex-mode))

;; ============================================================================
;; ORG-MODE
;; ============================================================================
(setq org-directory (expand-file-name "org/" doom-user-dir))

(after! org
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-image-actual-width nil
        org-ellipsis " ▾"
        org-startup-folded 'content
        org-startup-indented t
        org-agenda-files (list org-directory org-roam-directory)
        org-log-done 'time
        org-log-into-drawer t))

(use-package! org-modern
  :hook (org-mode .  org-modern-mode)
  :config
  (setq org-modern-table-vertical 1
        org-modern-table-horizontal 0.2))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))


(use-package! org-download
  :after org
  :config
  (setq org-download-image-dir (expand-file-name "org/images" org-directory)
        org-download-screenshot-method "xfce4-screenshooter -r -o"))

;; ============================================================================
;; ORG-ROAM 
;; ============================================================================
(setq org-roam-directory (file-truename (expand-file-name "org/roam/" doom-user-dir)))

(after! org-roam
  (setq org-roam-db-gc-threshold most-positive-fixnum  
        org-roam-completion-everywhere t))             

;; ============================================================================
;; DEFT
;; ============================================================================
(setq deft-recursive t                          ;; Search also in subdirectories
      deft-use-filename-as-title t              ;; Use note filenames to generate the displayed titles in deft file browser
      deft-use-filter-string-for-filename t     ;; Slashes are removed and replaced by hyphens,
      deft-file-naming-rules '((noslash . "-")  ;; Naming rules to replace unwanted chars with hyphens
                               (nospace . "-")
                               (case-fn . downcase))
      deft-default-extension '("org" "txt" "text" "tex" "md" "markdown")                ;; Default extensions
      deft-directory (file-truename (expand-file-name "org/notes/" doom-user-dir)))     ;; deft notes home directory

;; ============================================================================
;; CALENDAR
;; ============================================================================
(after! calendar
  (setq calendar-date-style 'iso
        calendar-mark-holidays-flag t
        calendar-week-start-day 1
        holiday-bulgarian-holidays '((holiday-fixed 3 3 "Bulgaria Liberation Day")
                                     )))

;; ============================================================================
;; LSP PERFORMANCE 
;; ============================================================================
(after! lsp-mode
  (setq lsp-response-timeout 30
        lsp-log-io nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-symbol-highlighting nil    
        lsp-completion-provider :capf
        lsp-disabled-clients '(semgrep-ls)))

;; ============================================================================
;; LSP-UI (Single merged block)
;; ============================================================================
(after! lsp-ui
  (setq
   ;; Inline information like references / implementations (IDEA-like)
   lsp-lens-enable t

   ;; Clean breadcrumb in headerline
   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)

   ;; No noisy popups
   lsp-ui-doc-enable nil
   lsp-ui-sideline-enable nil

   ;; Keep peek (very useful)
   lsp-ui-peek-enable t))

;; ============================================================================
;; DAP (Debug Adapter Protocol) - IntelliJ-like Debugging
;; ============================================================================
(after! dap-java
  (setq 
   lsp-java-autobuild-enabled t))

(after! dap-mode
  ;; Enable IntelliJ-like auto UI
  (dap-auto-configure-mode 1)

  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions controls tooltip)

        dap-auto-show-output t
        dap-output-window-min-height 10
        dap-output-window-max-height 20))

;; ============================================================================
;; COMPANY
;; ============================================================================
(after! company
  (setq
   ;; Performance optimization
   company-minimum-prefix-length 1
   company-show-quick-access t
   company-require-match nil
   ;; UI improvements
   company-tooltip-align-annotations t
   company-tooltip-limit 12
   company-tooltip-minimum 6
   company-selection-wrap-around t
   company-tooltip-flip-when-above t
   ;; dabbrev settings
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil
   company-dabbrev-other-buffers t))

;; Enable company in modes
(add-hook 'after-init-hook 'global-company-mode) ;; enable company mode instant
;;(add-hook 'prog-mode-hook #'company-mode)      ;; enable company mode after programming mode
;;(add-hook 'text-mode-hook #'company-mode))     ;; enable company mode after text mode

;; ============================================================================
;; VERTICO
;; ============================================================================
(after! vertico
  ;; UI
  (setq vertico-count 15                        ;; Maximum numbers of candidates to show
        vertico-resize t                        ;; How to resize the Vertico minibuffer window, see resize-mini-windows.
        vertico-cycle t                         ;; Enable cycling for vertico-next and vertico-previous.
        enable-recursive-minibuffers t))        ;; Non-nil means to allow minibuffer commands while in the minibuffer.

;; ============================================================================
;; WHICH-KEY
;; ============================================================================
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0.05)

;; ============================================================================
;; JAVA (LSP + JDTLS) - FIXED VERSION
;; ============================================================================
(after! lsp-java
  ;; Remove the system path - let Doom handle it
  (setq lsp-java-server-install-dir (expand-file-name "lsp/jdtls/" doom-cache-dir)
        ;; Remove this line to allow auto-download: 
        ;; lsp-java-jdt-download-url nil
        lsp-java-java-path (or (executable-find "java") "/usr/bin/java")
        dap-java-java-command (or (executable-find "java") "/usr/bin/java")
        
        ;; Your other settings...
        dap-java-vm-args '("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n")
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled t 
        lsp-java-import-gradle-annotation-processing-enabled t
        lsp-java-save-actions-organize-imports t 
        dap-java-test-runner "junit"
        lsp-java-completion-import-order ["java" "javax" "org" "com"])

  ;; Rest of your configuration...
  (setq lsp-java-completion-favorite-static-members
        ["org.junit.Assert.*" "org.junit. Assume.*" "org.junit.jupiter.api.Assertions.*"
         "org.junit.jupiter. api. Assumptions.*"
         "org.junit.jupiter.api.DynamicContainer.*"
         "org.junit.jupiter.api. DynamicTest.*" "org.mockito.Mockito.*"
         "org.mockito.ArgumentMatchers.*" "org.mockito.Answers.*" "java.util.Objects. requireNonNull"])

  ;; IDE features
  (setq lsp-java-lens-mode t
        lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t)

  ;; Formatter
  (setq lsp-java-format-enabled t
        lsp-java-format-comments-enabled t
        lsp-java-format-on-type-enabled nil)

  ;; JVM args with Lombok
  (let* ((java-lib-dir (expand-file-name "lib/java/" doom-user-dir))
         (lombok-jar   (expand-file-name "lombok.jar" java-lib-dir)))

    (unless (file-directory-p java-lib-dir)
      (make-directory java-lib-dir t))

    (setq lsp-java-vmargs
          '("-XX:+UseG1GC"
            "-XX:+UseStringDeduplication"
            "-Xmx4G"
            "-Xms1G"
            "-XX:+AlwaysPreTouch"
            "-Dsun.zip.disableMemoryMapping=true"))

    (when (file-exists-p lombok-jar)
      (push (concat "-javaagent:" lombok-jar) lsp-java-vmargs))))

;;; config.el ends here
