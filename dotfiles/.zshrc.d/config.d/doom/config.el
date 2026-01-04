;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ============================================================================
;; IDENTITY
;; ============================================================================
(setq user-full-name "Borislav Alexandrov Dostumski"
      user-mail-address "b.dostumski@gmail.com")

;; ============================================================================
;; APPEARANCE
;; ============================================================================
(setq doom-theme 'doom-one)

(use-package doom-themes
  :ensure t
  :custom

  ;; Global settings
  (doom-themes-enable-bold t)           ;; if nil, bold is universally disabled
  (doom-themes-enable-italic t)         ;; if nil, italics is universally disabled
  (doom-themes-padded-modeline t)       ;; Pad the mode-line in 4px on each side
  ;; Must be used *after* the theme is loaded
  (custom-set-faces
   `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
   `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))

  (doom-themes-visual-bell-config)      ;; Enable flashing mode-line on errors
  (doom-themes-neotree-config)          ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-treemacs-config)         ;; or for treemacs users
  (doom-themes-org-config)              ;; Corrects (and improves) org-mode's native fontification.
  (display-line-numbers-type 'relative) ;; Enable relative numbers 

  ;; Fonts settings
  (doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'medium))
  (doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16))
  (doom-serif-font (font-spec :family "Noto Serif" :size 16))
  (doom-symbol-font (font-spec :family "Symbols Nerd Font"))
  (doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))

  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-one") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t))

;; ============================================================================
;; MODELINE
;; ============================================================================
(after! doom-modeline-mode
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
;; PERFORMANCE TWEAKS
;; ============================================================================
;;(setq idle-update-delay 1.0
;;      read-process-output-max (* 4 1024 1024)
;;      bidi-paragraph-direction 'left-to-right
;;      bidi-inhibit-bpa t)

;; ============================================================================
;; FILE HANDLING
;; ============================================================================
(setq auto-save-default t
      make-backup-files t
      create-lockfiles nil)

;; ============================================================================
;; PROJECTILE
;; ============================================================================
(setq projectile-enable-caching t
      projectile-indexing-method 'hybrid
      projectile-project-search-path '("~/Workspace" "~/Documents"))

;; ============================================================================
;; DIRED + DIRVISH
;; ============================================================================
(after!  dired
  :config
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
;; YAS-SNIPPET
;; ============================================================================
(let ((snippets-dir (expand-file-name "snippets" doom-user-dir)))
  (unless (file-directory-p snippets-dir)
    (make-directory snippets-dir t)))

(after! yasnippet
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" doom-user-dir)))


;; ============================================================================
;; WORD-WRAP
;; ============================================================================
(+global-word-wrap-mode) ;; Enable word-wrap almost everywhere

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
;; HL-TODO
;; ============================================================================
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local undo-tree-auto-save-history t)))


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
;; ORG-ROAM (Add after your ORG-MODE section)
;; ============================================================================
(let ((org-roam-dir (expand-file-name "org/roam/" doom-user-dir)))
  (unless (file-directory-p org-roam-dir)
    (make-directory org-roam-dir t)))

(setq org-roam-directory (file-truename (expand-file-name "org/roam/" doom-user-dir)))

(after! org-roam
  (setq org-roam-db-gc-threshold most-positive-fixnum  
        org-roam-completion-everywhere t))             

;; ============================================================================
;; DEFT
;; ============================================================================
(let ((org-notes-dir (expand-file-name "org/notes/" doom-user-dir))) ;; Create /org/notes/ in doom emacs home directory
  (unless (file-directory-p org-notes-dir)
    (make-directory org-notes-dir t)))

(setq deft-recursive t                          ;; Search also in subdirectories
      deft-use-filename-as-title t              ;; Use note filenames to generate the displayed titles in deft file browser
      deft-use-filter-string-for-filename t     ;; Slashes are removed and replaced by hyphens,
      deft-file-naming-rules '((noslash . "-")  ;; Naming rules to replace unwanted chars with hyphens
                               (nospace . "-")
                               (case-fn . downcase))
      deft-default-extension '("org" "txt" "text" "tex" "md" "markdown")                ;; Default extensions
      deft-directory (file-truename (expand-file-name "org/notes/" doom-user-dir)))     ;; deft notes home directory

;; ============================================================================
;; ORG-MODE
;; ============================================================================
(let ((org-notes-dir (expand-file-name "org/" doom-user-dir)))
  (unless (file-directory-p org-notes-dir)
    (make-directory org-notes-dir t)))

(setq org-directory (expand-file-name "org/" doom-user-dir))

(after! org
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
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

;; ============================================================================
;; CALENDAR
;; ============================================================================
(after! calendar
  :config calendar-date-style 'iso
  calendar-mark-holidays-flag t
  calendar-week-start-day 1)

;; ============================================================================
;; LSP PERFORMANCE (Add after your PERFORMANCE TWEAKS section)
;; ============================================================================
;;(after! lsp-mode
;;  (setq lsp-response-timeout 30
;;        lsp-idle-delay 1.0                    
;;        lsp-log-io nil
;;        lsp-enable-file-watchers nil
;;        lsp-enable-folding nil
;;        lsp-enable-text-document-color nil
;;        lsp-enable-symbol-highlighting nil    
;;        lsp-headerline-breadcrumb-enable nil  
;;        lsp-completion-provider :capf
;;        lsp-disabled-clients '(semgrep-ls)))

;; ============================================================================
;; LSP-UI (Single merged block)
;; ============================================================================
;;(after! lsp-ui
;;  (setq lsp-ui-doc-enable nil
;;        lsp-ui-doc-position 'at-point
;;        lsp-ui-doc-show-with-cursor nil     
;;        lsp-ui-doc-show-with-mouse nil
;;        lsp-ui-doc-delay 0.5
;;        lsp-ui-doc-max-height 15
;;        lsp-ui-doc-max-width 80
;;        lsp-ui-sideline-enable t
;;        lsp-ui-sideline-show-diagnostics t
;;        lsp-ui-sideline-show-hover nil      
;;        lsp-ui-sideline-show-code-actions nil
;;        lsp-ui-peek-enable t
;;        lsp-ui-peek-show-directory t))

;; ============================================================================
;; COMPANY
;; ============================================================================
(after! company
  (setq
   ;;Performance optimization
   company-minimum-prefix-length 1           ;; start completion after 1 char
   company-idle-delay 0.1                    ;; fast pop-up (0.0 for instant)
   company-show-quick-access t               ;; show numbers for quick selection
   company-require-match nil                 ;; allow free text input

   ;; UI improvements
   company-tooltip-align-annotations t       ;; align annotations to the right
   company-tooltip-limit 12                  ;; limit tooltip items
   company-tooltip-minimum 6                 ;; minimum tooltip items
   company-selection-wrap-around t           ;; circular navigation
   company-tooltip-flip-when-above t         ;; flip tooltip when above cursor

   ;; Company Backends
   company-backends
   '((company-capf company-yasnippet)        ;; LSP + snippets
     (company-dabbrev-code company-keywords company-files)
     company-dabbrev))

  ;; dabbrev-like company-mode completion backend.
  company-dabbrev-downcase nil               ;; preserve case
  company-dabbrev-ignore-case nil            ;; case sensitive
  company-dabbrev-other-buffers t)           ;; search other buffers


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
;; TREE-SITTER
;; ============================================================================
(setq +tree-sitter-hl-enabled-modes t)

;; ============================================================================
;; WHICH-KEY
;; ============================================================================
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0.05)

;; ============================================================================
;; JAVA (LSP + JDTLS)
;; ============================================================================
(add-hook 'java-ts-mode-hook #'lsp!)
(add-hook 'java-mode-hook #'lsp!)

(after! lsp-java
  (setq lsp-java-java-path "/usr/lib/jvm/default/bin/java")
  
  (setq lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled nil
        lsp-java-save-actions-organize-imports t 
        lsp-java-completion-import-order ["java" "javax" "org" "com"])

  (setq lsp-java-completion-favorite-static-members
        ["org.junit.Assert.*"
         "org.junit.jupiter.api.Assertions.*"
         "org.mockito.Mockito.*"
         "java.util.Objects.requireNonNull"])
  
  (setq lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t)
  
  ;; Formatter settings
  (setq lsp-java-format-enabled t
        lsp-java-format-comments-enabled t
        lsp-java-format-on-type-enabled nil)

  ;; Use expand-file-name first, then prepend file://
  (let ((formatter-file (expand-file-name "formatter/formatter-java.xml" doom-user-dir)))
    (when (file-exists-p formatter-file)
      (setq lsp-java-format-settings-url (concat "file:///" formatter-file)
            lsp-java-format-settings-profile "Default")))
  
  ;; Profile name MUST match the name="..." in your XML file
  (setq lsp-java-format-settings-profile "Default")

  (setq lsp-java-vmargs
        '("-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Xmx4G"          
          "-Xms1G"
          "-Dsun.zip.disableMemoryMapping=true"))

  (let ((lombok-jar (expand-file-name "~/.local/share/lombok/lombok.jar")))
    (when (file-exists-p lombok-jar)
      (push (concat "-javaagent:" lombok-jar) lsp-java-vmargs))))

;; ============================================================================
;; DAP (Debug Adapter Protocol) - IntelliJ-like Debugging
;; ============================================================================
(after! dap-mode
  (setq dap-java-java-command "/usr/lib/jvm/default/bin/java")
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
  
  (setq dap-auto-show-output t
        dap-output-window-min-height 10
        dap-output-window-max-height 20))

(add-hook! (java-mode java-ts-mode) 
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

;;; config.el ends here
