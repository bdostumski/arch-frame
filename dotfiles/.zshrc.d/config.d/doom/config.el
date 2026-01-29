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

   ;; FONTS
   doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'medium)
   doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16)
   doom-serif-font (font-spec :family "Noto Serif" :size 16)
   doom-symbol-font (font-spec :family "Symbols Nerd Font")
   doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one t))

;; Turn on pixel scrolling
(pixel-scroll-precision-mode t)

;; ============================================================================
;; CREATE DIRECTORY IF NOT EXISTS
;; ============================================================================
(defun ensure-dir-exists (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun ensure-file-exists (file)
  (unless (file-exists-p file)
    (with-temp-buffer
      (write-file file))))

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

;; Ensure org/notes/tasks.org file
(ensure-file-exists (expand-file-name "org/notes/tasks.org" doom-user-dir))

;; Ensure org/notes/notes.org file 
(ensure-file-exists (expand-file-name "org/notes/notes.org" doom-user-dir))

;; Ensure backups directory
(let ((backup-dir (expand-file-name "backups/" doom-cache-dir)))
  (ensure-dir-exists backup-dir)
  (setq backup-directory-alist `(("." . ,backup-dir))
        backup-by-copying t       ;; Copy files instead of linking them
        delete-old-versions t     ;; Don't ask before deleting old backups
        kept-new-versions 6       ;; Keep 6 newest versions
        kept-old-versions 2       ;; Keep 2 oldest versions
        version-control t))       ;; Use version numbers for backups

;; ============================================================================
;; PROJECTILE 
;; ============================================================================
(setq 
 projectile-enable-caching t
 projectile-indexing-method 'hybrid
 projectile-project-search-path '("~/Workspace" "~/Documents"))

;; ============================================================================
;; FILE HANDLING
;; ============================================================================
(setq 
 auto-save-default t
 make-backup-files t
 create-lockfiles nil)

;; Assign .log files to text-mode
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode)) 
;;  .env files -> conf-mode
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)) 

;; ============================================================================
;; LINE NUMBERS
;; ============================================================================
(setq display-line-numbers-type 'relative)
(remove-hook! '(text-mode-hook) #'display-line-numbers-mode)

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
  (setq dirvish-attributes '(vc-state subtree-state nerd-icons collapse git-msg file-size)
        dirvish-mode-line-format '(:left (sort file-time " " file-size))
        dirvish-use-mode-line t))

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
(setq gc-cons-threshold (* 50 1000 1000) ;; Set a high threshold of 50MB during startup
      gc-cons-percentage 0.6           ;; Temporarily increase GC frequency during startup
      gcmh-idle-delay 1  ; Trigger GC after being idle for 1 second
      gcmh-high-cons-threshold (* 64 1024 1024)) ; Slightly lower the threshold to 64MB

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; After startup, lower GC threshold
            (setq gc-cons-threshold (* 2 1000 1000) ;; Lower threshold to 2MB after startup
                  gc-cons-percentage 0.1)))

;; Use gcmh-mode to handle garbage collection dynamically
(use-package! gcmh
  :config
  (setq gcmh-idle-delay 2                      ;; Trigger GC after being idle for 5 seconds
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
;; HL LINE
;; ============================================================================
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local global-hl-line-sticky-flag nil)))

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
(setq +format-on-save-enabled-modes
      '(not 
        xml-mode
        nxml-mode
        tex-mode
        latex-mode
        org-mode
        markdown-mode))

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

(setq org-capture-templates
      '(("t" "Task" entry (file+headline (expand-file-name "org/notes/tasks.org" doom-user-dir) "Tasks")
         "* TODO %?\n  %u\n  %a")
        ("n" "Note" entry (file+headline (expand-file-name "org/notes/notes.org" doom-user-dir) "Notes")
         "* %? :NOTE:\n%U\n%a")))

(use-package! org-download
  :after org
  :config
  (setq org-download-image-dir (expand-file-name "org/images" org-directory)
        org-download-screenshot-method "xfce4-screenshooter -r -o"))

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "◆" "▶")
        org-superstar-item-bullet-alist '((?* . ?•)
                                          (?+ . ?➤)
                                          (?- . ?✦))))

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'org-indent-mode)

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
;; ORG-ROAM 
;; ============================================================================
(setq org-roam-directory (file-truename (expand-file-name "org/roam/" doom-user-dir)))

(after! org-roam
  (setq org-roam-db-gc-threshold most-positive-fixnum  
        org-roam-completion-everywhere t))             

(use-package! websocket
  :ensure t ;; Ensure the package is installed
  :after org-roam) ;; Dependency for org-roam-ui

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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
;; VERTICO
;; ============================================================================
(after! vertico
  (setq vertico-count 15                        ;; Maximum numbers of candidates to show
        vertico-resize t                        ;; How to resize the Vertico minibuffer window, see resize-mini-windows.
        vertico-cycle t                         ;; Enable cycling for vertico-next and vertico-previous.
        enable-recursive-minibuffers t))        ;; Non-nil means to allow minibuffer commands while in the minibuffer.

;; ============================================================================
;; VERTICO ADDITIONAL PACKAGES
;; ============================================================================
(use-package! marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)) ;; Cycle annotations
  :init
  (marginalia-mode))

(use-package! embark
  :bind
  (("C-." . embark-act) ;; Invoke actions from the minibuffer
   ("C-;" . embark-dwim))) ;; Act on completion at point

;; Optional: Integrate Consult with Embark actions
(use-package! embark-consult
  :after (consult embark)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; WHICH-KEY
;; ============================================================================
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0.05)

;; ============================================================================
;; COPILOT
;; ============================================================================
;;(use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (:map copilot-completion-map
;;              ("C-<tab>" . copilot-accept-completion)
;;              ("M-]" . copilot-next-completion)
;;              ("M-[" . copilot-previous-completion)))
;;
;; ============================================================================
;; MU4E
;; ============================================================================
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)

;; Mail user agent
(setq mail-user-agent 'mu4e-user-agent)

;; Maildir configuration (Gmail)
(setq mu4e-maildir "~/Maildir")  ;; Adjust to your Maildir location
(setq mu4e-drafts-folder   "/[Gmail].Drafts"
      mu4e-sent-folder     "/[Gmail].Sent Mail"
      mu4e-trash-folder    "/[Gmail].Trash")

;; Gmail IMAP handles sent messages, so do not save copies locally
(setq mu4e-sent-messages-behavior 'delete)

;; Maildir shortcuts & bookmarks
(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX"             :key ?i)
         (:maildir "/[Gmail].Sent Mail" :key ?s)
         (:maildir "/[Gmail].Trash"     :key ?t)
         (:maildir "/[Gmail].All Mail"  :key ?a)))

;; Add a bookmark for Inbox and mark it as favorite (shown in modeline)
(add-to-list 'mu4e-bookmarks
             '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

;; Mail fetching
(setq mu4e-get-mail-command "offlineimap")

;; User identity & signature
(setq user-mail-address "b.dostumski@gmail.com"
      user-full-name  "Borislav Dostumski"
      message-signature
      (concat
       "Borislav Dostumski\n"
       "http://www.github.com/bdostumski\n"))

;; Sending mail via msmtp or smtpmail
(setq sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)

;; Optional SMTP via smtpmail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;         '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Additional options
(setq message-kill-buffer-on-exit t) ;; Automatically kill message buffer after sending

;; Optional authentication sources
;; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; ============================================================================
;; LSP Configuration for Doom Emacs
;; ============================================================================
(use-package! lsp-mode
  :init
  ;; Keyboard shortcut prefix for LSP commands
  (setq lsp-keymap-prefix "C-c l")
  ;; Enable LSP in programming modes
  :hook ((prog-mode . lsp-deferred))
  :config
  (setq lsp-idle-delay 0.5                 ;; Delay for sending data to LSP server
        lsp-log-io nil                     ;; Disable LSP server communication logs
        lsp-response-timeout 30            ;; Timeout for LSP server responses
        lsp-completion-provider :capf))    ;; Use Completion-at-Point framework for suggestions

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t                ;; Show documentation when hovering over symbols
        lsp-ui-doc-delay 0.3               ;; Delay for showing documentation popup
        lsp-ui-sideline-enable t           ;; Enable sideline hints
        lsp-ui-sideline-show-diagnostics t ;; Show diagnostics (e.g., errors/warnings) in sideline
        lsp-ui-sideline-show-hover t       ;; Show hover documentation in sideline
        lsp-ui-sideline-update-mode 'line))

(use-package! company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2             ;; Delay before showing suggestions after typing
        company-minimum-prefix-length 1    ;; Show suggestions after typing 1 character
        company-tooltip-align-annotations t
        company-selection-wrap-around t))

;; Optional: Add icons to completions (requires `all-the-icons`)
(use-package! company-box
  :hook (company-mode . company-box-mode))

;; Purely optional, disable LSP progress spinner (if you find it distracting)
(setq lsp-progress-spinner nil)

;; ============================================================================
;; DAP (Debug Adapter Protocol) - IntelliJ-like Debugging
;; ============================================================================
(after! dap-java
  (setq 
   lsp-java-autobuild-enabled t))
;;
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


;; ============================================================================
;; JAVA (LSP + JDTLS) - FIXED VERSION
;; ============================================================================
(after! lsp-java
  ;; Remove the system path - let Doom handle it
  (setq sp-java-test-additional-args '("--scan-class-path")
        dap-java-vm-args '("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n")
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled t 
        lsp-java-import-gradle-annotation-processing-enabled t
        lsp-java-save-actions-organize-imports t 
        dap-java-test-runner "junit"
        lsp-java-completion-import-order ["java" "javax" "org" "com"]
        lsp-java-workspace-folders-ignore-directories
        '("^\\.idea$" "^\\.metadata$" "^node_modules$" "^\\.git$" "^build$"))

  ;; Rest of your configuration...
  (setq lsp-java-completion-favorite-static-members
        ["org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*"
         "org.junit.jupiter.api.Assumptions.*"
         "org.junit.jupiter.api.DynamicContainer.*"
         "org.junit.jupiter.api.DynamicTest.*" "org.mockito.Mockito.*"
         "org.mockito.ArgumentMatchers.*" "org.mockito.Answers.*" "java.util.Objects.requireNonNull"])

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
    ;; Ensure directory exists
    (unless (file-directory-p java-lib-dir)
      (make-directory java-lib-dir t))

    ;; Base JVM args
    (setq lsp-java-vmargs
          '("-XX:+UseG1GC"
            "-XX:+UseStringDeduplication"
            "-Xmx4G"
            "-Xms1G"
            "-Dsun.zip.disableMemoryMapping=true"))
    ;; Add Lombok javaagent if present

    (when (file-exists-p lombok-jar)
      (push (concat "-javaagent:" lombok-jar) lsp-java-vmargs))))

;;; config.el ends here
