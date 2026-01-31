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
(add-hook 'doom-first-file-hook #'global-git-commit-mode)

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
        version-control t)
  (setq backup-by-copying-when-linked t ;; Preserve links instead of breaking them when saving backups
        buffer-backed-up nil))       ;; Use version numbers for backups

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
(setq gc-cons-threshold (* 10 1000 1000) ;; Set a high threshold of 50MB during startup
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
(defun my-code-setup ()
  "Custom setup for programming modes."
  (hl-line-mode) ;; Highlight current code line
  (hl-todo-mode) ;; Highlight TODO keywords
  (setq-local global-hl-line-sticky-flag nil)) ;; Disable global hl-line

(add-hook 'prog-mode-hook 'my-code-setup)

(global-hl-todo-mode 1)

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
  (setq org-hide-emphasis-markers t              ; Hide *, /, etc. markers
        org-pretty-entities t                    ; Show pretty symbols (e.g., \alpha → α)
        org-image-actual-width '(300)            ; Scale inline images to max 300px width
        org-ellipsis " ▾"                        ; Custom ellipsis symbol for folded sections
        org-startup-folded 'content              ; Show content, fold headers at startup
        org-startup-indented t                   ; Enable clean indented view
        org-agenda-files                         ; Specify agenda files explicitly
        (list (expand-file-name "org/notes/tasks.org" org-directory)
              (expand-file-name "org/notes/projects.org" org-directory)
              (expand-file-name "org/roam/" org-directory))
        org-log-done 'time                       ; Log time when marking TODOs done
        org-log-into-drawer t                    ; Log activity into :LOGBOOK:
        org-agenda-custom-commands
        '(("d" "Daily Agenda"
           ((agenda "" ((org-agenda-span 'day))))
           ((org-agenda-overriding-header "Daily Overview")))
          ("w" "Weekly Overview"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "PROG"
                  ((org-agenda-overriding-header "In Progress")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting On")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Todo")))))))

  (setq org-export-with-smart-quotes t  ;; Use proper typographic quotes
        org-export-with-section-numbers nil ;; Remove section numbers from exported documents
        org-export-headline-levels 4    ;; Restrict to 4-level headers
        org-export-with-toc t))         ;; Include a table of contents                 

(use-package! org-modern
  :hook ((org-mode . org-modern-mode)           ; Enable org-modern in Org Mode
         (org-agenda-finalize . org-modern-agenda)) ; Enable for agenda, if used
  :config
  (setq org-modern-table-vertical 2             ; More vertical spacing in tables
        org-modern-table-horizontal 1           ; Slightly more horizontal spacing
        org-modern-todo-faces
        '(("TODO"  . (:foreground "#EC5f67" :weight bold :box t))
          ("PROG"  . (:foreground "#FABD2F" :weight bold :box t))
          ("DONE"  . (:foreground "#8EC07C" :weight bold :box t))
          ("WAIT"  . (:foreground "#D3869B" :weight bold :box t)))
        org-modern-block t                      ; Modern blocks (e.g., src, quote blocks)
        org-modern-block-fringe t               ; Adds fringe styling
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))           ; Custom bullets
        org-modern-horizontal "──────────────────────────────────────"
        ))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)       ; Enable org-appear in Org Mode
  :config
  (setq org-appear-autoemphasis t          ; Show *bold*, /italic/, etc. when cursor is near
        org-appear-autolinks t             ; Show [[links]] when cursor is over them
        org-appear-autosubmarkers t        ; Show _subscript_ and ^superscript^ markers
        org-appear-autokeywords t          ; Show #+KEYWORDS dynamically when near the cursor
        org-appear-delay 0.3))             ; Add delay before showing elements (optional)

(setq org-tasks-file (expand-file-name "org/notes/tasks.org" doom-user-dir))
(setq org-notes-file (expand-file-name "org/notes/notes.org" doom-user-dir))
(setq org-journal-file (expand-file-name "org/notes/journal.org" doom-user-dir))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-tasks-file "Tasks")
         "* TODO %?\n  SCHEDULED: %^t\n  %u\n  %a")
        ("n" "Note" entry (file+headline org-notes-file "Notes")
         "* %? :NOTE:\n%U\n%a")
        ("p" "Project" entry (file+headline (expand-file-name "org/notes/projects.org" doom-user-dir) "Projects")
         "* %^{Project Name}\n  %U\n  %a")
        ("j" "Journal Entry" entry (file+datetree org-journal-file)
         "* %U\n%?\n")))

(use-package! org-download
  :after org
  :config
  (setq org-download-image-dir nil               ; Use dynamic image directories per Org file
        org-download-method                      ; Save images to file-specific folder
        (lambda (filename)
          (let ((dir (concat (file-name-sans-extension (buffer-file-name))
                             "-images/")))        ; Saves under `file-name-images/`
            (unless (file-exists-p dir) (make-directory dir t))
            (expand-file-name (file-name-nondirectory filename) dir)))
        org-download-screenshot-method "xfce4-screenshooter -r -o" ; XFCE screenshot tool
        org-image-actual-width '(300)           ; Default max image width for inline display
        org-download-heading-lvl nil)           ; Don't group images under headers in headlines

  ;; Enable drag-and-drop in Dired Mode
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode) ;; Automatically enable org-superstar-mode in org-mode
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "◆" "▶") ; Prettier headline bullets
        org-superstar-item-bullet-alist '((?* . ?•)                ; Replace list item bullets
                                          (?+ . ?➤)
                                          (?- . ?✦))
        org-superstar-leading-bullet "    "                        ; Adds indentation for nested lists
        org-ellipsis " ⬎"))                                        ; Matches bullet style for folded headlines

;; Enable word wrap and visual indentation in Org Mode for readability
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)  ; Enable word wrap for long lines
            (org-indent-mode 1))) ; Align content with heading levels

;; ============================================================================
;; DEFT
;; ============================================================================
(setq deft-recursive t                            ;; Search notes in subdirectories
      deft-use-filename-as-title t                ;; Use filenames as note titles in deft browser
      deft-use-filter-string-for-filename t       ;; Clean filenames from unnecessary characters
      deft-file-naming-rules '((noslash . "-")    ;; Replace slashes with hyphens
                               (nospace . "-")   ;; Replace spaces with hyphens
                               (case-fn . downcase)) ; Convert filenames to lowercase
      deft-default-extension '("org" "txt" "text" "tex" "md" "markdown") ; Extensions to search
      deft-directory (file-truename (expand-file-name "org/notes/" doom-user-dir)) ; Notes home
      deft-recursive-ignore-dir-regexp "^archive$"          ; Ignore "archive" folder
      deft-recursive-ignore-file-regexp "\\(?:^\\|/\\)\\."  ; Ignore dotfiles
      deft-auto-save-interval 0.0                           ; Save changes immediately
      deft-strip-summary-regexp "\\([\n\r]\\|^\*+\\|^#\\+[A-Z_]+:.*$\\)" ; Ignore metadata and headlines
      deft-strip-title-regexp "\\`#\\+title: \\|-\\*-.*-\\*-" ; Display title cleanly in Deft browser
      deft-open-file-other-window t                         ; Open files in vertical split
      deft-new-file-format "#+TITLE: %s\n\n")               ; Template for new files

;; ============================================================================
;; ORG-ROAM 
;; ============================================================================
;; Define the main Org-roam directory
(setq org-roam-directory (file-truename (expand-file-name "org/roam/" doom-user-dir)))

;; (Optional) Explicitly set the database location
(setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))

;; Ensure the directory exists
(unless (file-directory-p org-roam-directory)
  (make-directory org-roam-directory t))

(after! org-roam
  (setq org-roam-db-gc-threshold 100000000               ; Database GC threshold (100MB)
        org-roam-completion-everywhere t                 ; Enable completions everywhere
        org-roam-db-update-method 'immediate             ; Update DB after every file change
        org-roam-node-display-template "${title:*} (${tags:10})" ; Display title and tags in completion
        org-roam-dailies-directory "daily/"              ; Directory for daily notes
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n#+created: %u\n#+filetags: ${slug}\n")
           :unnarrowed t)
          ("p" "project" plain "%?"
           :target (file+head "projects/${slug}.org"
                              "#+title: ${title}\n#+category: ${slug}\n#+created: %u\n#+filetags: :project:\n")
           :unnarrowed t))))

(use-package! websocket
  :after org-roam) ;; Loaded after org-roam for org-roam-ui

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t                  ;; Sync with your Emacs theme
        org-roam-ui-follow t                     ;; Automatically follow current file in UI
        org-roam-ui-update-on-save t             ;; Update graph on save
        org-roam-ui-open-on-start nil            ;; Disable auto-opening on Emacs start
        org-roam-ui-port 35999                   ;; Set a custom port for the web interface
        org-roam-ui-default-zoom 1.0             ;; Default zoom level in the graph
        org-roam-ui-default-prefs '((:collapse-metadata t) ;; Collapse metadata for clarity
                                    (:collapse-right-sidebar t)
                                    (:show-tags t)))        ;; Show tags on graph display

  ;; Optional keybinding for quick toggle of org-roam-ui
  (defun toggle-org-roam-ui ()
    "Toggle the org-roam-ui server on or off."
    (interactive)
    (if (get-process "org-roam-ui")
        (kill-process "org-roam-ui")            ;; Close server if running
      (org-roam-ui-mode)))                     ;; Start server if not running
  (global-set-key (kbd "C-c n u") #'toggle-org-roam-ui)) ;; Custom keybinding for toggling

;;; Org-Babel Configuration
(after! org
  ;; Load languages for Org-Babel
  (setq org-babel-load-languages
        '((emacs-lisp . t)  ;; Enable Emacs Lisp (default)
          (python . t)      ;; Enable Python
          (shell . t)       ;; Enable Shell scripting
          (java . t)        ;; Enable Java
          (js . t)          ;; Enable JavaScript
          (sql . t)         ;; Enable SQL
          (restclient . t)  ;; Enable REST Client
          ))

  ;; Disable confirmation prompt when executing code blocks
  (setq org-confirm-babel-evaluate nil)  ;; Set to 'y-or-n-p' to confirm

  ;; Prevent large results from choking your Org buffer
  (setq org-babel-max-tangle-lines 1000) ;; Limit large tangled files

  (setq org-babel-python-command "python3") ;; Specify Python3 explicitly
  
  ;; Display inline images by default after executing code blocks
  (setq org-startup-with-inline-images t
        org-babel-results-keyword "results")  ;; Customize the keyword for result blocks
  
  ;; Automatically display inline images after executing a block
  (add-hook 'org-babel-after-execute-hook
            #'org-display-inline-images 'append)

  ;; Customize the behavior of code blocks
  (setq org-babel-default-header-args:sh
        '((:results . "output replace")   ;; Show and replace output
          (:exports . "both")))           ;; Export results and code when exporting

  ;; Set source block delimiter style (default is #+BEGIN_SRC and #+END_SRC)
  (setq org-babel-tangle-lang-exts '(("shell" . "sh")
                                     ("python" . "py")
                                     ("emacs-lisp" . "el"))))  ;; For tangling scripts to files

;;; Install optional dependencies for certain languages
(use-package! ob-python
  :after org)  ;; Enable Org Mode support for Python
(use-package! ob-shell
  :after org)  ;; Enable Org Mode support for Shell scripting
(use-package! ob-java
  :after org)  ;; Enable Org Mode support for Java
(use-package! ob-js
  :after org)  ;; Enable Org Mode support for JavaScript

;; ============================================================================
;; CALENDAR
;; ============================================================================
(after! calendar
  (setq calendar-date-style 'iso ; Use ISO date format (YYYY-MM-DD) for consistency
        calendar-mark-holidays-flag t ; Highlight holidays for better visibility
        calendar-week-start-day 1 ; Start the week on Monday
        calendar-weekend-days '(0 6) ; Mark Saturday and Sunday as weekends
        calendar-weekend-marker 'holiday ; Highlight weekends using holiday style

        ;; Define Bulgarian holidays
        holiday-bulgarian-holidays '((holiday-fixed 1 1 "New Year's Day")) ; Add Bulgarian holidays

        ;; Add Bulgarian holidays to the calendar-holidays list
        calendar-holidays (append holiday-other-holidays holiday-bulgarian-holidays)

        ;; Display ISO week numbers in the calendar
        calendar-intermonth-text
        '(propertize
          (format "%2d" (car (calendar-iso-from-absolute
                              (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))

  ;; Highlight today's date in the calendar.
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  ;; Set the current day to use a distinct highlight.
  (setq calendar-today-marker 'highlight))

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

;; Suppress warn messages
(after! lsp-mode
  (setq lsp-warn-no-matched-clients nil))

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
;; EJC DATABASE CONFIGURATION
;; ============================================================================
(use-package! ejc-sql
  :init
  ;; Add database drivers path
  (setq ejc-jdbc-driver-path (expand-file-name "~/.emacs.d/.local/jars/")) ;; Update with your jar files location
  (setq ejc-jdbc-default-driver "postgresql") ;; Set default DB driver
  (setq ejc-jdbc-default-url "jdbc:postgresql://localhost/my_database") ;; Default connection string if needed
  
  :config
  (setq ejc-sql-default-connection-schema "public") ;; Default schema (PostgreSQL/Oracle/etc.)
  (setq ejc-sql-use-metadata-server t)             ;; Use metadata server for autocompletion

  ;; Enable company mode support
  (use-package! company
    :config
    (require 'ejc-company)
    (add-to-list 'company-backends 'ejc-company-backend)) ;; Add EJC backend to company-mode

  ;; Enable inline results
  (require 'ejc-sql)
  ;; PostgreSQL Connection
  (ejc-create-connection
   "my-postgres-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "postgresql-42.5.0.jar")
   :classname "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname "//localhost:5432/my_database"
   :user "user"
   :password "password")

  ;; MySQL Connection
  (ejc-create-connection
   "my-mysql-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "mysql-connector-java-8.0.33.jar")
   :classname "com.mysql.cj.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//127.0.0.1:3306/my_mysql_database"
   :user "user"
   :password "password")

  ;; MariaDB (MySQL Fork)
  (ejc-create-connection
   "mariadb-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "mariadb-java-client-3.0.8.jar") ;; MariaDB JDBC driver
   :classname "org.mariadb.jdbc.Driver"
   :subprotocol "mariadb"
   :subname "//127.0.0.1:3306/your_database"
   :user "user"
   :password "password")

  ;; SQLite
  (ejc-create-connection
   "sqlite-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "sqlite-jdbc-3.36.0.3.jar")
   :classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname (expand-file-name "~/path/to/your_database.sqlite"))

  ;; Oracle
  (ejc-create-connection
   "oracle-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "ojdbc8.jar") ;; Note: Oracle restricts its downloads
   :classname "oracle.jdbc.driver.OracleDriver"
   :subprotocol "oracle:thin"
   :subname "@//localhost:1521/your_service_name"
   :user "user"
   :password "password")

  ;; Microsoft SQL Server
  (ejc-create-connection
   "sqlserver-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "mssql-jdbc-9.4.1.jre8.jar") ;; Microsoft SQL Server JDBC driver
   :classname "com.microsoft.sqlserver.jdbc.SQLServerDriver"
   :subprotocol "sqlserver"
   :subname "//localhost:1433;databaseName=your_database"
   :user "user"
   :password "password")

  ;; H2 Database (In-Memory or File-Based)
  (ejc-create-connection
   "h2-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "h2-2.1.214.jar")
   :classname "org.h2.Driver"
   :subprotocol "h2"
   :subname "~/your_database_file.h2.db" ;; For file-based H2 database
   :user "sa"
   :password "")

  ;; Amazon RDS / Aurora PostgreSQL
  (ejc-create-connection
   "rds-postgres-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "postgresql-42.5.0.jar") ;; PostgreSQL JAR
   :classname "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname "//your-rds-endpoint.amazonaws.com:5432/your_database"
   :user "your_username"
   :password "your_password")

  ;; MariaDB or MySQL in Google Cloud SQL
  (ejc-create-connection
   "google-mysql-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "mysql-connector-java-8.0.33.jar")
   :classname "com.mysql.cj.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//google-cloud-sql-ip-address:3306/your_database?cloudSqlInstance=your-cloudsql-instance&socketFactory=com.google.cloud.sql.mysql.SocketFactory"
   :user "db_user"
   :password "db_password")

  ;; MariaDB or MySQL in Azure Database (MySQL Server)
  (ejc-create-connection
   "azure-mysql-database"
   :classpath (concat (file-name-as-directory ejc-jdbc-driver-path) "mysql-connector-java-8.0.33.jar")
   :classname "com.mysql.cj.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//<server-name>.mysql.database.azure.com:3306/your_database"
   :user "username@<server-name>"
   :password "db_password")
  
  ;; Optional keybinding: Connect and execute queries
  (after! ejc-sql
    (map! :map sql-mode-map
          :n "SPC e c" #'ejc-connect
          :n "SPC e e" #'ejc-eval-user-sql-at-point))

  (setq sql-indent-offset 2) ;; Indentation depth

  ;; Enable SQL package
  (add-hook 'sql-mode-hook
            (lambda ()
              ;; Enable ejc-sql
              (ejc-sql-mode 1)
              ;; Display inline query results
              (ejc-set-inline-result t)
              (company-mode t)))) ;; Make sure company-mode is active

;;; PostgreSQL Connection
(defun ejc-connect-postgres ()
  "Connect to the PostgreSQL database."
  (interactive)
  (ejc-connect "my-postgres-database"))

;;; MySQL Connection
(defun ejc-connect-mysql ()
  "Connect to the MySQL database."
  (interactive)
  (ejc-connect "my-mysql-database"))

;;; MariaDB Connection
(defun ejc-connect-mariadb ()
  "Connect to the MariaDB database."
  (interactive)
  (ejc-connect "mariadb-database"))

;;; SQLite Connection
(defun ejc-connect-sqlite ()
  "Connect to the SQLite database."
  (interactive)
  (ejc-connect "sqlite-database"))

;;; Oracle Connection
(defun ejc-connect-oracle ()
  "Connect to the Oracle database."
  (interactive)
  (ejc-connect "oracle-database"))

;;; SQL Server Connection
(defun ejc-connect-sqlserver ()
  "Connect to the Microsoft SQL Server database."
  (interactive)
  (ejc-connect "sqlserver-database"))

;;; H2 Database Connection
(defun ejc-connect-h2 ()
  "Connect to the H2 database."
  (interactive)
  (ejc-connect "h2-database"))

;;; Amazon RDS PostgreSQL Connection
(defun ejc-connect-rds-postgres ()
  "Connect to the Amazon RDS PostgreSQL database."
  (interactive)
  (ejc-connect "rds-postgres-database"))

;;; Google Cloud SQL MySQL Connection
(defun ejc-connect-google-mysql ()
  "Connect to the Google Cloud SQL MySQL database."
  (interactive)
  (ejc-connect "google-mysql-database"))

;;; Azure MySQL Connection
(defun ejc-connect-azure-mysql ()
  "Connect to the Azure MySQL database."
  (interactive)
  (ejc-connect "azure-mysql-database"))

(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist '(("+" . ?✚) ("--" . ?—)))
            (prettify-symbols-mode)))

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
        ["org.junit.Assert.*"
         "org.junit.Assume.*"
         "org.junit.jupiter.api.Assertions.*"
         "org.junit.jupiter.api.Assumptions.*"
         "org.junit.jupiter.api.DynamicContainer.*"
         "org.junit.jupiter.api.DynamicTest.*"
         "org.mockito.Mockito.*"
         "org.mockito.ArgumentMatchers.*"
         "org.mockito.Answers.*"
         "java.util.Objects.requireNonNull"])

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

    (when (file-exists-p lombok-jar)
      (push (concat "-javaagent:" lombok-jar) lsp-java-vmargs))))

;;; config.el ends here
