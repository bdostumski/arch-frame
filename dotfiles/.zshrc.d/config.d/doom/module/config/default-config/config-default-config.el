;;; module/config/default-config/config-default-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive default configuration for Doom Emacs with enhanced editing,
;; navigation, and productivity features. Fixed compatibility issues and
;; removed problematic variable references.

;;; Code:

;; ----------------------------
;; Performance Optimizations
;; ----------------------------

;; Startup performance - use safe GC threshold
(setq +default-config-gc-cons-threshold (* 100 1024 1024))  ; 100MB during startup

;; File I/O optimizations
(setq read-process-output-max (* 4 1024 1024)  ; 4MB
      process-adaptive-read-buffering nil)

;; ----------------------------
;; Enhanced Editor Defaults
;; ----------------------------

(setq-default
 ;; Indentation and formatting
 tab-width 4
 indent-tabs-mode nil
 fill-column 88  ; Slightly wider for modern screens

 ;; Scrolling behavior
 scroll-margin 5
 scroll-conservatively 10000
 scroll-preserve-screen-position t
 scroll-step 1

 ;; Advanced editing behavior
 require-final-newline t
 sentence-end-double-space nil
 truncate-lines nil
 word-wrap t
 truncate-partial-width-windows nil

 ;; Performance for large files
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t)

;; Better defaults for various modes
(setq
 ;; Search behavior
 search-highlight t
 search-whitespace-regexp ".*?"
 isearch-lax-whitespace t
 isearch-regexp-lax-whitespace nil
 isearch-lazy-highlight t
 isearch-lazy-count t
 lazy-count-prefix-format "(%s/%s) "
 lazy-count-suffix-format nil

 ;; Case sensitivity
 case-fold-search t
 case-replace t

 ;; Mouse and scrolling
 mouse-wheel-scroll-amount '(3 ((shift) . hscroll))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t

 ;; Window management
 split-height-threshold nil
 split-width-threshold 120
 window-combination-resize t

 ;; Cursor behavior
 cursor-in-non-selected-windows 'hollow
 highlight-nonselected-windows nil

 ;; Auto-save and backup (enhance Doom's settings)
 auto-save-visited-interval 30
 auto-save-timeout 30
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 5
 version-control t)

;; ----------------------------
;; Advanced Smartparens Configuration
;; ----------------------------

(after! smartparens
  ;; Enhanced smartparens settings
  (setq sp-base-key-bindings 'sp
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil
        sp-show-pair-delay 0.1
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  ;; Fix problematic quote pairing in Lisp modes
  (dolist (mode '(emacs-lisp-mode lisp-mode scheme-mode racket-mode clojure-mode))
    (sp-local-pair mode "'" nil :actions :rem)
    (sp-local-pair mode "`" nil :actions :rem))

  ;; Enhanced language-specific pairs
  (sp-with-modes '(markdown-mode gfm-mode org-mode)
    (sp-local-pair "*" "*" :wrap "C-*")
    (sp-local-pair "**" "**" :wrap "C-8")
    (sp-local-pair "_" "_" :wrap "C-_")
    (sp-local-pair "=" "=" :wrap "C-=")
    (sp-local-pair "~" "~" :wrap "C-~"))

  (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
    (sp-local-pair "$" "$")
    (sp-local-pair "\\(" "\\)")
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "\\{" "\\}"))

  (sp-with-modes '(html-mode sgml-mode web-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<!--" "-->"))

  (sp-with-modes '(minibuffer-inactive-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  ;; Custom pair for strings in programming modes
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "\"" "\"" :unless '(sp-in-comment-p sp-in-string-quotes-p))))

;; ----------------------------
;; Enhanced Text Objects & Selection
;; ----------------------------

(after! expand-region
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"
        expand-region-subword-enabled t))

;; Better rectangle selection
(after! rect
  (setq rectangle-mark-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [remap self-insert-command] 'string-rectangle)
          (define-key map "\C-g" 'rectangle-mark-mode)
          map)))

;; ----------------------------
;; Advanced File Management
;; ----------------------------

;; Enhanced uniquify settings
(after! uniquify
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Better recentf configuration
(after! recentf
  (setq recentf-max-saved-items 500
        recentf-auto-cleanup 600
        recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                          "^/tmp/" "^/var/folders/" "^/ssh:" "/\\.git/")))

;; Auto-revert configuration
(setq auto-revert-verbose nil
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil
      revert-without-query '(".*"))

;; ----------------------------
;; Enhanced Navigation & Search
;; ----------------------------

;; Better search ring
(setq search-ring-max 200
      regexp-search-ring-max 200)

;; Enhanced occur mode
(after! replace
  (setq list-matching-lines-default-context-lines 2))

;; ----------------------------
;; Productivity Enhancements
;; ----------------------------

;; Enhanced abbrev mode
(setq abbrev-file-name (expand-file-name "abbrev_defs" doom-private-dir)
      save-abbrevs 'silently)

;; Better bookmark configuration
(after! bookmark
  (setq bookmark-default-file (expand-file-name "bookmarks" doom-private-dir)
        bookmark-save-flag 1
        bookmark-version-control t
        bookmark-sort-flag nil))

;; Enhanced completion
(after! company
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-minimum-prefix-length 2
        company-idle-delay 0.2))

;; ----------------------------
;; Advanced Custom Functions
;; ----------------------------

(defun +default-config/smart-kill-whole-line (&optional arg)
  "Kill whole line and move to next line intelligently."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (kill-line)
    (kill-whole-line arg)))

(defun +default-config/smart-backward-kill-word ()
  "Kill word backward, but respect subwords and symbols."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (subword-backward-kill 1)
    (backward-kill-word 1)))

(defun +default-config/duplicate-line-or-region (&optional n)
  "Duplicate current line or region N times (default 1)."
  (interactive "p")
  (let ((n (or n 1)))
    (if (region-active-p)
        (let ((text (buffer-substring (region-beginning) (region-end))))
          (goto-char (region-end))
          (dotimes (_ n)
            (insert text)))
      (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
        (end-of-line)
        (dotimes (_ n)
          (newline)
          (insert text))))))

(defun +default-config/move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun +default-config/move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun +default-config/smart-open-line ()
  "Open line above or below based on context."
  (interactive)
  (if (and (eolp) (not (bolp)))
      (progn (end-of-line) (newline) (indent-according-to-mode))
    (progn (beginning-of-line) (newline) (forward-line -1) (indent-according-to-mode))))

(defun +default-config/cleanup-buffer-or-region ()
  "Cleanup buffer or region if active."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (untabify (region-beginning) (region-end))
          (delete-trailing-whitespace (region-beginning) (region-end))
          (indent-region (region-beginning) (region-end)))
      (progn
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace)
        (indent-region (point-min) (point-max))))))

(defun +default-config/insert-date ()
  "Insert current date in ISO format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun +default-config/insert-timestamp ()
  "Insert current timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun +default-config/copy-file-path ()
  "Copy current file path to clipboard."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "File path copied: %s" buffer-file-name)))

(defun +default-config/toggle-window-split ()
  "Toggle between horizontal and vertical window splits."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; ----------------------------
;; Mode-Specific Enhancements
;; ----------------------------

;; Programming mode enhancements
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (subword-mode +1)
            (electric-pair-local-mode +1)))

;; Text mode enhancements
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode +1)
            (visual-line-mode +1)))

;; Org mode enhancements
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode +1)
            (org-indent-mode +1)
            (visual-line-mode +1)))

;; Shell script mode
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

;; YAML mode
(after! yaml-mode
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq tab-width 2))))

;; JSON mode
(after! json-mode
  (add-hook 'json-mode-hook
            (lambda ()
              (setq js-indent-level 2
                    json-reformat:indent-width 2))))

;; ----------------------------
;; Display and UI Fixes
;; ----------------------------

;; Ensure compatibility with Doom's display systems
(add-hook 'doom-load-theme-hook
          (lambda ()
            (set-face-attribute 'show-paren-match nil
                                :weight 'bold
                                :underline t)
            (set-face-attribute 'show-paren-mismatch nil
                                :weight 'bold
                                :underline '(:color "red" :style wave))))

;; Fix potential display issues
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (bound-and-true-p display-line-numbers-mode)
              (display-line-numbers-update-width))))

;; ----------------------------
;; Safe Performance Restoration
;; ----------------------------

;; Restore GC threshold after initialization - safe version
(add-hook! 'doom-after-init-hook
  (run-with-idle-timer
   2 nil
   (lambda ()
     ;; Use a safe fallback if doom-gc-cons-threshold is not available
     (setq gc-cons-threshold (if (boundp 'doom-gc-cons-threshold)
                                 doom-gc-cons-threshold
                               (* 16 1024 1024)))  ; 16MB fallback
     (message "Enhanced configuration loaded successfully"))))

(provide 'config-default-config)

;;; config-default-config.el ends here
