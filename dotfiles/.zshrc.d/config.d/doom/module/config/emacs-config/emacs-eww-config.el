;;; module/config/emacs-config/emacs-eww-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive EWW configuration for Doom Emacs with enhanced browsing experience.

;;; Code:

;;; Basic EWW Settings
;; Set default search engine to Google
(setq eww-search-prefix "https://duckduckgo.com/?q=")

;; Enable images in EWW
(setq shr-inhibit-images nil)

;; Maximum image width and height
(setq shr-max-image-proportion 0.6)
(setq shr-max-width 120)

;; Download directory for EWW
(setq eww-download-directory "~/Downloads/")

;; History size
(setq eww-history-limit 50)

;; Auto-rename buffer to page title
(setq eww-auto-rename-buffer 'title)

;;; Visual Improvements
;; Improve readability: slightly larger font with JetBrains Mono
(custom-set-faces!
  '(shr-face :height 1.1 :family "JetBrains Mono")
  '(eww-form-submit :background "#4CAF50" :foreground "white" :weight bold)
  '(eww-form-checkbox :background "#2196F3" :foreground "white")
  '(eww-form-select :background "#FF9800" :foreground "black")
  '(shr-link :foreground "#2196F3" :underline t)
  '(shr-h1 :height 1.8 :weight bold :foreground "#E91E63")
  '(shr-h2 :height 1.6 :weight bold :foreground "#9C27B0")
  '(shr-h3 :height 1.4 :weight bold :foreground "#673AB7")
  '(shr-h4 :height 1.2 :weight bold :foreground "#3F51B5")
  '(shr-h5 :height 1.1 :weight bold :foreground "#2196F3")
  '(shr-h6 :height 1.0 :weight bold :foreground "#009688"))

;;; Color scheme adjustments for better readability
(setq shr-color-visible-luminance-min 75)
(setq shr-color-visible-distance-min 5)

;;; Enhanced Bookmarks
(defvar eww-bookmarks-file (expand-file-name "eww-bookmarks" doom-user-dir)
  "File to store EWW bookmarks.")

(setq eww-bookmarks-directory doom-user-dir)

;;; Custom EWW Functions
(defun +eww/eww-browse-url-new-buffer (url)
  "Open URL in a new EWW buffer."
  (interactive "sURL: ")
  (let ((eww-buffer (generate-new-buffer "*eww*")))
    (with-current-buffer eww-buffer
      (eww-mode)
      (eww url))))

(defun +eww/eww-search-duck-duck-go (query)
  "Search QUERY using DuckDuckGo."
  (interactive "sSearch DuckDuckGo: ")
  (eww (concat "https://duckduckgo.com/?q=" (url-encode-url query))))

(defun +eww/eww-search-github (query)
  "Search QUERY on GitHub."
  (interactive "sSearch GitHub: ")
  (eww (concat "https://github.com/search?q=" (url-encode-url query))))

(defun +eww/eww-search-stack-overflow (query)
  "Search QUERY on Stack Overflow."
  (interactive "sSearch Stack Overflow: ")
  (eww (concat "https://stackoverflow.com/search?q=" (url-encode-url query))))

(defun +eww/eww-toggle-images ()
  "Toggle image display in EWW."
  (interactive)
  (setq shr-inhibit-images (not shr-inhibit-images))
  (eww-reload)
  (message "Images %s" (if shr-inhibit-images "disabled" "enabled")))

(defun +eww/eww-copy-page-url ()
  "Copy current page URL to clipboard."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (kill-new (eww-current-url))
    (message "URL copied: %s" (eww-current-url))))

(defun +eww/eww-browse-with-external-browser ()
  "Open current page in external browser."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (browse-url-default-browser (eww-current-url))))

(defun +eww/eww-readable-mode ()
  "Toggle readable mode for better text reading."
  (interactive)
  (if (bound-and-true-p eww-readable-mode)
      (progn
        (setq-local shr-width 80)
        (setq-local eww-readable-mode nil)
        (message "Readable mode disabled"))
    (progn
      (setq-local shr-width 60)
      (setq-local eww-readable-mode t)
      (message "Readable mode enabled"))
    (eww-reload)))

;;; EWW Hooks
(defun +eww/eww-mode-setup ()
  "Setup function for EWW mode."
  ;; Enable word wrap
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)

  ;; Better scrolling
  (setq-local scroll-margin 3)
  (setq-local scroll-conservatively 101)

  ;; Enable line numbers for debugging
  (display-line-numbers-mode -1)

  ;; Auto-save history
  (add-hook 'kill-buffer-hook #'eww-save-history nil t))

(add-hook 'eww-mode-hook #'+eww/eww-mode-setup)

;;; Advanced Search Engines
(defvar +eww/eww-search-engines
  '(("google" . "https://www.google.com/search?q=")
    ("duckduckgo" . "https://duckduckgo.com/?q=")
    ("bing" . "https://www.bing.com/search?q=")
    ("github" . "https://github.com/search?q=")
    ("stackoverflow" . "https://stackoverflow.com/search?q=")
    ("wikipedia" . "https://en.wikipedia.org/wiki/Special:Search?search=")
    ("reddit" . "https://www.reddit.com/search/?q=")
    ("youtube" . "https://www.youtube.com/results?search_query="))
  "List of search engines for EWW.")

(defun +eww/eww-search-engine (engine query)
  "Search QUERY using specified search ENGINE."
  (interactive
   (list (completing-read "Search engine: "
                          (mapcar #'car +eww/eww-search-engines))
         (read-string "Query: ")))
  (let ((url (cdr (assoc engine +eww/eww-search-engines))))
    (if url
        (eww (concat url (url-encode-url query)))
      (message "Unknown search engine: %s" engine))))

;;; EWW link numbering (requires eww-lnum package)
(after! eww-lnum
  (setq eww-lnum-quick-browsing 'quick))

;;; Integration with other packages
(after! which-key
  (which-key-add-major-mode-key-based-replacements 'eww-mode
    "SPC o" "open"))

;;; Performance optimizations
;;(setq eww-retrieve-command '("curl" "--silent" "--compressed"))

;;; Cookie management
(setq url-cookie-file (expand-file-name "cookies" doom-user-dir))
(setq url-cookie-confirmation nil)
(setq url-cookie-save-interval 3600)

;;; Proxy settings (uncomment and configure if needed)
;; (setq url-proxy-services
;;       '(("http" . "proxy.example.com:8080")
;;         ("https" . "proxy.example.com:8080")))

;;; User Agent customization
(setq url-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")

;;; Auto-completion for URLs
(defun +eww/eww-complete-url ()
  "Provide URL completion from history and bookmarks."
  (append (mapcar #'car eww-history)
          (mapcar #'cdr (eww-read-bookmarks))))

;;; Restore session functionality
(defvar +eww/eww-session-file (expand-file-name "eww-session.el" doom-user-dir)
  "File to store EWW session.")

(defun +eww/eww-save-session ()
  "Save current EWW session."
  (interactive)
  (with-temp-file +eww/eww-session-file
    (prin1 (mapcar (lambda (buf)
                     (with-current-buffer buf
                       (when (derived-mode-p 'eww-mode)
                         (eww-current-url))))
                   (buffer-list))
           (current-buffer)))
  (message "EWW session saved"))

(defun +eww/eww-restore-session ()
  "Restore EWW session."
  (interactive)
  (when (file-exists-p +eww/eww-session-file)
    (let ((urls (with-temp-buffer
                  (insert-file-contents +eww/eww-session-file)
                  (read (current-buffer)))))
      (dolist (url urls)
        (when url
          (+eww/eww-browse-url-new-buffer url))))
    (message "EWW session restored")))

;;; Automatically save session on Emacs exit
(add-hook 'kill-emacs-hook #'+eww/eww-save-session)

;;; Custom startup page
(defvar +eww/eww-start-page "https://duckduckgo.com"
  "Default start page for EWW.")

(defun +eww/eww-start ()
  "Start EWW with default page."
  (interactive)
  (eww +eww/eww-start-page))

;;; PDF and media handling
(setq eww-use-external-browser-for-content-type
      "\\`\\(video/\\|audio/\\|application/pdf\\)")

;;; Form improvements
(setq eww-form-checkbox-selected-symbol "☑")
(setq eww-form-checkbox-symbol "☐")

(provide 'emacs-eww-config)

;;; emacs-eww-config.el ends here
