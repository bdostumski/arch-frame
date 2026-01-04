;;; app-rss-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; RSS feed setup using Elfeed in Doom Emacs.

;;; Code:

(use-package! elfeed
  :defer t
  :init
  ;; Define RSS feed sources
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"       ; Emacs blogs
          "https://www.reddit.com/r/emacs/.rss"        ; Reddit Emacs
          "https://hackernoon.com/feed/rss"            ; Hackernoon articles
          "https://kubernetes.io/feed.xml"             ; Kubernetes blog
          "https://www.docker.com/blog/feed/"          ; Docker blog
          "https://programmingdigest.net/rss"))        ; Programming Digest

  :config
  ;; Ensure database directory exists
  (setq elfeed-db-directory (expand-file-name "elfeed" doom-cache-dir))
  (unless (file-directory-p elfeed-db-directory)
    (make-directory elfeed-db-directory t))

  ;; Display settings
  (setq elfeed-search-title-max-width 100
        elfeed-search-title-min-width 30
        elfeed-search-trailing-width 25
        elfeed-search-date-format '("%Y-%m-%d %H:%M")
        elfeed-search-filter "@1-week-ago +unread"
        elfeed-show-refresh-function #'elfeed-show-refresh--mail-style)

  ;; Browser configuration - use system default or eww
  (setq browse-url-browser-function
        (if (display-graphic-p)
            'browse-url-default-browser
          'eww-browse-url))

  ;; Auto-update configuration
  (defvar elfeed-update-timer nil
    "Timer for auto-updating elfeed feeds.")

  (defun elfeed-setup-auto-update ()
    "Set up automatic feed updates."
    (when elfeed-update-timer
      (cancel-timer elfeed-update-timer))
    (setq elfeed-update-timer
          (run-at-time 300 3600 #'elfeed-update))) ; Update after 5 min, then every hour

  ;; Start auto-update when elfeed is first loaded
  (add-hook 'elfeed-search-mode-hook #'elfeed-setup-auto-update)

  ;; Improve entry display
  (setq elfeed-show-entry-switch 'pop-to-buffer
        elfeed-show-entry-delete #'delete-window)

  ;; Enhanced functionality
  ;; Mark all as read function
  (defun elfeed-mark-all-as-read ()
    "Mark all entries as read."
    (interactive)
    (when (eq major-mode 'elfeed-search-mode)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread)))

  ;; Smart entry opening
  (defun elfeed-show-visit-or-external ()
    "Visit entry in browser or open externally based on content type."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (if (string-match-p "\\(youtube\\|youtu\\.be\\|vimeo\\)" link)
            (browse-url-default-browser link)
          (elfeed-show-visit)))))

  ;; Save entry to org (if org is available)
  (when (modulep! :lang org)
    (defun elfeed-save-entry-to-org ()
      "Save current entry to an org file."
      (interactive)
      (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                        elfeed-show-entry
                      (elfeed-search-selected :ignore-region)))
             (title (elfeed-entry-title entry))
             (url (elfeed-entry-link entry))
             (date (format-time-string "%Y-%m-%d" (elfeed-entry-date entry)))
             (org-file (expand-file-name "rss-articles.org" org-directory)))
        (when entry
          (unless (file-exists-p org-file)
            (with-temp-file org-file
              (insert "#+TITLE: RSS Articles\n\n")))
          (with-temp-buffer
            (insert (format "* [[%s][%s]]\n  :PROPERTIES:\n  :DATE: %s\n  :END:\n\n"
                            url title date))
            (append-to-file (point-min) (point-max) org-file))
          (message "Entry saved to %s" org-file)))))

  ;; Fix potential evil-mode conflicts
  (add-hook 'elfeed-search-mode-hook
            (lambda ()
              (setq-local evil-move-cursor-back nil)))

  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (setq-local evil-move-cursor-back nil))))

;; Org integration (conditional loading)
(use-package! elfeed-org
  :after elfeed
  :when (modulep! :lang org)
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-directory)))
  (elfeed-org))

;; Protocol handler (optional - only load if needed)
(use-package! elfeed-protocol
  :after elfeed
  :when (and (modulep! :app rss +protocol) ; Only if explicitly enabled
             (not (eq system-type 'windows-nt))) ; Skip on Windows due to potential issues
  :config
  (setq elfeed-use-curl t
        elfeed-curl-max-connections 10
        elfeed-protocol-feeds '())
  (elfeed-protocol-enable))

;; Database cleanup function (helps with SQLite issues)
(defun elfeed-reset-database ()
  "Reset the elfeed database."
  (interactive)
  (when (and elfeed-db-directory (file-directory-p elfeed-db-directory))
    (delete-directory elfeed-db-directory t)
    (make-directory elfeed-db-directory t))
  (setq elfeed-db nil)
  (elfeed-db-load)
  (message "Elfeed database reset"))

(provide 'app-rss-config)

;;; app-rss-config.el ends here
