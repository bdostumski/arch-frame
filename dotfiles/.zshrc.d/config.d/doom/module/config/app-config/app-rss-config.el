;;; app-rss-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full-featured RSS feed setup using Elfeed in Doom Emacs.
;; - Defines feed sources
;; - Enables Org integration for saving articles
;; - Sets up auto-update and display tweaks
;; - Provides convenient leader keybindings for common actions

;;; Code:

(use-package! elfeed
  :defer t
  :init
  ;; Define RSS feed sources
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://www.reddit.com/r/emacs/.rss"))
  :config
  ;; Display settings
  (setq elfeed-search-title-max-width 80
        elfeed-search-title-min-width 40
        elfeed-search-date-format '("%Y-%m-%d %H:%M")
        elfeed-search-filter "@1-week-ago +unread")

  ;; Enable auto-update every hour
  (run-at-time nil (* 60 60) #'elfeed-update)

  ;; Enable Org integration for saving articles
  (use-package! elfeed-org
    :after elfeed
    :config
    (setq rmh-elfeed-org-files '("~/Documents/org/feeds.org")))

  ;; Enable protocol handler for browser integration
  (use-package! elfeed-protocol
    :after elfeed
    :config
    (elfeed-set-max-connections 8)
    (elfeed-protocol-enable))

  ;; Open links in eww by default, or use browse-url if you prefer
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq browse-url-browser-function 'eww-browse-url)
  )

;; ----------------------------
;; Leader keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor") 
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("r" . "rss")
                                 :desc "Open RSS reader" "o" #'elfeed
                                 :desc "Update feeds"    "u" #'elfeed-update
                                 :desc "Search feeds"    "s" #'elfeed-search-set-filter
                                 :desc "Show entry"      "e" #'elfeed-show-entry
                                 :desc "Mark as read"    "m" #'elfeed-search-untag-all-unread
                                 :desc "Save entry to Org" "S" #'elfeed-org-save-entry))))

(provide 'app-rss-config)

;;; app-rss-config.el ends here
