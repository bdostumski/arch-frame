;;; config/app-config/app-rss-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic RSS feed setup using Elfeed in Doom Emacs.
;; - Defines feed sources
;; - Enables capturing to Org
;; - Provides convenient leader keybindings for common actions

;;; Code:

(after! elfeed
  ;; Define RSS feed sources
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://www.reddit.com/r/emacs/.rss"))

  ;; Enable Org integration for saving articles
  (setq elfeed-org-files '("~/org/feeds.org")))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("r" . "rss")
;;       :desc "Open RSS reader" "o" #'elfeed
;;       :desc "Update feeds"     "u" #'elfeed-update
;;       :desc "Search feeds"     "s" #'elfeed-search-set-filter))

(provide 'app-rss-config)

;;; app-rss-config.el ends here
