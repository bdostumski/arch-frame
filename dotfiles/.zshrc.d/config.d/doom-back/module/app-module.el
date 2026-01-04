;;; module/app-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module groups together all "application-like" configurations
;; inside Emacs. These are non-coding tools such as calendar, music,
;; chat, RSS, and external integration utilities.

;;; Code:

;; Calendar integration for scheduling and agenda management
(load! "config/app-config/app-calendar-config.el")

;; Diary file configuration
(load! "config/app-config/app-diary-lib-config.el")

;; EMMS (Emacs Multimedia System) - play audio/music inside Emacs
(load! "config/app-config/app-emms-config.el")

;; "Everywhere" - use Emacs keybindings in external applications
(load! "config/app-config/app-everywhere-config.el")

;; IRC client - socialize and chat directly in Emacs
(load! "config/app-config/app-irc-config.el")

;; RSS reader with Org-mode integration
(load! "config/app-config/app-rss-config.el")

(provide 'app-module)

;;; app-module.el ends here
