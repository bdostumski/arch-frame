;;; config/app-config/app-calendar-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Calendar setup for Doom Emacs.
;; - Week starts on Monday
;; - Display selected holidays
;; - Keybindings for quick access

;;; Code:

(after! calendar
  ;; Start week on Monday
  (setq calendar-week-start-day 1)

  ;; Define custom holidays
  (setq calendar-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 12 25 "Christmas")))

  ;; Optional: highlight today in calendar
  (setq calendar-mark-today t))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("c" . "calendar")
;;       :desc "Open calendar" "o" #'calendar
;;       :desc "Go to today" "t" #'calendar-goto-today
;;       :desc "View holidays" "h" #'calendar-list-holidays))

(provide 'app-calendar-config)

;;; app-calendar-config.el ends here
