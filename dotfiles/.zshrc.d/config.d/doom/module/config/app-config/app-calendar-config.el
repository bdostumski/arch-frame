;;; app-calendar-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Simplified Doom Emacs calendar config:
;; - Week starts on Monday
;; - International, Bulgarian, and Christian holidays, including Orthodox Easter
;; - Highlight today
;; - Appointment notifications
;; - Org agenda integration
;; - Keybindings for quick access

;;; Code:

(after! calendar
  ;; Week starts on Monday
  (setq calendar-week-start-day 1
        calendar-mark-today t
        calendar-latitude 42.6977
        calendar-longitude 23.3219
        calendar-location-name "Sofia, Bulgaria"
        calendar-date-style 'iso
        calendar-view-holidays-initially-flag t
        calendar-view-diary-initially-flag t
        calendar-view-phases-of-moon-initially-flag t
        calendar-view-sunrise-sunset-initially-flag t)

  ;; Define custom holiday lists
  (setq holiday-international-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 3 8 "International Women's Day")
          (holiday-fixed 5 1 "Labour Day")
          (holiday-fixed 6 1 "Children's Day")
          (holiday-fixed 10 1 "International Day of Older Persons")
          (holiday-fixed 12 10 "Human Rights Day")))

  (setq holiday-bulgarian-holidays
        '((holiday-fixed 1 1 "Нова година (New Year)")
          (holiday-fixed 3 3 "Освобождение на България (Liberation Day)")
          (holiday-fixed 5 1 "Ден на труда (Labour Day)")
          (holiday-fixed 5 6 "Гергьовден (St. George's Day, Army Day)")
          (holiday-fixed 5 24 "Ден на Славянската писменост (Cyrillic Alphabet Day)")
          (holiday-fixed 9 6 "Съединението на България (Unification Day)")
          (holiday-fixed 9 22 "Ден на независимостта (Independence Day)")
          (holiday-fixed 12 24 "Бъдни вечер (Christmas Eve)")
          (holiday-fixed 12 25 "Рождество Христово (Christmas)")
          (holiday-fixed 12 26 "Втори ден на Рождество Христово (2nd Christmas Day)")))

  ;; Define Orthodox Easter and related holidays
  (setq holiday-orthodox-holidays
        '((holiday-greek-orthodox-easter)
          (holiday-fixed 1 6 "Богоявление (Epiphany)")
          (holiday-easter-etc -48 "Месни заговезни (Meat Shrovetide)")
          (holiday-easter-etc -7 "Цветница (Palm Sunday)")
          (holiday-easter-etc 0 "Великден (Orthodox Easter)")
          (holiday-easter-etc 1 "Великденски понеделник (Easter Monday)")
          (holiday-easter-etc 39 "Спасовден (Ascension Day)")
          (holiday-easter-etc 49 "Духовден (Whit Sunday)")
          (holiday-easter-etc 50 "Духовски понеделник (Whit Monday)")))

  ;; Combine all holidays
  (setq calendar-holidays
        (append holiday-international-holidays
                holiday-bulgarian-holidays
                holiday-orthodox-holidays
                holiday-general-holidays    ; Built-in general holidays
                holiday-local-holidays))    ; Any local holidays you might define

  ;; Week number display
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian
                     (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))

  ;; Appointment configuration
  (use-package! appt
    :config
    (appt-activate 1)
    (setq appt-message-warning-time 15
          appt-display-mode-line t
          appt-display-format 'window))

  ;; Org integration
  (after! org
    (setq org-agenda-include-diary t))

  ;; Diary settings
  (setq mark-diary-entries-in-calendar t
        mark-holidays-in-calendar t
        diary-file "~/Documents/diary"))

;; Keybindings
(map! :leader
      (:prefix-map ("c" . "calendar")
       :desc "Open calendar"        "o" #'calendar
       :desc "Go to today"          "t" #'calendar-goto-today
       :desc "View holidays"        "h" #'calendar-list-holidays
       :desc "View phases of moon"  "m" #'calendar-phases-of-moon
       :desc "View sunrise/sunset"  "s" #'calendar-sunrise-sunset
       :desc "List diary entries"   "d" #'diary-list-entries
       :desc "Show org agenda"      "a" #'org-agenda
       :desc "Create appointment"   "A" #'appt-add))

(provide 'app-calendar-config)

;;; app-calendar-config.el ends here
