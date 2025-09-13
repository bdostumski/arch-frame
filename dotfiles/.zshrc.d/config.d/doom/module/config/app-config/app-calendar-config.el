;;; app-calendar-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Doom Emacs calendar configuration

;;; Code:

(use-package! calendar
  :commands (calendar)  ; Only load when explicitly called
  :config
  ;; Basic calendar settings
  (setq calendar-week-start-day 1                    ; Monday
        calendar-mark-today t                        ; Highlight today
        calendar-date-style 'iso                     ; ISO date format
        calendar-mark-holidays-flag t                ; Mark holidays
        calendar-mark-diary-entries-flag nil         ; DON'T auto-mark diary entries
        calendar-view-holidays-initially-flag nil    ; Don't show holidays initially
        calendar-view-diary-initially-flag nil)      ; Don't show diary initially

  ;; Location settings for Sofia, Bulgaria
  (setq calendar-latitude 42.6977
        calendar-longitude 23.3219
        calendar-location-name "Sofia, Bulgaria")

  ;; Use a safe date display function instead
  (setq calendar-date-echo-text
        (lambda (date)
          (format "%04d-%02d-%02d" 
                  (nth 2 date)    ; year
                  (nth 0 date)    ; month  
                  (nth 1 date)))) ; day

  ;; HOLIDAYS - Simplified and safe
  (setq holiday-general-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 5 1 "Labour Day")
          (holiday-fixed 12 25 "Christmas Day")))

  (setq holiday-local-holidays
        '((holiday-fixed 1 1 "Нова година")
          (holiday-fixed 3 3 "Ден на Освобождението")
          (holiday-fixed 5 1 "Ден на труда")
          (holiday-fixed 5 6 "Гергьовден")
          (holiday-fixed 5 24 "Ден на културата")
          (holiday-fixed 9 6 "Ден на Съединението")
          (holiday-fixed 9 22 "Ден на Независимостта")
          (holiday-fixed 12 24 "Бъдни вечер")
          (holiday-fixed 12 25 "Рождество Христово")))

  ;; Simple Orthodox holidays
  (setq holiday-christian-holidays
        '((holiday-fixed 1 6 "Богоявление")
          (holiday-easter-etc 0 "Великден")
          (holiday-easter-etc 1 "Втори ден на Великден")))

  ;; Combine holidays
  (setq calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
                holiday-christian-holidays))

  ;; Week number display 
  (setq calendar-intermonth-text
        '(condition-case nil
             (let ((week-num (car (calendar-iso-from-absolute
                                   (calendar-absolute-from-gregorian
                                    (list month day year))))))
               (when (and week-num (numberp week-num))
                 (propertize (format "W%02d" week-num)
                             'font-lock-face 'font-lock-comment-face)))
           (error "")))  ; Return empty string on any error

  ;; Safe header for week numbers
  (setq calendar-intermonth-header
        (propertize "Week" 'font-lock-face 'font-lock-keyword-face)))

;; Appointment configuration - DISABLED by default
(use-package! appt
  :commands (appt-activate appt-add appt-delete)  ; Only load when needed
  :config
  ;; Don't automatically activate appointments
  (setq appt-audible nil                    ; Disable by default
        appt-message-warning-time 15
        appt-display-mode-line nil          ; Don't show in mode line
        appt-display-format 'ignore))       ; Don't display

;; Manual function to enable appointments when wanted
(defun +calendar/enable-appointments ()
  "Manually enable appointment reminders."
  (interactive)
  (require 'appt)
  (appt-activate 1)
  (setq appt-audible t
        appt-display-mode-line t
        appt-display-format 'window)
  (message "Appointment reminders enabled"))

(defun +calendar/disable-appointments ()
  "Disable appointment reminders."
  (interactive)
  (when (featurep 'appt)
    (appt-activate -1)
    (setq appt-audible nil
          appt-display-mode-line nil
          appt-display-format 'ignore)
    (message "Appointment reminders disabled")))

;; Org integration with error protection - but don't auto-include diary
(after! org
  (condition-case nil
      (setq org-agenda-include-diary nil          ; DON'T auto-include diary
            org-agenda-insert-diary-extract-time t)
    (error nil)))

;; Manual function to include diary in org-agenda when wanted
(defun +calendar/toggle-org-diary-integration ()
  "Toggle diary integration with org-agenda."
  (interactive)
  (setq org-agenda-include-diary (not org-agenda-include-diary))
  (message "Org-agenda diary integration: %s" 
           (if org-agenda-include-diary "enabled" "disabled")))

;; Safe helper functions
(defun +calendar/calendar-goto-today-safe ()
  "Safely go to today's date."
  (interactive)
  (condition-case nil
      (calendar-goto-today)
    (error (message "Could not go to today's date"))))

(defun +calendar/calendar-view-holidays-safe ()
  "Safely view holidays."
  (interactive)
  (condition-case nil
      (calendar-cursor-holidays)
    (error (message "Could not display holidays"))))

(defun +calendar/calendar-view-diary-safe ()
  "Safely view diary entries."
  (interactive)
  (condition-case nil
      (progn
        (require 'diary-lib)  ; Only load when explicitly requested
        (diary-view-entries))
    (error (message "Could not display diary entries"))))

;; Basic keybindings
(map! :leader
      (:prefix ("o" . "open")
       :desc "Calendar" "c" #'calendar))

(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix-map ("c" . "calendar")
        :desc "Open calendar" "c" #'calendar
        :desc "Enable appointments" "a" #'+calendar/enable-appointments
        :desc "Disable appointments" "d" #'+calendar/disable-appointments
        :desc "Toggle org diary" "o" #'+calendar/toggle-org-diary-integration)))

(map! :leader
      (:prefix-map ("e" . "editor")
      (:prefix-map ("a" . "applications")
       (:prefix-map ("c" . "calendar")
        :desc "Open calendar" "c" #'calendar
        :desc "Enable appointments" "a" #'+calendar/enable-appointments
        :desc "Disable appointments" "d" #'+calendar/disable-appointments
        :desc "Toggle org diary" "o" #'+calendar/toggle-org-diary-integration))))

(map! :map calendar-mode-map
      :n "q" #'calendar-exit
      :n "r" #'calendar-redraw
      :n "g" #'+calendar/calendar-goto-today-safe
      :n "." #'+calendar/calendar-goto-today-safe
      :n "j" #'calendar-forward-day
      :n "k" #'calendar-backward-day
      :n "h" #'calendar-backward-day
      :n "l" #'calendar-forward-day
      :n "H" #'+calendar/calendar-view-holidays-safe
      :n "d" #'+calendar/calendar-view-diary-safe
      :n "TAB" #'calendar-forward-month
      :n "S-TAB" #'calendar-backward-month
      :n "<" #'calendar-scroll-left
      :n ">" #'calendar-scroll-right)

;; Additional safe customizations
(after! calendar
  ;; Ensure proper encoding
  (set-language-environment "UTF-8")
  
  ;; Safe hook additions - but don't auto-mark diary
  (condition-case nil
      (progn
        (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
        (add-hook 'calendar-today-invisible-hook 'calendar-mark-today))
    (error nil)))

;; Optional: Simple faces (only if no errors)
(after! calendar
  (condition-case nil
      (custom-set-faces!
       '(calendar-today :inherit highlight :weight bold)
       '(holiday :inherit font-lock-string-face :weight bold))
    (error nil)))

(provide 'app-calendar-config)

;;; app-calendar-config.el ends here
