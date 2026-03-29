;;; app-diary-lib-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete diary configuration for Doom Emacs
;; Integrates with calendar and org-agenda

;;; Code:

(use-package! diary-lib
  :commands (diary-list-entries diary-show-all-entries diary-fancy-display)
  :init
  ;; Defer diary loading - don't load automatically
  (setq diary-display-function 'diary-fancy-display)

  :config
  ;; Basic diary settings - only set when explicitly loaded
  (setq diary-file (expand-file-name "diary" doom-user-dir)
        diary-list-entries-hook '(diary-include-other-diary-files
                                  diary-sort-entries)
        diary-mark-entries-hook 'diary-mark-included-diary-files
        diary-number-of-entries 7
        diary-comment-start ";;"
        diary-nonmarking-symbol "&")

  ;; Only set display hook when diary is actually used
  (setq diary-display-hook nil)  ; Don't auto-enable fancy display

  (setq diary-date-forms
        '((month "/" day "[^/0-9]")           ; American format: 12/25
          (day "/" month "[^/0-9]")           ; European format: 25/12
          (year "-" month "-" day "[^0-9]")   ; ISO format: 2024-12-25
          (monthname " *" day "[^,0-9]")      ; December 25
          (day " *" monthname "[^,0-9]")      ; 25 December
          (dayname "\\W")))                   ; Monday, Tuesday, etc.

  ;; Enhanced diary display - but don't auto-create buffer
  (setq diary-fancy-buffer "*Fancy Diary Entries*"
        diary-fancy-overwrite t))

;; -------------------------------------------------------------------
;; Diary helper: Safe open with auto-initialize if file is missing
;; -------------------------------------------------------------------
(defun +diary/open-diary-file ()
  "Open diary file, initializing it if it doesn't exist."
  (interactive)
  (unless (file-exists-p diary-file)
    (+diary/diary-initialize))
  (find-file diary-file))

;; -------------------------------------------------------------------
;; Diary entry functions
;; -------------------------------------------------------------------
(defun +diary/diary-create-entry ()
  "Create a new diary entry for the selected date."
  (interactive)
  (unless (file-exists-p diary-file)
    (+diary/diary-initialize))
  (let* ((date (if (eq major-mode 'calendar-mode)
                   (calendar-cursor-to-date t)
                 (calendar-current-date)))
         (date-string (calendar-date-string date)))
    (find-file diary-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "%s " date-string))
    (save-excursion (insert "\n"))
    (message "Creating diary entry for %s" date-string)))

(defun +diary/diary-create-recurring-entry ()
  "Create a recurring diary entry."
  (interactive)
  (unless (file-exists-p diary-file)
    (+diary/diary-initialize))
  (let* ((type (completing-read "Recurrence type: "
                                '("Daily" "Weekly" "Monthly" "Yearly" "Every Monday" "Every Friday")))
         (description (read-string "Event description: "))
         (time (read-string "Time (optional, e.g., 09:00): "))
         (entry (cond
                 ((string= type "Daily")
                  (format "%s %s%s"
                          (if (string-empty-p time) "" (concat time " "))
                          description
                          " (daily)"))
                 ((string= type "Weekly")
                  (format "%%1 %s %s%s"
                          (if (string-empty-p time) "" (concat time " "))
                          description
                          " (weekly)"))
                 ((string= type "Monthly")
                  (format "%%2 %s %s%s"
                          (if (string-empty-p time) "" (concat time " "))
                          description
                          " (monthly)"))
                 ((string= type "Yearly")
                  (format "%%3 %s %s%s"
                          (if (string-empty-p time) "" (concat time " "))
                          description
                          " (yearly)"))
                 ((string= type "Every Monday")
                  (format "Monday %s %s%s"
                          (if (string-empty-p time) "" (concat time " "))
                          description
                          " (every Monday)"))
                 ((string= type "Every Friday")
                  (format "Friday %s %s%s"
                          (if (string-empty-p time) "" (concat time " "))
                          description
                          " (every Friday)")))))
    (find-file diary-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert entry "\n")
    (save-buffer)
    (message "Added recurring entry: %s" description)))

(defun +diary/diary-quick-add ()
  "Quick add diary entry for today."
  (interactive)
  (unless (file-exists-p diary-file)
    (+diary/diary-initialize))
  (let* ((today (calendar-current-date))
         (date-string (calendar-date-string today))
         (entry (read-string (format "Quick entry for %s: " date-string))))
    (when (not (string-empty-p entry))
      (with-temp-buffer
        (insert (format "%s %s\n" date-string entry))
        (append-to-file (point-min) (point-max) diary-file))
      (message "Added: %s" entry))))

(defun +diary/diary-show-entries ()
  "Show diary entries for current week."
  (interactive)
  (require 'diary-lib)  ; Only load when explicitly called
  (diary-list-entries (calendar-current-date) 7))

(defun +diary/diary-goto-date ()
  "Go to a specific date in diary."
  (interactive)
  (unless (file-exists-p diary-file)
    (+diary/diary-initialize))
  (let* ((date (org-read-date nil t))
         (cal-date (list (nth 4 (decode-time date))  ; month
                         (nth 3 (decode-time date))  ; day
                         (nth 5 (decode-time date))))) ; year
    (require 'diary-lib)  ; Only load when needed
    (diary-list-entries cal-date 1)))

;; -------------------------------------------------------------------
;; Diary templates
;; -------------------------------------------------------------------
(defun +diary/diary-insert-template ()
  "Insert a diary template."
  (interactive)
  (let ((template (completing-read "Template: "
                                   '("Meeting" "Appointment" "Birthday" "Anniversary" "Reminder" "Custom"))))
    (cond
     ((string= template "Meeting")
      (insert (format "%s Meeting with %s at %s"
                      (read-string "Time: ")
                      (read-string "Person/Company: ")
                      (read-string "Location: "))))
     ((string= template "Appointment")
      (insert (format "%s %s appointment"
                      (read-string "Time: ")
                      (read-string "Type (doctor, dentist, etc.): "))))
     ((string= template "Birthday")
      (insert (format "%s's birthday"
                      (read-string "Name: "))))
     ((string= template "Anniversary")
      (insert (format "%s anniversary (%s)"
                      (read-string "Type: ")
                      (read-string "Details: "))))
     ((string= template "Reminder")
      (insert (format "Reminder: %s"
                      (read-string "What to remember: "))))
     ((string= template "Custom")
      (insert (read-string "Custom entry: "))))))

;; -------------------------------------------------------------------
;; Calendar integration - only when explicitly called
;; -------------------------------------------------------------------
(defun +diary/calendar-mark-diary-entries ()
  "Mark diary entries in calendar."
  (interactive)
  (when (get-buffer "*Calendar*")
    (with-current-buffer "*Calendar*"
      (require 'diary-lib)
      (calendar-mark-diary-entries))))

;; -------------------------------------------------------------------
;; Diary file initialization
;; -------------------------------------------------------------------
(defun +diary/diary-initialize ()
  "Initialize diary file with example entries."
  (interactive)
  (unless (file-exists-p diary-file)
    (with-temp-file diary-file
      (insert ";; -*- mode: diary -*-\n")
      (insert ";; Personal Diary File\n\n")
      (insert ";; Date formats supported:\n")
      (insert ";; 12/25 Christmas (American format)\n")
      (insert ";; 25/12 Christmas (European format)\n")
      (insert ";; 2024-12-25 Christmas (ISO format)\n")
      (insert ";; December 25 Christmas (Month name)\n")
      (insert ";; 25 December Christmas (Day Month)\n")
      (insert ";; Monday 09:00 Weekly team meeting (Day of week)\n\n")
      (insert ";; Recurring entries:\n")
      (insert ";; %%(diary-anniversary 5 24 1970) Wedding anniversary (%d years)\n")
      (insert ";; %%(diary-block 12 20 2024 1 5 2025) Holiday period\n")
      (insert ";; Monday 09:00 Weekly team meeting\n\n")
      (insert ";; Examples (commented out to prevent auto-display):\n")
      (insert (format ";; %s Example diary entry for today\n"
                      (calendar-date-string (calendar-current-date))))
      (insert ";; Monday 09:00 Weekly team meeting\n")
      (insert ";; Friday 17:00 Weekend planning\n"))
    (message "Diary file initialized at %s" diary-file)))

(provide 'app-diary-lib-config)

;;; app-diary-lib-config.el ends here
