;;; lang-sql-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; SQL configuration for Doom Emacs.
;; Provides PostgreSQL connection defaults, formatting preferences,
;; and leader keybindings for connecting and executing queries.

;;; Code:

(after! sql
  ;; PostgreSQL default connection parameters
  (setq sql-postgres-login-params
        '((user     :default "postgres")
          (database :default "mydb")
          (server   :default "localhost")
          (port     :default 5432)))

  ;; SQL formatting preferences
  (setq sql-indent-offset 2
        sql-capitalize-keywords t)
  
  ;; Improve SQL result buffer display
  (setq sql-display-sqli-buffer-function #'pop-to-buffer
        sql-pop-to-buffer-after-send-region t)
  
  ;; Set product by default
  (setq sql-product 'postgres))

;; SQL mode hooks for additional customization
(add-hook! sql-mode
  (setq-local tab-width 2)
  (setq-local evil-shift-width 2))

;; Enable SQL completion with company-mode
(after! company
  (add-to-list 'company-backends 'company-keywords))

;; SQLi history configuration
(after! sql-interactive-mode
  (setq sql-input-ring-file-name
        (concat doom-cache-dir "sql-input-ring")
        sql-input-ring-separator "\n"))

(provide 'lang-sql-config)
;;; lang-sql-config.el ends here
