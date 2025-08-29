;;; module/config/lang-config/lang-sql-config.el -*- lexical-binding: t; -*-
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
        sql-capitalize-keywords t))

;; ----------------------------
;; Leader keybindings for SQL
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("s" . "sql")
;;       :desc "Connect to DB"           "c" #'sql-connect
;;       :desc "Execute buffer/query"    "e" #'sql-send-buffer
;;       :desc "Execute region/query"    "r" #'sql-send-region))

(provide 'lang-sql-config)

;;; lang-sql-config.el ends here
