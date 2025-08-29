;;; module/config/lang-config/ejc-sql-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for database and API development.
;; Supports EJC SQL, GraphQL, SQL, JSON, YAML.
;; Enables company-mode for all DB/API-related modes and optional formatting.

;;; Code:

;; ----------------------------
;; EJC SQL: interactive SQL queries
;; ----------------------------
(use-package! ejc-sql
  :defer t
  :commands ejc-sql-mode
  :init
  ;; Define multiple SQL connections
  (setq ejc-sql-connection-alist
        '((:name "postgres-local"   :dbtype "postgres" :dbname "testdb"  :user "postgres" :password "password")
          (:name "mysql-local"      :dbtype "mysql"    :dbname "testdb"  :user "root"     :password "password")
          (:name "postgres-remote"  :dbtype "postgres" :dbname "remotedb":user "remoteuser":password "remotepass")
          (:name "mysql-remote"     :dbtype "mysql"    :dbname "remotedb":user "remoteuser":password "remotepass"))))

;; ----------------------------
;; GraphQL mode
;; ----------------------------
(use-package! graphql-mode
  :defer t
  :mode "\\.graphql\\'"
  :hook (graphql-mode . prettier-mode))

;; ----------------------------
;; SQL mode
;; ----------------------------
(use-package! sql
  :defer t
  :mode "\\.sql\\'")

;; ----------------------------
;; JSON mode
;; ----------------------------
(use-package! json-mode
  :defer t
  :mode "\\.json\\'"
  :hook (json-mode . prettier-mode))

;; ----------------------------
;; YAML mode
;; ----------------------------
(use-package! yaml-mode
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (setq yaml-indent-offset 2))

;; ----------------------------
;; Enable company-mode for all DB/API modes
;; ----------------------------
(dolist (hook '(ejc-sql-mode-hook
                graphql-mode-hook
                sql-mode-hook
                json-mode-hook
                yaml-mode-hook))
  (add-hook hook #'company-mode))

(provide 'ejc-sql-config)

;;; ejc-sql-config.el ends here
