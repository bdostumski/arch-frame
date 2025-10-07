;;; ejc-sql-config.el --- Configuration for database and API development -*- lexical-binding: t; -*-
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
          (:name "mysql-remote"     :dbtype "mysql"    :dbname "remotedb":user "remoteuser":password "remotepass")))

  :config
  ;; Auto-reconnect if connection drops
  (setq ejc-auto-reconnect t)
  
  ;; Format SQL queries when executing
  (setq ejc-format-sql-at-execution t)
  
  ;; Display results in dedicated buffer
  (setq ejc-result-table-impl 'orgtbl-mode)
  
  ;; Load credentials from auth-source when available
  (when (require 'auth-source nil t)
    (defun ejc-get-password-from-auth (connection)
      "Get password for CONNECTION from auth-source if available."
      (let* ((name (plist-get connection :name))
             (host (or (plist-get connection :host) "localhost"))
             (user (plist-get connection :user))
             (auth-source-creation-prompts
              '((secret . "Password for %h:%u: ")))
             (found (nth 0 (auth-source-search
                            :host host
                            :user user
                            :max 1
                            :create t))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret)
                (funcall secret)
              secret)))))
    
    (advice-add 'ejc-connect :around
                (lambda (orig-fun connection)
                  (let ((conn (copy-tree connection)))
                    (when-let ((password (ejc-get-password-from-auth conn)))
                      (plist-put conn :password password))
                    (funcall orig-fun conn))))))

;; ----------------------------
;; GraphQL mode
;; ----------------------------
(use-package! graphql-mode
  :defer t
  :mode "\\.graphql\\'"
  :hook ((graphql-mode . prettier-mode)
         (graphql-mode . display-line-numbers-mode))
  :config
  ;; Custom syntax highlighting
  (setq graphql-indent-level 2)
  (setq graphql-electric-matching t)
  ;; Allow sending queries to endpoint
  (when (featurep 'request)
    (setq graphql-additional-headers '(("Authorization" . "Bearer ${TOKEN}")))))

;; ----------------------------
;; SQL mode
;; ----------------------------
(use-package! sql
  :defer t
  :mode "\\.sql\\'"
  :config
  ;; Set product based on file or directory name
  (add-hook! 'sql-mode-hook
    (defun my-set-sql-product ()
      (setq-local sql-product
                  (cond
                   ((string-match-p "postgres\\|pgSQL" (buffer-file-name)) 'postgres)
                   ((string-match-p "mysql" (buffer-file-name)) 'mysql)
                   ((string-match-p "oracle" (buffer-file-name)) 'oracle)
                   ((string-match-p "sqlite" (buffer-file-name)) 'sqlite)
                   (t 'postgres)))))  ; default
  
  ;; Set syntax highlighting based on dialect
  (setq sql-dialect-alist
        '((postgres :keywords ("SELECT" "INSERT" "UPDATE" "DELETE" "WITH" "CREATE" "DROP" "ALTER")
           :functions ("count" "sum" "avg" "max" "min" "now" "current_timestamp"))
          (mysql    :keywords ("SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER")
                    :functions ("count" "sum" "avg" "max" "min" "now" "current_timestamp"))))
  
  ;; Format SQL (requires sqlformat package)
  (when (featurep 'sqlformat)
    (setq sqlformat-command 'pgformatter
          sqlformat-args '("-s2" "-g"))))

;; ----------------------------
;; JSON mode
;; ----------------------------
(use-package! json-mode
  :defer t
  :mode "\\.json\\'"
  :hook ((json-mode . prettier-mode)
         (json-mode . (lambda () (setq-local js-indent-level 2))))
  :config
  ;; Validate JSON
  (when (executable-find "jsonlint")
    (add-hook 'json-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          (lambda ()
                            (shell-command (concat "jsonlint --quiet " buffer-file-name)))
                          nil t))))
  
  ;; Format JSON on save if jq is available
  (when (executable-find "jq")
    (defun jq-format-json ()
      "Format the current buffer as JSON using jq."
      (interactive)
      (let ((point (point))
            (exit-code (shell-command-on-region
                        (point-min) (point-max)
                        "jq ." (buffer-name) t "*jq-error*" t)))
        (if (zerop exit-code)
            (goto-char point)
          (message "Failed to format JSON"))))))

;; ----------------------------
;; YAML mode
;; ----------------------------
(use-package! yaml-mode
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :hook (yaml-mode . display-line-numbers-mode)
  :config
  (setq yaml-indent-offset 2)
  
  ;; Validate YAML with yamllint if available
  (when (executable-find "yamllint")
    (add-hook 'yaml-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          (lambda ()
                            (shell-command (concat "yamllint " buffer-file-name)))
                          nil t))))
  
  ;; Format YAML on save with yq if available
  (when (executable-find "yq")
    (defun yq-format-yaml ()
      "Format the current buffer as YAML using yq."
      (interactive)
      (let ((point (point))
            (exit-code (shell-command-on-region
                        (point-min) (point-max)
                        "yq e -P '.' -" (buffer-name) t "*yq-error*" t)))
        (if (zerop exit-code)
            (goto-char point)
          (message "Failed to format YAML"))))))

;; ----------------------------
;; Enable company-mode for all DB/API modes
;; ----------------------------
(dolist (hook '(ejc-sql-mode-hook
                graphql-mode-hook
                sql-mode-hook
                json-mode-hook
                yaml-mode-hook))
  (add-hook hook #'company-mode))

;; Set up company backends for specific modes
(add-hook 'ejc-sql-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(ejc-company-backend company-dabbrev-code company-keywords))))

(provide 'ejc-sql-config)

;;; ejc-sql-config.el ends here
