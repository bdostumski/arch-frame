;;; module/config/tools-config/tools-lookup-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Lookup module provides a unified way to jump to definitions, references,
;; and documentation across languages and tools, integrating LSP, man pages, info,
;; and other sources for easy navigation and code exploration.

;;; Code:

;; ----------------------------
;; Enhanced Core Lookup Configuration
;; ----------------------------
(after! lookup
  ;; Dictionary and language settings
  (setq lookup-dictionary-dictionary "en_US"
        +lookup-open-url-fn #'browse-url
        +lookup-provider-url-alist
        '(("DuckDuckGo" "https://duckduckgo.com/?q=%s")
          ("GitHub" "https://github.com/search?q=%s")
          ("MDN" "https://developer.mozilla.org/en-US/search?q=%s")
          ("Stack Overflow" "https://stackoverflow.com/search?q=%s")
          ("Arch Wiki" "https://wiki.archlinux.org/index.php?search=%s")
          ("NPM" "https://www.npmjs.com/search?q=%s")
          ("Rust Docs" "https://docs.rs/releases/search?query=%s")))
  
  ;; Enhanced documentation providers for user's tech stack
  (setq +lookup-definition-functions
        '(+lookup/xref-definitions-backend-fn
          +lookup/dumb-jump-backend-fn
          +lookup/project-search-backend-fn
          +lookup/evil-goto-definition-backend-fn)
        +lookup-references-functions
        '(+lookup/xref-references-backend-fn
          +lookup/project-search-backend-fn)
        +lookup-documentation-functions
        '(+lookup/online-backend-fn
          +lookup/dash-docsets-backend-fn
          man
          woman)))

;; ----------------------------
;; Enhanced Documentation Integration
;; ----------------------------
(use-package! dash-docs
  :defer t
  :config
  ;; Docsets for user's development stack
  (setq dash-docs-docsets-path (expand-file-name "docsets" doom-cache-dir)
        dash-docs-common-docsets 
        '("JavaScript" "HTML" "CSS" "Node.js" "TypeScript" "React"
          "Python" "Rust" "Go" "Bash" "Git" "Docker"))
  
  ;; Auto-install docsets based on project type
  (defun +lookup/auto-install-docsets ()
    "Auto-install relevant docsets based on current project."
    (when-let ((root (projectile-project-root)))
      (cond
       ;; JavaScript/Node.js projects (learning-javascript)
       ((or (file-exists-p (expand-file-name "package.json" root))
            (string-match-p "javascript" root))
        (dash-docs-ensure-docset-installed "JavaScript")
        (dash-docs-ensure-docset-installed "Node.js")
        (dash-docs-ensure-docset-installed "TypeScript"))
       
       ;; Python projects
       ((or (file-exists-p (expand-file-name "requirements.txt" root))
            (file-exists-p (expand-file-name "pyproject.toml" root)))
        (dash-docs-ensure-docset-installed "Python"))
       
       ;; Rust projects
       ((file-exists-p (expand-file-name "Cargo.toml" root))
        (dash-docs-ensure-docset-installed "Rust"))
       
       ;; Go projects
       ((file-exists-p (expand-file-name "go.mod" root))
        (dash-docs-ensure-docset-installed "Go"))
       
       ;; Arch Linux configs (arch-frame)
       ((string-match-p "arch-frame" root)
        (dash-docs-ensure-docset-installed "Bash")
        (dash-docs-ensure-docset-installed "Docker")))))
  
  ;; Hook to auto-install docsets
  (add-hook 'projectile-after-switch-project-hook #'+lookup/auto-install-docsets))

;; ----------------------------
;; GitHub Integration for user's repositories
;; ----------------------------
(defun +lookup/search-in-user-repos (query)
  "Search for QUERY across user's GitHub repositories."
  (interactive "sSearch in bdostumski repos: ")
  (let ((repos '("arch-frame" "bdostumski" "learning-workspace" 
                 "bdostumski.github.io" "learning-javascript"))
        (search-url "https://github.com/search?q=%s+user:bdostumski"))
    (browse-url (format search-url (url-hexify-string query)))))

(defun +lookup/browse-current-repo-docs ()
  "Browse documentation for the current repository."
  (interactive)
  (when-let ((root (projectile-project-root)))
    (let ((repo-name (file-name-nondirectory (string-trim-right root "/"))))
      (cond
       ;; GitHub Pages site
       ((string= repo-name "bdostumski.github.io")
        (browse-url "https://bdostumski.github.io"))
       
       ;; Other repositories - browse GitHub README
       ((member repo-name '("arch-frame" "bdostumski" "learning-workspace" "learning-javascript"))
        (browse-url (format "https://github.com/bdostumski/%s" repo-name)))
       
       ;; Fallback to project root
       (t (dired root))))))

(defun +lookup/search-arch-wiki (query)
  "Search the Arch Wiki for QUERY (useful for arch-frame repo)."
  (interactive "sSearch Arch Wiki: ")
  (browse-url (format "https://wiki.archlinux.org/index.php?search=%s" 
                     (url-hexify-string query))))

;; ----------------------------
;; Language-Specific Lookup Functions
;; ----------------------------
(defun +lookup/javascript-mdn (symbol)
  "Look up JavaScript SYMBOL on MDN."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Search MDN: "))))
  (browse-url (format "https://developer.mozilla.org/en-US/search?q=%s" 
                     (url-hexify-string symbol))))

(defun +lookup/npm-package (package)
  "Look up NPM PACKAGE."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "NPM package: "))))
  (browse-url (format "https://www.npmjs.com/package/%s" package)))

(defun +lookup/rust-docs (symbol)
  "Look up Rust SYMBOL in documentation."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Search Rust docs: "))))
  (browse-url (format "https://docs.rs/releases/search?query=%s" 
                     (url-hexify-string symbol))))

(defun +lookup/github-search (query)
  "Search GitHub for QUERY."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Search GitHub: "))))
  (browse-url (format "https://github.com/search?q=%s" 
                     (url-hexify-string query))))

;; ----------------------------
;; Learning-Focused Lookup Tools
;; ----------------------------
(defun +lookup/explain-concept (concept)
  "Get explanation of programming CONCEPT (useful for learning repos)."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Programming concept: "))))
  (let ((search-terms (format "%s programming concept explanation" concept)))
    (browse-url (format "https://duckduckgo.com/?q=%s" 
                       (url-hexify-string search-terms)))))

(defun +lookup/code-examples (topic)
  "Find code examples for TOPIC."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Code examples for: "))))
  (let ((search-terms (format "%s code examples tutorial" topic)))
    (browse-url (format "https://github.com/search?q=%s&type=code" 
                       (url-hexify-string search-terms)))))

(defun +lookup/learning-resources (topic)
  "Find learning resources for TOPIC."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Learning topic: "))))
  (with-current-buffer (get-buffer-create "*Learning Resources*")
    (erase-buffer)
    (insert (format "# Learning Resources: %s\n\n" topic))
    (insert "## Documentation\n")
    (insert (format "- [MDN](%s)\n" 
                   (format "https://developer.mozilla.org/en-US/search?q=%s" 
                          (url-hexify-string topic))))
    (insert (format "- [Stack Overflow](%s)\n"
                   (format "https://stackoverflow.com/search?q=%s" 
                          (url-hexify-string topic))))
    (insert (format "- [GitHub Examples](%s)\n"
                   (format "https://github.com/search?q=%s&type=code" 
                          (url-hexify-string topic))))
    (insert "\n## Practice\n")
    (insert "- [ ] Read documentation\n")
    (insert "- [ ] Try code examples\n")
    (insert "- [ ] Build practice project\n")
    (markdown-mode)
    (display-buffer (current-buffer))))

;; ----------------------------
;; Project-Aware Lookup
;; ----------------------------
(defun +lookup/smart-lookup-at-point ()
  "Smart lookup based on current context and project."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t))
        (root (projectile-project-root)))
    (when symbol
      (cond
       ;; JavaScript/Node.js context
       ((and root (or (string-match-p "javascript" root)
                     (file-exists-p (expand-file-name "package.json" root))))
        (+lookup/javascript-mdn symbol))
       
       ;; Rust context
       ((and root (file-exists-p (expand-file-name "Cargo.toml" root)))
        (+lookup/rust-docs symbol))
       
       ;; Arch Linux context (arch-frame)
       ((and root (string-match-p "arch-frame" root))
        (+lookup/search-arch-wiki symbol))
       
       ;; Learning context
       ((and root (string-match-p "learning-" root))
        (+lookup/explain-concept symbol))
       
       ;; Default: try LSP first, then general search
       (t (or (and (bound-and-true-p lsp-mode) (lsp-find-definition))
              (+lookup/github-search symbol)))))))

;; ----------------------------
;; Documentation Browser Integration
;; ----------------------------
(use-package! devdocs
  :defer t
  :config
  ;; Auto-install relevant documentation based on projects
  (setq devdocs-data-dir (expand-file-name "devdocs" doom-cache-dir))
  
  (defun +lookup/install-devdocs-for-project ()
    "Install relevant DevDocs based on current project."
    (when-let ((root (projectile-project-root)))
      (cond
       ((or (file-exists-p (expand-file-name "package.json" root))
            (string-match-p "javascript" root))
        (devdocs-install "javascript")
        (devdocs-install "node"))
       ((file-exists-p (expand-file-name "Cargo.toml" root))
        (devdocs-install "rust"))
       ((string-match-p "arch-frame" root)
        (devdocs-install "bash")))))
  
  (add-hook 'projectile-after-switch-project-hook #'+lookup/install-devdocs-for-project))

;; ----------------------------
;; Enhanced Dictionary and Thesaurus
;; ----------------------------
(use-package! define-word
  :defer t
  :config
  (setq define-word-default-service 'wordnik))

(defun +lookup/define-programming-term (term)
  "Define programming TERM using multiple sources."
  (interactive (list (or (thing-at-point 'symbol t)
                        (read-string "Define programming term: "))))
  (with-current-buffer (get-buffer-create "*Programming Definition*")
    (erase-buffer)
    (insert (format "# Definition: %s\n\n" term))
    (insert "## Programming Context\n\n")
    (insert "## Common Usage\n\n")
    (insert "## Related Concepts\n\n")
    (insert "## Resources\n")
    (insert (format "- [Search GitHub](%s)\n"
                   (format "https://github.com/search?q=%s" 
                          (url-hexify-string term))))
    (insert (format "- [Stack Overflow](%s)\n"
                   (format "https://stackoverflow.com/search?q=%s" 
                          (url-hexify-string term))))
    (markdown-mode)
    (display-buffer (current-buffer))))

;; ----------------------------
;; Mode-Specific Enhancements
;; ----------------------------
(after! js2-mode
(setq-local +lookup-documentation-functions
        (cons #'+lookup/javascript-mdn +lookup-documentation-functions)))

(after! rust-mode
(setq-local +lookup-documentation-functions
        (cons #'+lookup/rust-docs +lookup-documentation-functions)))

(after! markdown-mode
 ;; Enhanced lookup for documentation files
 (setq-local +lookup-definition-functions
        (cons #'+lookup/github-search +lookup-definition-functions)))

;; ----------------------------
;; Project Context Integration
;; ----------------------------
(defun +lookup/show-context-info ()
  "Show lookup context information for current buffer/project."
  (interactive)
  (let ((project (projectile-project-name))
        (mode major-mode)
        (symbol (thing-at-point 'symbol t)))
    (message "Context: Project=%s | Mode=%s | Symbol=%s" 
             (or project "none") mode (or symbol "none"))))

;; ----------------------------
;; Performance and Caching
;; ----------------------------
(setq +lookup-cache-directory (expand-file-name "lookup/" doom-cache-dir))
(unless (file-directory-p +lookup-cache-directory)
  (make-directory +lookup-cache-directory t))

;; Cache frequently looked up symbols
(defvar +lookup/symbol-cache (make-hash-table :test 'equal)
  "Cache for frequently looked up symbols.")

(defun +lookup/cache-symbol (symbol result)
  "Cache SYMBOL lookup RESULT."
  (puthash symbol result +lookup/symbol-cache))

(defun +lookup/get-cached-symbol (symbol)
  "Get cached result for SYMBOL."
  (gethash symbol +lookup/symbol-cache))

(provide 'tools-lookup-config)

;;; tools-lookup-config.el ends here
