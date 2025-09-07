;;; module/config/tools-config/tools-magit-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Clean Magit + Forge setup for Doom Emacs

;;; Code:

;; ----------------------------
;; Core Magit Configuration
;; ----------------------------
(use-package! magit
  :defer t
  :init
  ;; Performance and auto-revert settings
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  
  ;; Repository discovery for user's repositories
  (setq magit-repository-directories
        '(("~/Workspace/arch-frame" . 2)
          ("~/Workspace/bdostumski" . 2)
          ("~/Workspace/bdostumski.github.io" . 1)
          ("~/Workspace/learning-apache-kafka" . 0)
          ("~/Workspace/learning-full-stack" . 0)
          ("~/Workspace/learning-javascript" . 0)
          ("~/Workspace/learning-microservices" . 0)
          ("~/Workspace/learning-workspace" . 0)))
  
  :config
  ;; Enhanced UI and behavior settings
  (setq magit-diff-refine-hunk t
        magit-diff-refine-ignore-whitespace t
        magit-save-repository-buffers 'dontask
        magit-revision-insert-related-refs nil
        magit-confirm-bury-buffer nil
        magit-bury-buffer-function #'magit-mode-quit-window
        
        ;; Better buffer display management
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        
        ;; Enhanced commit display
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-revision-headers-format
        "\
Commit:     %h
Author:     %aN <%aE>
AuthorDate: %ad
Commit:     %cN <%cE>
CommitDate: %cd
Subject:    %s
Refs:       %D
"
        
        ;; Log formatting and performance
        magit-log-auto-more t
        magit-log-show-refname-after-summary t
        magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)
        
        ;; Process optimizations
        magit-process-popup-time 10
        magit-process-connection-type nil)
  
  ;; Enhanced diff settings
  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing t
        magit-diff-paint-whitespace nil
        magit-diff-highlight-hunk-header-using-face t)
  
  ;; Hooks for enhanced workflow
  (add-hook 'magit-status-mode-hook #'+magit-setup-project-specific)
  (add-hook 'magit-post-refresh-hook #'+magit-update-forge-data)
  
  ;; Auto-fetch in background (every 5 minutes)
  (run-with-timer 0 300 #'+magit-auto-fetch-user-repos))

;; ----------------------------
;; Enhanced Forge Configuration
;; ----------------------------
(use-package! forge
  :after magit
  :init
  ;; Ensure authentication is set up
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.ssh/id_ed25519.pub"))
  
  :config
  ;; Enhanced Forge settings
  (setq forge-pullreq-auto-update t
        forge-topic-list-limit 100
        forge-add-default-bindings nil  ;; Use Doom's defaults
        forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  
  ;; Better error handling
  (advice-add 'forge-get-repository :around
              (lambda (orig &rest args)
                (condition-case err
                    (apply orig args)
                  (error
                   (message "Forge: %s (repo may not support forge)" (error-message-string err))
                   nil))))
  
  ;; Auto-setup forge for known repositories
  (add-hook 'magit-status-mode-hook #'+forge-auto-setup-user)
  
  ;; Enhanced forge database management
  (setq forge-database-file (expand-file-name "forge-database.sqlite"
                                              doom-cache-dir)))

;; ----------------------------
;; Git Commit Message Enhancement
;; ----------------------------
(after! git-commit
  ;; Better commit message formatting
  (setq git-commit-summary-max-length 72
        git-commit-fill-column 72
        git-commit-style-convention-checks t
        git-commit-known-pseudo-headers
        '("Signed-off-by" "Acked-by" "Modified-by" "Cc"
          "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by"
          "Co-authored-by" "Closes" "Fixes" "Refs" "Resolves"))
  
  ;; Enable helpful modes
  (add-hook 'git-commit-mode-hook #'flyspell-mode)
  (add-hook 'git-commit-mode-hook #'git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-mode-hook #'+git-commit-setup-conventional))

;; ----------------------------
;; Project-specific Functions for user. 
;; ----------------------------
(defun +magit-setup-project-specific ()
  "Setup project-specific Magit configurations for user repositories."
  (when-let ((root (magit-toplevel)))
    ;; Setup commit templates for specific projects
    (let ((template-file (expand-file-name ".gitmessage" root)))
      (when (file-exists-p template-file)
        (setq-local git-commit-template template-file)))
    
    ;; Special configurations for user's specific repos
    (cond
     ;; arch-frame repository - main project
     ((string-match-p "arch-frame" root)
      (setq-local git-commit-template "feat: \n\n- \n\nCloses: #")
      (message "Arch-frame repository detected"))
     
     ;; learning repositories - educational content
     ((string-match-p "learning-" root)
      (setq-local git-commit-template "learn: \n\n- \n\nNotes: ")
      (message "Learning repository detected"))
     
     ;; GitHub Pages repository - content updates
     ((string-match-p "bdostumski\\.github\\.io" root)
      (setq-local git-commit-template "content: \n\n- \n\nUpdates: ")
      (message "GitHub Pages repository detected"))
     
     ;; Profile repository - README and profile updates
     ((string-match-p "/bdostumski$" root)
      (setq-local git-commit-template "profile: \n\n- \n\nUpdates: ")
      (message "Profile repository detected")))
    
    ;; Auto-setup forge for GitHub repositories
    (when (string-match-p "github\\.com.*bdostumski" (or (magit-get "remote.origin.url") ""))
      (unless (and (fboundp 'forge-get-repository) (forge-get-repository :tracked?))
        (run-with-timer 2 nil
                        (lambda ()
                          (when (y-or-n-p "Setup Forge for this repository? ")
                            (forge-add-repository (forge-get-repository :create? t)))))))))

(defun +magit-update-forge-data ()
  "Update forge data in background."
  (when (and (fboundp 'forge-get-repository)
             (forge-get-repository :tracked?))
    (run-with-timer 1 nil #'forge-pull)))

(defun +magit-auto-fetch-user-repos ()
  "Auto-fetch user's repositories in background."
  (let ((user-repos '("~/Workspace/arch-frame"
                           "~/Workspace/bdostumski"
                           "~/Workspace/bdostumski.github.io"
                           "~/Workspace/learning-apache-kafka"
                           "~/Workspace/learning-full-stack"
                           "~/Workspace/learning-javascript"
                           "~/Workspace/learning-microservices"
                           "~/Workspace/learning-workspace")))
    (dolist (repo user-repos)
      (when (and (file-directory-p repo)
                 (file-directory-p (expand-file-name ".git" repo)))
        (let ((default-directory repo))
          (condition-case nil
              (magit-run-git-async "fetch" "--all")
            (error nil)))))))

(defun +forge-auto-setup-user ()
  "Automatically setup forge for user's GitHub repositories."
  (when (and (magit-gitdir)
             (magit-get "remote.origin.url"))
    (let ((url (magit-get "remote.origin.url")))
      (when (and (string-match-p "github\\.com.*bdostumski" url)
                 (not (and (fboundp 'forge-get-repository) (forge-get-repository :tracked?))))
        (run-with-timer 2 nil
                        (lambda ()
                          (condition-case nil
                              (forge-add-repository (forge-get-repository :create? t))
                            (error nil))))))))

(defun +git-commit-setup-conventional ()
  "Setup conventional commit format helpers."
  (when (derived-mode-p 'git-commit-mode)
    ;; Add conventional commit types to completion
    (setq-local completion-at-point-functions
                (cons '+git-commit-conventional-complete
                      completion-at-point-functions))))

(defun +git-commit-conventional-complete ()
  "Completion function for conventional commit types."
  (when (and (= (point) (save-excursion (goto-char (point-min)) (point-at-eol)))
             (looking-back "^\\([a-z]*\\)" (point-at-bol)))
    (let ((types '("feat" "fix" "docs" "style" "refactor" "test" "chore"
                   "perf" "ci" "build" "revert" "learn" "content" "profile")))
      (list (match-beginning 1) (match-end 1) types))))

;; ----------------------------
;; External Tool Integration
;; ----------------------------
(when (executable-find "delta")
  ;; Use delta for better diffs
  (setq magit-delta-default-dark-theme "Dracula")
  (add-hook 'magit-mode-hook #'magit-delta-mode))

(when (executable-find "gh")
  ;; GitHub CLI integration (minimal, using Doom's existing structure)
  (defun +magit-gh-create-pr ()
    "Create GitHub PR using gh CLI."
    (interactive)
    (shell-command "gh pr create --web"))
  
  (defun +magit-gh-browse-repo ()
    "Browse current repository on GitHub."
    (interactive)
    (shell-command "gh browse")))

;; ----------------------------
;; Performance Optimizations
;; ----------------------------
(after! magit
  ;; Disable expensive operations in large repos
  (add-hook 'magit-status-mode-hook
            (lambda ()
              (when (> (length (magit-list-files)) 1000)
                (setq-local magit-diff-highlight-indentation nil)
                (setq-local magit-diff-highlight-trailing nil))))
  
  ;; Cache repository information
  (setq magit-repository-directory-cache-file
        (expand-file-name "magit-repos.cache" doom-cache-dir)))

(provide 'tools-magit-config)

;;; tools-magit-config.el ends here
