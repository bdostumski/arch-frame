;;; module/config/tools-config/tools-git-config.el -*- lexical-binding: t -*-
 
;;; Commentary:
;; Git integration for Doom Emacs
 
;;; Code:

;; ----------------------------
;; Core Git Configuration
;; ----------------------------
(after! magit
  (setq magit-diff-refine-hunk t
        magit-repository-directories
        '(("~/Workspace/arch-frame" . 2)
          ("~/Workspace/bdostumski" . 2)
          ("~/Workspace/bdostumski.github.io" . 1)
          ("~/Workspace/learning-apache-kafka" . 0)
          ("~/Workspace/learning-full-stack" . 0)
          ("~/Workspace/learning-javascript" . 0)
          ("~/Workspace/learning-microservices" . 0)
          ("~/Workspace/learning-workspace" . 0))
        magit-save-repository-buffers 'dontask
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-bury-buffer-function #'magit-mode-quit-window
        magit-revision-show-gravatars t
        magit-revision-headers-format "\
Commit:     %h
Author:     %aN <%aE>
AuthorDate: %ad
Commit:     %cN <%cE>
CommitDate: %cd
Subject:    %s
Refs:       %D
"
        magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

;; ----------------------------
;; git-link
;; ----------------------------
(use-package! git-link
  :defer t
  :config
  (setq git-link-use-commit t
        git-link-open-in-browser nil
        git-link-default-branch nil))

;; ----------------------------
;; git-messenger
;; ----------------------------
(use-package! git-messenger
  :defer t
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t
        git-messenger:popup-time 10
        git-messenger:handle-popup-timeout t)
  (setq git-messenger:popup-format-function
        (lambda (commit-info)
          (format "Commit: %s\nAuthor: %s\nDate: %s\n\n%s"
                  (plist-get commit-info :id)
                  (plist-get commit-info :author)
                  (plist-get commit-info :date)
                  (plist-get commit-info :summary)))))

;; ----------------------------
;; git-timemachine
;; ----------------------------
(use-package! git-timemachine
  :defer t
  :config
  (map! :map git-timemachine-mode-map
        :n "p" #'git-timemachine-show-previous-revision
        :n "n" #'git-timemachine-show-next-revision
        :n "g" #'git-timemachine-show-nth-revision
        :n "q" #'git-timemachine-quit
        :n "b" #'git-timemachine-blame))

;; ----------------------------
;; GitHub CLI integration
;; ----------------------------
(when (executable-find "gh")
  (defun +git/gh-create-pr () (interactive) (compile "gh pr create --web"))
  (defun +git/gh-browse-repo () (interactive) (compile "gh browse"))
  (defun +git/gh-view-issues () (interactive) (compile "gh issue list --web"))
  (defun +git/gh-view-prs () (interactive) (compile "gh pr list --web"))
  (defun +git/gh-checkout-pr () (interactive) (compile "gh pr checkout $(gh pr list --limit 10 | fzf | cut -f1)"))

  (map! :leader
        (:prefix ("g" . "git")
         (:prefix ("i" . "GitHub")
          :desc "Create PR"      "R" #'+git/gh-create-pr
          :desc "Browse Repo"    "H" #'+git/gh-browse-repo
          :desc "View Issues"    "i" #'+git/gh-view-issues
          :desc "View PRs"       "p" #'+git/gh-view-prs
          :desc "Checkout PR"    "c" #'+git/gh-checkout-pr))))


(provide 'tools-git-config)
;;; tools-git-config.el ends here
