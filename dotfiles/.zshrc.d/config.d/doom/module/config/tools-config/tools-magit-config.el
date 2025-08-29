;;; module/config/tools-config/tools-magit-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Magit configuration with optional Forge integration for GitHub/GitLab.
;; Provides lazy-loading, UI tweaks, auto-revert behavior, and leader keybindings
;; for common Git operations, issues, and pull requests.

;;; Code:

;; ----------------------------
;; Magit configuration
;; ----------------------------
(use-package! magit
  :defer t
  :commands (magit-status magit-blame magit-dispatch)
  :config
  ;; UI and behavior tweaks
  (setq magit-confirm-bury-buffer nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-auto-revert-mode t)) ;; auto-refresh buffers

;; ----------------------------
;; Forge: GitHub/GitLab integration
;; ----------------------------
(use-package! forge
  :after magit
  :commands (forge-browse-issues forge-browse-pullrequests)
  :config
  ;; Optional: fetch pull requests automatically
  (setq forge-pullreq-auto-update t
        forge-alist '(("github.com" "api.github.com" "github.com" forge-github-repository))))

;; ----------------------------
;; Leader keybindings for Magit and Forge
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("g" . "git")
;;       :desc "Magit status" "s" #'magit-status
;;       :desc "Magit dispatch" "d" #'magit-dispatch
;;       :desc "Magit blame" "b" #'magit-blame
;;       :desc "Forge issues" "i" #'forge-browse-issues
;;       :desc "Forge pull requests" "p" #'forge-browse-pullrequests))

(provide 'tools-magit-config)

;;; tools-magit-config.el ends here
