;;; module/config/tools-config/tools-magit-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Full Magit + Forge setup for Doom Emacs
;; - Lazy loading
;; - UI tweaks
;; - Auto-revert
;; - Forge integration for GitHub/GitLab
;; - Leader keybindings for common Git commands

;;; Code:

;; ----------------------------
;; Magit configuration
;; ----------------------------
(use-package! magit
  :defer t
  :commands (magit-status magit-blame magit-dispatch)
  :init
  ;; Optional: enable auto-revert globally for Magit buffers
  (setq global-auto-revert-non-file-buffers t)
  :config
  ;; UI and behavior tweaks
  (setq magit-confirm-bury-buffer nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-auto-revert-mode t))

;; ----------------------------
;; Forge: GitHub/GitLab integration
;; ----------------------------
(use-package! forge
  :after magit
  :commands (forge-browse-issues forge-browse-pullrequests)
  :config
  ;; Auto-update pull requests
  (setq forge-pullreq-auto-update t)
  ;; Optional: ensure Forge does not error on empty repos
  (advice-add 'forge-get-repository :around
              (lambda (orig &rest args)
                (if (and (car args) (not (file-directory-p (concat (car args) "/.git"))))
                    (message "Forge skipped: no git repo")
                  (apply orig args)))))

;; ----------------------------
;; Leader keybindings for Magit + Forge
;; ----------------------------
(map! :leader
      (:prefix-map ("g" . "git")
       :desc "Magit status" "s" #'magit-status
       :desc "Magit dispatch" "d" #'magit-dispatch
       :desc "Magit blame" "b" #'magit-blame
       :desc "Forge issues" "i" #'forge-browse-issues
       :desc "Forge pull requests" "p" #'forge-browse-pullrequests))

(provide 'tools-magit-config)

;;; tools-magit-config.el ends here
