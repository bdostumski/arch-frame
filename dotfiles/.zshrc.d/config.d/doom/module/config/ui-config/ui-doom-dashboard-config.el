;;; module/config/ui-config/ui-doom-dashboard-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Custom configuration for Doom Emacs Dashboard (:ui doom-dashboard).
;; Provides banner, menu sections, and startup tweaks.

;;; Code:

(after! doom-dashboard
  ;; ----------------------------------------
  ;; Banner Options
  ;; ----------------------------------------
  ;; Default Doom text logo
  (setq fancy-splash-image nil)

  ;; Example: custom ASCII banner
  ;; (setq +doom-dashboard-banner-file (concat doom-private-dir "banner.txt"))

  ;; Example: image banner
  ;; (setq fancy-splash-image "/path/to/banner.png")

  ;; ----------------------------------------
  ;; Dashboard Menu Sections
  ;; ----------------------------------------
  (setq +doom-dashboard-menu-sections
        '(("Recent files"
           :icon "file-text"
           :action find-file
           :items (+doom-dashboard-loaded-files))
          ("Projects"
           :icon "briefcase"
           :action projectile-switch-project
           :items (projectile-recentf-files))
          ("Open config"
           :icon "cog"
           :action (lambda ()
                     (find-file (concat doom-private-dir "config.org"))))
          ;; Uncomment to add Magit shortcut
          ;; ("Magit Status" :icon "git" :action magit-status)
          ;; Uncomment to add Deft notes
          ;; ("Deft Notes" :icon "sticky-note" :action deft)
          ;; Uncomment to add Org-Roam
          ;; ("Org-Roam" :icon "book" :action org-roam)
          ))

  ;; ----------------------------------------
  ;; Icons (only when GUI)
  ;; ----------------------------------------
  (when (display-graphic-p)
    (use-package! all-the-icons
      :defer t))

  ;; ----------------------------------------
  ;; Footer Message
  ;; ----------------------------------------
  ;; (setq +doom-dashboard-footer-messages
  ;;       '("Happy hacking!" "Have a productive day!" "Emacs > VSCode"))

  ;; ----------------------------------------
  ;; Refresh dashboard after Doom startup
  ;; ----------------------------------------
  (add-hook 'doom-after-init-hook #'+doom-dashboard-refresh-buffer)

  ;; ----------------------------------------
  ;; Dashboard as default startup buffer
  ;; ----------------------------------------
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*doom-dashboard*"))))

(provide 'ui-doom-dashboard-config)

;;; ui-doom-dashboard-config.el ends here
