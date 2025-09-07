;;; module/config/ui-config/ui-treemacs-projectile-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full-featured Treemacs configuration for Doom Emacs.
;; Integrates Projectile, icons, Dired, Magit, perspectives, Tab Bar, and keybindings.

;;; Code:

;; ----------------------------
;; Projectile configuration
;; ----------------------------
(use-package! projectile
  :demand t
  :init
  (setq projectile-project-search-path '("~/Workspace/" "~/Documents/")
        projectile-enable-caching t
        projectile-globally-ignored-directories
        '(".git" "node_modules" "vendor" ".cask" ".stack-work")
        projectile-globally-ignored-files
        '("TAGS" "*.pyc" "*.o" "*.class")))
(projectile-mode +1)

;; ----------------------------
;; Core Treemacs
;; ----------------------------
(use-package! treemacs
  :defer t
  :custom
  (treemacs-width 30)
  (treemacs-is-never-other-window t)
  (treemacs-follow-after-init t))

;; ----------------------------
;; Treemacs extensions (lazy-loaded)
;; ----------------------------
(use-package! treemacs-all-the-icons
  :after treemacs
  :commands (treemacs-load-theme)
  :config (treemacs-load-theme "all-the-icons"))

(use-package! treemacs-icons-dired
  :after (treemacs dired)
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package! treemacs-magit :after (treemacs magit))
(use-package! treemacs-persp :after (treemacs persp-mode))
(use-package! treemacs-projectile :after (treemacs projectile))
(use-package! treemacs-tab-bar :after treemacs)

;; ----------------------------
;; Keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("t" . "treemacs")
;;       :desc "Toggle Treemacs" "t" #'treemacs
;;       :desc "Treemacs Find File" "f" #'treemacs-find-file
;;       :desc "Treemacs Follow Mode" "F" #'treemacs-follow-mode))

(provide 'ui-treemacs-projectile-config)

;;; ui-treemacs-projectile-config.el ends here
