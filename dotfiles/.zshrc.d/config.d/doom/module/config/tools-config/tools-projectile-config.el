;;; module/config/tools-config/tools-projectile-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Projectile configuration for Doom Emacs.
;; Provides project management, caching, ignored files, native indexing,
;; optional integration with deadgrep for project-wide searches, and
;; leader keybindings for easy access to Projectile commands.

;;; Code:

;; ----------------------------
;; Projectile core configuration
;; ----------------------------
(use-package! projectile
  :demand t
  :init
  ;; Default project search paths
  (setq projectile-project-search-path '("~/Workspace/" "~/Documents/Projects/"))
  ;; Enable caching and native indexing for performance
  (setq projectile-enable-caching t
        projectile-indexing-method 'native)
  ;; Globally ignored directories and files
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "vendor" ".cask" ".stack-work" "build" "dist"))
  (setq projectile-globally-ignored-files
        '("TAGS" "*.pyc" "*.o" "*.class")))

;; Enable Projectile globally
(projectile-mode +1)

;; ----------------------------
;; Deadgrep integration
;; ----------------------------
(use-package! deadgrep
  :after projectile
  :commands deadgrep
  :config
  ;; Use Projectile project root as default
  (setq deadgrep-project-root-function #'projectile-project-root))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
(map! :leader
      :desc "Projectile command map" "p" #'projectile-command-map)

(provide 'tools-projectile-config)

;;; tools-projectile-config.el ends here
