;;; module/config/tools-config/git-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Optimized Git integration for Doom Emacs.
;; Provides git-link for generating remote URLs and git-messenger for inline commit info.

;;; Code:

;; ----------------------------
;; git-link: generate URLs to files/lines in remote repos
;; ----------------------------
(use-package! git-link
  :defer t
  :commands (git-link)
  :init
  ;; Keybinding under leader key
  (map! :leader
        :desc "Git link" "g i" #'git-link)
  :config
  ;; Optional: set default remote (auto-detect usually)
  ;; (setq git-link-default-branch "main")
  )

;; ----------------------------
;; git-messenger: show commit info for current line
;; ----------------------------
(use-package! git-messenger
  :defer t
  :commands (git-messenger:popup-message)
  :init
  ;; Keybinding under leader key
  (map! :leader
        :desc "Git messenger" "g m" #'git-messenger:popup-message)
  :config
  ;; Optional: auto-close popup after N seconds
  (setq git-messenger:show-detail t
        git-messenger:popup-time 5))

(provide 'git-config)

;;; git-config.el ends here
