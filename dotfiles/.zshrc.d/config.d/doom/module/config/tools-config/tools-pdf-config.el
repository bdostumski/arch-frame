;;; module/config/tools-config/tools-pdf-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; PDF viewing and annotation configuration using `pdf-tools`.
;; Sets up automatic activation, keybindings for navigation and annotations,
;; and global word wrapping for text modes.

;;; Code:

;; ----------------------------
;; Enable PDF Tools
;; ----------------------------
(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

;; ----------------------------
;; Leader keybindings for PDF actions
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("f" . "file")
;;       :desc "Open PDF" "p" #'find-file
;;       :desc "Scroll up / next page" "k" #'pdf-view-scroll-up-or-next-page
;;       :desc "Scroll down / previous page" "j" #'pdf-view-scroll-down-or-previous-page
;;       :desc "Add highlight annotation" "a" #'pdf-annot-add-highlight-markup-annotation))

;; ----------------------------
;; Word wrap configuration
;; ----------------------------
;; Enable soft wrapping globally
(global-visual-line-mode t)
;; Auto-wrap in text modes
(add-hook 'text-mode-hook #'turn-on-auto-fill)
;; Show continuation arrows in the fringe
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(provide 'tools-pdf-config)

;;; tools-pdf-config.el ends here
