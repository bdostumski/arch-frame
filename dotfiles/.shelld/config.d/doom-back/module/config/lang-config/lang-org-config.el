;;; module/config/lang-config/lang-org-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Org-mode configuration with:
;;  - Org-roam, Org-download, Org-appear, Org-modern
;;  - Org-super-agenda, Ox-Hugo, optional Google Calendar integration
;; Provides:
;;  - Visual enhancements, drag-and-drop images
;;  - Export support, leader keybindings for common Org actions

;;; Code:

;; ----------------------------
;; Global Org settings
;; ----------------------------
(setq org-directory "~/Documents/org/"
      org-startup-indented t
      org-startup-with-inline-images t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-ellipsis " ▾"
      org-image-actual-width '(300))

;; ----------------------------
;; Org-appear: auto reveal emphasis markers
;; ----------------------------
(use-package! org-appear
  :hook (org-mode . org-appear-mode))

;; ----------------------------
;; Org-download: drag-and-drop images
;; ----------------------------
(use-package! org-download
  :after org
  :hook (org-mode . org-download-enable)
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-screenshot-method "flameshot gui"))

;; ----------------------------
;; Org-modern: visual improvements
;; ----------------------------
(use-package! org-modern
  :hook (org-mode . org-modern-mode))

;; ----------------------------
;; Org-roam v2
;; ----------------------------
(after! org-roam
  (setq org-roam-directory "~/Documents/org/roam/")
  (org-roam-db-autosync-mode))

;; ----------------------------
;; Org-roam UI
;; ----------------------------
(use-package! org-roam-ui
  :after org-roam)

;; ----------------------------
;; Org-super-agenda
;; ----------------------------
(use-package! org-super-agenda
  :config
  (org-super-agenda-mode))

;; ----------------------------
;; Ox-Hugo: export to Hugo
;; ----------------------------
(use-package! ox-hugo
  :after ox
  :config
  (setq org-hugo-base-dir "~/Documents/hugo-site/"
        org-hugo-section "posts"))

;; ----------------------------
;; Optional Google Calendar sync
;; ----------------------------
;; (use-package! org-gcal
;;   :config
;;   (setq org-gcal-client-id "your-client-id"
;;         org-gcal-client-secret "your-client-secret"
;;         org-gcal-file-alist '(("your-email@gmail.com" .  "~/Documents/org/gcal.org"))))

(provide 'lang-org-config)
;;; lang-org-config.el ends here
