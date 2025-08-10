;;; configurations/lang-config.el --- Language Support Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Language modes, org enhancements, and related tooling.

;;; Code:

;; ----------------------------------------
;; SQL and GraphQL
;; ----------------------------------------

(use-package! ejc-sql
  :commands ejc-sql-mode)

(use-package! graphql-mode
  :mode "\\.graphql\\'")

;; ----------------------------------------
;; LaTeX and Math
;; ----------------------------------------

(use-package! auctex-latexmk)
(use-package! math-preview)
(use-package! cdlatex)

;; ----------------------------------------
;; Org Mode Enhancements
;; ----------------------------------------

(use-package! org-appear
  :hook (org-mode . org-appear-mode))

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-screenshot-method "flameshot gui"))

;; ;; Uncomment and configure if you want Google Calendar sync
;; (use-package! org-gcal
;;   :config
;;   :init
;;   (setq org-gcal-client-id "your-client-id"
;;         org-gcal-client-secret "your-client-secret"
;;         org-gcal-file-alist '(("your-email@gmail.com" .  "~/org/gcal.org"))))

(use-package! org-modern
  :hook (org-mode . org-modern-mode))

(use-package! org-roam-ui
  :after org-roam)

(use-package! org-super-agenda
  :config
  (org-super-agenda-mode))

(use-package! ox-hugo
  :after ox)

;; ----------------------------------------
;; Shell and Scripting Languages
;; ----------------------------------------

(use-package! fish-mode
  :mode "\\.fish\\'")

(use-package! bash-completion
  :after shell)

;; ----------------------------------------
;; JSON and JavaScript
;; ----------------------------------------

(use-package! jq-mode
  :mode "\\.jq\\'")

(use-package! prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

;; ----------------------------------------
;; Web Development
;; ----------------------------------------

(use-package! emmet-mode
  :hook ((html-mode css-mode web-mode) . emmet-mode))

;;; lang-config.el ends here
