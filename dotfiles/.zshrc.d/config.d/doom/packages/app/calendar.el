;;; packages/app/calendar.el -*- lexical-binding: t; -*-

(use-package! org-gcal
  :defer t
  :init
  (setq org-gcal-client-id "CLIENT-ID"
        org-gcal-client-secret "CLIENT-SECRET"
        org-gcal-file-alist '(("b.dostumski@gmail.com" .  "~/Documents/doom/org/gcal.org"))))
