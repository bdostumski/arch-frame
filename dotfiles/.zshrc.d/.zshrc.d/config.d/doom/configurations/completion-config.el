;;; configurations/completion-config.el --- Completion Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Ivy-based completion UI and Company-based autocompletion.

;;; Code:

;; -------------------------------
;; Ivy and related enhancements
;; -------------------------------

(use-package! ivy
  :config
  (ivy-mode 1))

(use-package! ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package! all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package! ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))

(use-package! ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (ivy-posframe-mode 1))

;; -------------------------------
;; Company and related UI
;; -------------------------------

(use-package! company
  :hook (after-init . global-company-mode))

(use-package! company-box
  :hook (company-mode . company-box-mode))

;;; completion-config.el ends here
