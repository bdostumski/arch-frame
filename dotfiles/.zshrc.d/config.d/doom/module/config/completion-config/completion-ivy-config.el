;;; module/config/completion-config/completion-ivy-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Ivy-based completion setup with rich UI enhancements, icons, sorting/persisting,
;; and posframe support for a modern Emacs experience.

;;; Code:

;; ----------------------------
;; Ivy: core incremental completion
;; ----------------------------
(use-package! ivy
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)       ;; include recent files/buffers
  (enable-recursive-minibuffers t)  ;; allow recursive minibuffers
  (ivy-count-format "(%d/%d) "))

;; ----------------------------
;; Ivy-Rich: show extra info in completions
;; ----------------------------
(use-package! ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; ----------------------------
;; All-the-icons integration for Ivy
;; ----------------------------
(use-package! all-the-icons-ivy-rich
  :after ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1))

;; ----------------------------
;; Prescient: smarter sorting and filtering
;; ----------------------------
(use-package! ivy-prescient
  :after ivy
  :init
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)) ;; persist sorting/filtering

;; ----------------------------
;; Posframe: popup completion UI
;; ----------------------------
(use-package! ivy-posframe
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :init
  (ivy-posframe-mode 1))

(provide 'completion-ivy-config)

;;; completion-ivy-config.el ends here
