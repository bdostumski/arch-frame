;;; module/config/completion-config/completion-company-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Company setup for auto-completion with enhanced UI features.
;; Includes company-box icons and quick, responsive completions.

;;; Code:

;; ----------------------------
;; Company: core completion engine
;; ----------------------------
(use-package! company
  :defer t
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)   ;; start completion after 1 char
  (company-idle-delay 0.1)            ;; fast pop-up
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t))   ;; circular navigation in candidates

;; ----------------------------
;; Company-Box: UI enhancements
;; ----------------------------
(use-package! company-box
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons))

(provide 'completion-company-config)

;;; completion-company-config.el ends here
