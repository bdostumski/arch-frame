;;; module/config/editor-config/ui-evil-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Evil mode setup for Doom Emacs.
;; Provides Vim-like behavior everywhere, with quality-of-life tweaks,
;; surround support, and commentary integration.

;;; Code:

(after! evil
  ;; ----------------------------
  ;; Core Evil settings
  ;; ----------------------------
  (setq evil-want-C-u-scroll t           ;; <C-u> scroll up
        evil-want-C-d-scroll t           ;; <C-d> scroll down
        evil-want-C-i-jump t             ;; <C-i> works like Vim
        evil-respect-visual-line-mode t  ;; respect visual line mode
        evil-move-cursor-back nil        ;; cursor stays at end of yank
        evil-kill-on-visual-paste nil)   ;; don't override kill ring on paste

  ;; ----------------------------
  ;; Keybindings
  ;; ----------------------------
  (map! :n "Y" (cmd! (evil-yank (point) (line-end-position))) ;; Y yanks to EOL
        :n "Q" #'evil-record-macro                             ;; Q for macro
        :v "v" #'er/expand-region)                             ;; better visual selection

  ;; Optional: more ergonomic navigation
  (map! :n "H" #'evil-first-non-blank
        :n "L" #'evil-end-of-line)
  )

;; ----------------------------
;; Evil-surround (like tpope's plugin)
;; ----------------------------
(use-package! evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ----------------------------
;; Evil-commentary (gc for commenting)
;; ----------------------------
(after! evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(provide 'ui-evil-config)

;;; ui-evil-config.el ends here
