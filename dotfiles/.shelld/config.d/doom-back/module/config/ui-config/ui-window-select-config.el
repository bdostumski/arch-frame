;;; module/config/ui-config/ui-window-select-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized window-select configuration for Doom Emacs with Evil (Vim) keybindings.
;; Provides fast visual window switching using letters, compatible with normal mode.

;;; Code:

(after! window-select
  ;; Enable letter hints for window selection
  (setq window-select-enable-shortcuts t
        window-select-delay 0.2                   ;; minimal hint delay
        ;; Vim-friendly keys: home row + easy reach
        window-select-keys '("h" "j" "k" "l" "a" "s" "d" "f"))

  ;; Exclude minibuffer, vterm, or other special modes
  (setq window-select-exclude-modes
        '(vterm-mode minibuffer-mode treemacs-mode dashboard-mode)))

(provide 'ui-window-select-config)

;;; ui-window-select-config.el ends here
