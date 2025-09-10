;;; module/config/editor-config/editor-rainbow-delimiters-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Rainbow delimiters with custom colors and enhanced parentheses highlighting.

;;; Code:

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; Custom colors for nesting levels
  (custom-set-faces!
    '(rainbow-delimiters-depth-1-face :foreground "#F07178")
    '(rainbow-delimiters-depth-2-face :foreground "#FFCB6B")
    '(rainbow-delimiters-depth-3-face :foreground "#C3E88D")
    '(rainbow-delimiters-depth-4-face :foreground "#82AAFF")
    '(rainbow-delimiters-depth-5-face :foreground "#C792EA")
    '(rainbow-delimiters-depth-6-face :foreground "#89DDFF")
    '(rainbow-delimiters-depth-7-face :foreground "#F78C6C")
    '(rainbow-delimiters-depth-8-face :foreground "#FF5370")
    '(rainbow-delimiters-depth-9-face :foreground "#B0BEC5")
    '(rainbow-delimiters-unmatched-face :foreground "#FF0000" :weight bold)
    '(rainbow-delimiters-mismatched-face :foreground "#FF0000" :background "#FFAAAA")
    '(show-paren-match-expression
      :background unspecified
      :foreground unspecified
      :weight bold)
    '(show-paren-match
      :background unspecified
      :foreground unspecified
      :weight bold))

  ;; Show matching parentheses instantly
  (setq show-paren-delay 0
        show-paren-style 'expression  ; Highlight entire expression
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1)
  
  ;; Optional: Add visual feedback for mismatched delimiters
  (setq rainbow-delimiters-highlight-braces-p t
        rainbow-delimiters-highlight-brackets-p t
        rainbow-delimiters-highlight-parens-p t))

(provide 'editor-rainbow-delimiters-config)

;;; editor-rainbow-delimiters-config.el ends here
