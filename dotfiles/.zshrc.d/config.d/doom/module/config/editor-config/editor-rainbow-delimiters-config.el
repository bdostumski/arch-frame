;;; module/config/editor-config/editor-rainbow-delimiters-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Rainbow delimiters with custom colors and enhanced parentheses highlighting.

;;; Code:

(use-package! rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (racket-mode . rainbow-delimiters-mode)
         (json-mode . rainbow-delimiters-mode)
         (yaml-mode . rainbow-delimiters-mode))
  :config
  ;; Custom colors for nesting levels
  (custom-set-faces!
    ;; Depth level colors (vibrant and distinct)
    '(rainbow-delimiters-depth-1-face :foreground "#F07178" :weight bold)      ; Red
    '(rainbow-delimiters-depth-2-face :foreground "#FFCB6B" :weight semi-bold) ; Yellow
    '(rainbow-delimiters-depth-3-face :foreground "#C3E88D" :weight semi-bold) ; Green
    '(rainbow-delimiters-depth-4-face :foreground "#82AAFF" :weight semi-bold) ; Blue
    '(rainbow-delimiters-depth-5-face :foreground "#C792EA" :weight semi-bold) ; Purple
    '(rainbow-delimiters-depth-6-face :foreground "#89DDFF" :weight semi-bold) ; Cyan
    '(rainbow-delimiters-depth-7-face :foreground "#F78C6C" :weight semi-bold) ; Orange
    '(rainbow-delimiters-depth-8-face :foreground "#FF5370" :weight semi-bold) ; Pink
    '(rainbow-delimiters-depth-9-face :foreground "#B0BEC5" :weight semi-bold) ; Gray

    ;; Additional depth levels for deeply nested code
    '(rainbow-delimiters-depth-10-face :foreground "#E91E63" :weight semi-bold) ; Deep Pink
    '(rainbow-delimiters-depth-11-face :foreground "#9C27B0" :weight semi-bold) ; Deep Purple
    '(rainbow-delimiters-depth-12-face :foreground "#3F51B5" :weight semi-bold) ; Indigo

    ;; Error states with strong visual feedback
    '(rainbow-delimiters-unmatched-face
      :foreground "#FF0000"
      :background unspecified
      :weight bold
      :underline nil)
    '(rainbow-delimiters-mismatched-face
      :foreground "#FF0000"
      :background "#FFAAAA"
      :weight bold
      :box (:line-width 2 :color "#FF0000"))

    ;; Enhanced parentheses matching
    '(show-paren-match-expression
      :background unspecified
      :foreground unspecified
      :weight bold
      :underline nil)
    '(show-paren-match
      :background unspecified
      :foreground unspecified
      :weight bold
      :box (:line-width 1 :color "#FFD700"))
    '(show-paren-mismatch
      :background unspecified
      :foreground unspecified
      :weight bold
      :box (:line-width 2 :color "#FF0000")))

  ;; Show matching parentheses instantly
  ;; Enhanced show-paren configuration
  (setq show-paren-delay 0                             ; Instant highlighting
        show-paren-style 'expression                   ; Highlight entire expression
        show-paren-when-point-inside-paren t           ; Show when inside parens
        show-paren-when-point-in-periphery t           ; Show when near parens
        show-paren-priority 1000)                      ; High priority for visibility

  ;; Enable show-paren-mode globally
  (show-paren-mode 1)

  ;; Optional: Highlight matching delimiters on hover (if available)
  (when (fboundp 'global-highlight-parentheses-mode)
    (global-highlight-parentheses-mode 1))


  ;; Optional: Add visual feedback for mismatched delimiters
  (setq rainbow-delimiters-highlight-braces-p t
        rainbow-delimiters-highlight-brackets-p t
        rainbow-delimiters-highlight-parens-p t))

;; Additional package: Highlight parentheses for extra visual feedback
(use-package! highlight-parentheses
  :hook ((prog-mode . highlight-parentheses-mode)
         (emacs-lisp-mode . highlight-parentheses-mode))
  :config
  ;; Colors for highlight-parentheses (subtle background highlighting)
  (setq highlight-parentheses-colors '("#FF6B6B" "#4ECDC4" "#45B7D1" "#96CEB4" "#FFEAA7" "#DDA0DD"))
  (setq highlight-parentheses-background-colors nil)  ; Use foreground colors only

  ;; Customize highlight attributes
  (setq highlight-parentheses-attributes '((:weight bold)))

  ;; Delay for highlighting
  (setq highlight-parentheses-delay 0.1))

;; Package: Smartparens for intelligent delimiter handling
(use-package! smartparens
  :hook ((prog-mode . smartparens-mode)
         (org-mode . smartparens-mode))
  :config
  ;; Enable strict mode for better delimiter management
  ;;(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  ;;(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
  ;;(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

  ;; Show smartparens overlays
  (setq sp-show-pair-from-inside t
        sp-show-pair-delay 0
        sp-highlight-pair-overlay t
        sp-highlight-wrap-overlay t
        sp-highlight-wrap-tag-overlay t)

  ;; Custom faces for smartparens
  (custom-set-faces!
    '(sp-show-pair-match-face
      :background "#3A3A3A"
      :foreground "#FFD700"
      :weight bold)
    '(sp-show-pair-mismatch-face
      :background "#FF4444"
      :foreground "#FFFFFF"
      :weight bold)
    '(sp-pair-overlay-face
      :background "#2A2A2A")
    '(sp-wrap-overlay-face
      :background "#1A1A2E"
      :foreground "#FFD700")
    '(sp-wrap-tag-overlay-face
      :background "#16213E"
      :foreground "#82AAFF")))

;; Custom functions for enhanced delimiter management
(defun rainbow-delimiters-toggle-colors ()
  "Toggle between different color schemes for rainbow delimiters."
  (interactive)
  (if (get 'rainbow-delimiters-toggle-colors 'alternative-colors)
      (progn
        ;; Switch back to original colors
        (custom-set-faces!
          '(rainbow-delimiters-depth-1-face :foreground "#F07178" :weight bold)
          '(rainbow-delimiters-depth-2-face :foreground "#FFCB6B" :weight semi-bold)
          '(rainbow-delimiters-depth-3-face :foreground "#C3E88D" :weight semi-bold)
          '(rainbow-delimiters-depth-4-face :foreground "#82AAFF" :weight semi-bold)
          '(rainbow-delimiters-depth-5-face :foreground "#C792EA" :weight semi-bold))
        (put 'rainbow-delimiters-toggle-colors 'alternative-colors nil)
        (message "Switched to default color scheme"))
    ;; Switch to alternative colors
    (custom-set-faces!
      '(rainbow-delimiters-depth-1-face :foreground "#E06C75" :weight bold)      ; Soft Red
      '(rainbow-delimiters-depth-2-face :foreground "#E5C07B" :weight semi-bold) ; Soft Yellow
      '(rainbow-delimiters-depth-3-face :foreground "#98C379" :weight semi-bold) ; Soft Green
      '(rainbow-delimiters-depth-4-face :foreground "#61AFEF" :weight semi-bold) ; Soft Blue
      '(rainbow-delimiters-depth-5-face :foreground "#C678DD" :weight semi-bold) ; Soft Purple
      '(rainbow-delimiters-depth-6-face :foreground "#56B6C2" :weight semi-bold) ; Soft Cyan
      '(rainbow-delimiters-depth-7-face :foreground "#D19A66" :weight semi-bold) ; Soft Orange
      '(rainbow-delimiters-depth-8-face :foreground "#BE5046" :weight semi-bold) ; Soft Pink
      '(rainbow-delimiters-depth-9-face :foreground "#ABB2BF" :weight semi-bold)) ; Soft Gray
    (put 'rainbow-delimiters-toggle-colors 'alternative-colors t)
    (message "Switched to alternative color scheme")))

(defun rainbow-delimiters-increase-contrast ()
  "Increase the contrast of rainbow delimiter colors."
  (interactive)
  (custom-set-faces!
    '(rainbow-delimiters-depth-1-face :foreground "#FF0000" :weight bold :background "#330000")
    '(rainbow-delimiters-depth-2-face :foreground "#FFFF00" :weight bold :background "#333300")
    '(rainbow-delimiters-depth-3-face :foreground "#00FF00" :weight bold :background "#003300")
    '(rainbow-delimiters-depth-4-face :foreground "#0000FF" :weight bold :background "#000033")
    '(rainbow-delimiters-depth-5-face :foreground "#FF00FF" :weight bold :background "#330033"))
  (message "Increased rainbow delimiters contrast"))

(defun rainbow-delimiters-reset-colors ()
  "Reset rainbow delimiters to default colors."
  (interactive)
  (custom-set-faces!
    '(rainbow-delimiters-depth-1-face :foreground "#F07178" :weight bold :background unspecified)
    '(rainbow-delimiters-depth-2-face :foreground "#FFCB6B" :weight semi-bold :background unspecified)
    '(rainbow-delimiters-depth-3-face :foreground "#C3E88D" :weight semi-bold :background unspecified)
    '(rainbow-delimiters-depth-4-face :foreground "#82AAFF" :weight semi-bold :background unspecified)
    '(rainbow-delimiters-depth-5-face :foreground "#C792EA" :weight semi-bold :background unspecified))
  (put 'rainbow-delimiters-toggle-colors 'alternative-colors nil)
  (message "Reset rainbow delimiters to default colors"))

(provide 'editor-rainbow-delimiters-config)

;;; editor-rainbow-delimiters-config.el ends here
