;;; module/config/tools-config/tools-rgb-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive RGB and color management for Doom Emacs
;; Enhanced color preview, manipulation, and palette tools for developers
;; Generic configuration for web development, design, and programming

;;; Code:

;; ----------------------------
;; Enhanced RGB Mode Configuration
;; ----------------------------
(use-package! rgb
  :hook ((prog-mode css-mode web-mode) . rgb-mode)
  :config
  ;; Color adjustment settings
  (setq rgb-colorscale-step 5)  ;; Smaller steps for finer control
  
  ;; Better color display
  (setq rgb-ansi-colors t
        rgb-color-model 'hsl)  ;; Use HSL as default model
  
  ;; Enable in more modes where colors are common
  (add-hook 'html-mode-hook #'rgb-mode)
  (add-hook 'scss-mode-hook #'rgb-mode)
  (add-hook 'less-mode-hook #'rgb-mode)
  (add-hook 'stylus-mode-hook #'rgb-mode)
  (add-hook 'conf-mode-hook #'rgb-mode))

;; ----------------------------
;; Color Utilities and Helpers
;; ----------------------------
(defun +rgb/hex-to-rgb (hex)
  "Convert HEX color to RGB values."
  (let* ((hex (string-remove-prefix "#" hex))
         (r (string-to-number (substring hex 0 2) 16))
         (g (string-to-number (substring hex 2 4) 16))
         (b (string-to-number (substring hex 4 6) 16)))
    (list r g b)))

(defun +rgb/rgb-to-hex (r g b)
  "Convert RGB values to HEX color."
  (format "#%02x%02x%02x" r g b))

(defun +rgb/hex-to-hsl (hex)
  "Convert HEX color to HSL values."
  (let* ((rgb (+rgb/hex-to-rgb hex))
         (r (/ (nth 0 rgb) 255.0))
         (g (/ (nth 1 rgb) 255.0))
         (b (/ (nth 2 rgb) 255.0))
         (max-val (max r g b))
         (min-val (min r g b))
         (diff (- max-val min-val))
         (sum (+ max-val min-val))
         (l (/ sum 2.0))
         (s (if (= diff 0) 0
              (if (< l 0.5)
                  (/ diff sum)
                (/ diff (- 2.0 sum)))))
         (h (cond ((= diff 0) 0)
                  ((= max-val r) (mod (/ (- g b) diff) 6))
                  ((= max-val g) (+ (/ (- b r) diff) 2))
                  (t (+ (/ (- r g) diff) 4)))))
    (list (* h 60) (* s 100) (* l 100))))

(defun +rgb/color-at-point ()
  "Get color at point in various formats."
  (interactive)
  (let ((color (thing-at-point 'word t)))
    (when color
      (cond
       ((string-match-p "^#[0-9a-fA-F]\\{6\\}$" color)
        (let* ((rgb (+rgb/hex-to-rgb color))
               (hsl (+rgb/hex-to-hsl color)))
          (with-current-buffer (get-buffer-create "*Color Info*")
            (erase-buffer)
            (insert (format "# Color Information: %s\n\n" color))
            (insert (format "**Hex:** %s\n" color))
            (insert (format "**RGB:** rgb(%d, %d, %d)\n" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
            (insert (format "**HSL:** hsl(%.0f°, %.0f%%, %.0f%%)\n" (nth 0 hsl) (nth 1 hsl) (nth 2 hsl)))
            (insert (format "**CSS RGB:** rgb(%d, %d, %d)\n" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
            (insert (format "**CSS RGBA:** rgba(%d, %d, %d, 1.0)\n" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
            ;;(markdown-mode)
            (display-buffer (current-buffer)))))
       (t
        (message "No valid color found at point"))))))

;; ----------------------------
;; Color Palette Management
;; ----------------------------
(defvar +rgb/color-palettes
  '((material . ("#F44336" "#E91E63" "#9C27B0" "#673AB7" "#3F51B5" "#2196F3"
                "#03A9F4" "#00BCD4" "#009688" "#4CAF50" "#8BC34A" "#CDDC39"
                "#FFEB3B" "#FFC107" "#FF9800" "#FF5722" "#795548" "#9E9E9E"))
    (tailwind . ("#EF4444" "#F97316" "#F59E0B" "#EAB308" "#84CC16" "#22C55E"
                "#10B981" "#14B8A6" "#06B6D4" "#0EA5E9" "#3B82F6" "#6366F1"
                "#8B5CF6" "#A855F7" "#D946EF" "#EC4899" "#F43F5E"))
    (solarized . ("#B58900" "#CB4B16" "#DC322F" "#D33682" "#6C71C4" "#268BD2"
                 "#2AA198" "#859900" "#073642" "#002B36" "#586E75" "#657B83"
                 "#839496" "#93A1A1" "#EEE8D5" "#FDF6E3"))
    (dracula . ("#FF5555" "#FFB86C" "#F1FA8C" "#50FA7B" "#8BE9FD" "#BD93F9"
               "#FF79C6" "#FF6E6E" "#282A36" "#44475A" "#6272A4" "#F8F8F2"))
    (gruvbox . ("#FB4934" "#FE8019" "#FABD2F" "#B8BB26" "#8EC07C" "#83A598"
               "#D3869B" "#FB4934" "#282828" "#3C3836" "#504945" "#665C54"
               "#BDAE93" "#D5C4A1" "#EBDBB2" "#FBF1C7")))
  "Predefined color palettes for development.")

(defun +rgb/show-palette (palette-name)
  "Show color palette with preview."
  (interactive (list (completing-read "Choose palette: " 
                                     (mapcar #'car +rgb/color-palettes))))
  (let ((colors (alist-get (intern palette-name) +rgb/color-palettes)))
    (when colors
      (with-current-buffer (get-buffer-create "*Color Palette*")
        (erase-buffer)
        (insert (format "# %s Color Palette\n\n" (capitalize palette-name)))
        (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        
        (insert "## Colors\n\n")
        (dolist (color colors)
          (let* ((rgb (+rgb/hex-to-rgb color))
                 (hsl (+rgb/hex-to-hsl color)))
            (insert (format "### %s\n" color))
            (insert (format "- **Hex:** `%s`\n" color))
            (insert (format "- **RGB:** `rgb(%d, %d, %d)`\n" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
            (insert (format "- **HSL:** `hsl(%.0f°, %.0f%%, %.0f%%)`\n" (nth 0 hsl) (nth 1 hsl) (nth 2 hsl)))
            (insert "\n")))
        
        (insert "## CSS Variables\n\n")
        (insert "```css\n:root {\n")
        (let ((counter 1))
          (dolist (color colors)
            (insert (format "  --color-%d: %s;\n" counter color))
            (setq counter (1+ counter))))
        (insert "}\n```\n\n")
        
        ;;(markdown-mode)
        (display-buffer (current-buffer))))))

;; ----------------------------
;; Color Generation and Manipulation
;; ----------------------------
(defun +rgb/generate-random-color ()
  "Generate a random hex color."
  (interactive)
  (let ((color (format "#%06x" (random 16777216))))
    (kill-new color)
    (message "🎨 Generated color: %s (copied to clipboard)" color)))

(defun +rgb/lighten-color (hex percent)
  "Lighten HEX color by PERCENT."
  (let* ((rgb (+rgb/hex-to-rgb hex))
         (r (min 255 (+ (nth 0 rgb) (round (* (nth 0 rgb) (/ percent 100.0))))))
         (g (min 255 (+ (nth 1 rgb) (round (* (nth 1 rgb) (/ percent 100.0))))))
         (b (min 255 (+ (nth 2 rgb) (round (* (nth 2 rgb) (/ percent 100.0)))))))
    (+rgb/rgb-to-hex r g b)))

(defun +rgb/darken-color (hex percent)
  "Darken HEX color by PERCENT."
  (let* ((rgb (+rgb/hex-to-rgb hex))
         (r (max 0 (- (nth 0 rgb) (round (* (nth 0 rgb) (/ percent 100.0))))))
         (g (max 0 (- (nth 1 rgb) (round (* (nth 1 rgb) (/ percent 100.0))))))
         (b (max 0 (- (nth 2 rgb) (round (* (nth 2 rgb) (/ percent 100.0)))))))
    (+rgb/rgb-to-hex r g b)))

(defun +rgb/adjust-color-at-point ()
  "Interactively adjust color at point."
  (interactive)
  (let ((color (thing-at-point 'word t)))
    (when (and color (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color))
      (let* ((action (completing-read "Action: " '("Lighten" "Darken" "Random similar")))
             (new-color (pcase action
                         ("Lighten" 
                          (let ((percent (read-number "Lighten by percent: " 20)))
                            (+rgb/lighten-color color percent)))
                         ("Darken"
                          (let ((percent (read-number "Darken by percent: " 20)))
                            (+rgb/darken-color color percent)))
                         ("Random similar"
                          (format "#%06x" (random 16777216))))))
        (when new-color
          (save-excursion
            (let ((bounds (bounds-of-thing-at-point 'word)))
              (delete-region (car bounds) (cdr bounds))
              (insert new-color)))
          (message "🎨 Color changed from %s to %s" color new-color))))))

;; ----------------------------
;; CSS/Web Development Helpers
;; ----------------------------
(defun +rgb/insert-css-color ()
  "Insert CSS color with format selection."
  (interactive)
  (let* ((format (completing-read "Color format: " 
                                 '("hex" "rgb" "rgba" "hsl" "hsla" "css-var")))
         (color (read-string "Color (hex): " "#"))
         (alpha (when (member format '("rgba" "hsla"))
                 (read-string "Alpha (0-1): " "1.0"))))
    
    (when (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color)
      (let* ((rgb (+rgb/hex-to-rgb color))
             (hsl (+rgb/hex-to-hsl color))
             (result (pcase format
                      ("hex" color)
                      ("rgb" (format "rgb(%d, %d, %d)" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
                      ("rgba" (format "rgba(%d, %d, %d, %s)" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) alpha))
                      ("hsl" (format "hsl(%.0f, %.0f%%, %.0f%%)" (nth 0 hsl) (nth 1 hsl) (nth 2 hsl)))
                      ("hsla" (format "hsla(%.0f, %.0f%%, %.0f%%, %s)" (nth 0 hsl) (nth 1 hsl) (nth 2 hsl) alpha))
                      ("css-var" (format "var(--%s)" (read-string "Variable name: " "primary-color"))))))
        (insert result)))))

(defun +rgb/create-gradient ()
  "Create CSS gradient from multiple colors."
  (interactive)
  (let* ((direction (completing-read "Direction: " 
                                    '("to right" "to left" "to bottom" "to top" 
                                      "45deg" "90deg" "180deg" "270deg")))
         (colors '())
         (color ""))
    
    (while (not (string-empty-p (setq color (read-string "Color (hex, empty to finish): " "#"))))
      (when (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color)
        (push color colors)))
    
    (when (>= (length colors) 2)
      (let ((gradient (format "linear-gradient(%s, %s)" 
                             direction 
                             (string-join (reverse colors) ", "))))
        (insert (format "background: %s;" gradient))
        (message "🌈 Gradient created with %d colors" (length colors))))))

;; ----------------------------
;; Color Accessibility Tools
;; ----------------------------
(defun +rgb/check-contrast (color1 color2)
  "Check contrast ratio between two colors (simplified)."
  (interactive "sFirst color (hex): \nsSecond color (hex): ")
  (when (and (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color1)
             (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color2))
    (let* ((rgb1 (+rgb/hex-to-rgb color1))
           (rgb2 (+rgb/hex-to-rgb color2))
           ;; Simplified luminance calculation
           (lum1 (/ (+ (* 0.299 (nth 0 rgb1)) (* 0.587 (nth 1 rgb1)) (* 0.114 (nth 2 rgb1))) 255.0))
           (lum2 (/ (+ (* 0.299 (nth 0 rgb2)) (* 0.587 (nth 1 rgb2)) (* 0.114 (nth 2 rgb2))) 255.0))
           (contrast (if (> lum1 lum2) (/ (+ lum1 0.05) (+ lum2 0.05)) (/ (+ lum2 0.05) (+ lum1 0.05)))))
      
      (with-current-buffer (get-buffer-create "*Color Contrast*")
        (erase-buffer)
        (insert "# Color Contrast Analysis\n\n")
        (insert (format "**Color 1:** %s\n" color1))
        (insert (format "**Color 2:** %s\n\n" color2))
        (insert (format "**Contrast Ratio:** %.2f:1\n\n" contrast))
        
        (insert "## Accessibility Guidelines\n\n")
        (insert (format "- **AA Normal Text:** %s (4.5:1 required)\n" 
                       (if (>= contrast 4.5) "✅ Pass" "❌ Fail")))
        (insert (format "- **AA Large Text:** %s (3:1 required)\n" 
                       (if (>= contrast 3.0) "✅ Pass" "❌ Fail")))
        (insert (format "- **AAA Normal Text:** %s (7:1 required)\n" 
                       (if (>= contrast 7.0) "✅ Pass" "❌ Fail")))
        (insert (format "- **AAA Large Text:** %s (4.5:1 required)\n" 
                       (if (>= contrast 4.5) "✅ Pass" "❌ Fail")))
        
       ;; (markdown-mode)
        (display-buffer (current-buffer))))))

;; ----------------------------
;; Development Workflow Integration
;; ----------------------------
(defun +rgb/extract-colors-from-buffer ()
  "Extract all hex colors from current buffer."
  (interactive)
  (let ((colors '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[0-9a-fA-F]\\{6\\}" nil t)
        (let ((color (match-string 0)))
          (unless (member color colors)
            (push color colors)))))
    
    (if colors
        (with-current-buffer (get-buffer-create "*Buffer Colors*")
          (erase-buffer)
          (insert (format "# Colors in %s\n\n" (buffer-name (other-buffer))))
          (insert (format "**Found:** %d unique colors\n\n" (length colors)))
          
          (insert "## Color List\n\n")
          (dolist (color (sort colors #'string<))
            (let* ((rgb (+rgb/hex-to-rgb color))
                   (hsl (+rgb/hex-to-hsl color)))
              (insert (format "- **%s** - RGB(%d,%d,%d) - HSL(%.0f°,%.0f%%,%.0f%%)\n" 
                             color (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)
                             (nth 0 hsl) (nth 1 hsl) (nth 2 hsl)))))
          
          (insert "\n## CSS Variables\n\n```css\n:root {\n")
          (let ((counter 1))
            (dolist (color (sort colors #'string<))
              (insert (format "  --color-%d: %s;\n" counter color))
              (setq counter (1+ counter))))
          (insert "}\n```\n")
          
          ;;(markdown-mode)
          (display-buffer (current-buffer)))
      (message "No hex colors found in buffer"))))

;; ----------------------------
;; Smart Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("r" . "colors")
                         ;; Basic color operations
                         :desc "Increase color" "+" #'rgb-increase-color
                         :desc "Decrease color" "-" #'rgb-decrease-color
                         :desc "Color at point" "." #'+rgb/color-at-point
                         :desc "Adjust color" "a" #'+rgb/adjust-color-at-point
                         
                         ;; Color generation
                         :desc "Theme colors" "t" #'+rgb/current-theme-colors
                         :desc "Random color" "r" #'+rgb/generate-random-color
                         :desc "Insert CSS color" "i" #'+rgb/insert-css-color
                         :desc "Create gradient" "g" #'+rgb/create-gradient
                         
                         ;; Palettes and collections
                         (:prefix ("p" . "palettes")
                          :desc "Show palette" "s" #'+rgb/show-palette
                          :desc "Extract from buffer" "e" #'+rgb/extract-colors-from-buffer)
                         
                         ;; Accessibility
                         (:prefix ("A" . "accessibility")
                          :desc "Check contrast" "c" #'+rgb/check-contrast)))))

;; Mode-specific keybindings for quick access
(map! :map (css-mode-map scss-mode-map web-mode-map)
      :localleader
      (:prefix ("r" . "colors")
       :desc "Color info" "i" #'+rgb/color-at-point
       :desc "Insert color" "n" #'+rgb/insert-css-color
       :desc "Create gradient" "g" #'+rgb/create-gradient
       :desc "Check contrast" "c" #'+rgb/check-contrast
       :desc "Extract colors" "e" #'+rgb/extract-colors-from-buffer))

;; ----------------------------
;; Enhanced Color Preview
;; ----------------------------
(defun +rgb/setup-color-preview ()
  "Setup enhanced color preview in supported modes."
  (when (featurep 'rgb)
    (font-lock-add-keywords nil
      `(( "#[0-9a-fA-F]\\{6\\}" 0
          (face (:background ,(match-string 0)) :extend t))))))

;; Add to rgb-mode, but also optionally prog-mode for general usage
(add-hook 'rgb-mode-hook #'+rgb/setup-color-preview)
(add-hook 'prog-mode-hook #'+rgb/setup-color-preview)

;; ----------------------------
;; Color Theme Integration
;; ----------------------------
(defun +rgb/current-theme-colors ()
  "Show colors from current Emacs theme."
  (interactive)
  (with-current-buffer (get-buffer-create "*Theme Colors*")
    (erase-buffer)
    (insert "# Current Theme Colors\n\n")
    (insert (format "**Theme:** %s\n" (car custom-enabled-themes)))
    (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
    
    (insert "## Face Colors\n\n")
    (let ((faces '(default mode-line mode-line-inactive header-line
                  font-lock-keyword-face font-lock-function-name-face
                  font-lock-variable-name-face font-lock-string-face
                  font-lock-comment-face font-lock-type-face)))
      (dolist (face faces)
        (let ((fg (face-foreground face))
              (bg (face-background face)))
          (when (and fg (not (equal fg "unspecified")))
            (insert (format "- **%s (fg):** %s\n" face fg)))
          (when (and bg (not (equal bg "unspecified")))
            (insert (format "- **%s (bg):** %s\n" face bg))))))
    
    ;;(markdown-mode)
    (display-buffer (current-buffer))))

;; ----------------------------
;; Performance and Auto-setup
;; ----------------------------
;; Enable RGB mode automatically in color-related files
(add-to-list 'auto-mode-alist '("\\.theme\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.palette\\'" . conf-mode))

;; Message on RGB mode activation
(add-hook 'rgb-mode-hook
          (lambda ()
            (message "🎨 RGB mode enabled. Use SPC e t r for color tools")))

(provide 'tools-rgb-config)

;;; tools-rgb-config.el ends here
