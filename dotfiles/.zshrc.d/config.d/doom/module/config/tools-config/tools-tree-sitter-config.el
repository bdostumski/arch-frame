;;; module/config/tools-config/tools-tree-sitter-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Fixed Tree-sitter integration for Doom Emacs
;; Enhanced syntax highlighting, code navigation, and intelligent parsing
;; Optimized for multi-language development workflows

;;; Code:

;; ----------------------------
;; Core Tree-sitter Configuration
;; ----------------------------
(use-package! tree-sitter
  :when (and (featurep 'tree-sitter) (treesit-available-p))
  :hook ((prog-mode text-mode) . tree-sitter-mode)
  :init
  ;; Performance optimizations
  (setq tree-sitter-debug-highlight-jump-region nil
        tree-sitter-debug-jump-buttons nil
        tree-sitter-hl-use-font-lock-keywords nil)
  
  :config
  ;; Enable global tree-sitter features safely
  (when (fboundp 'global-tree-sitter-mode)
    (global-tree-sitter-mode))
  
  (when (fboundp 'tree-sitter-hl-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
  
  ;; Safe language mapping
  (when (boundp 'tree-sitter-major-mode-language-alist)
    (setq tree-sitter-major-mode-language-alist
          '((bash-mode . bash)
            (c-mode . c)
            (c++-mode . cpp)
            (css-mode . css)
            (go-mode . go)
            (html-mode . html)
            (java-mode . java)
            (js-mode . javascript)
            (js2-mode . javascript)
            (json-mode . json)
            (python-mode . python)
            (ruby-mode . ruby)
            (rust-mode . rust)
            (typescript-mode . typescript)
            (yaml-mode . yaml))))
  
  ;; Large file handling
  (add-hook 'tree-sitter-mode-hook #'+tree-sitter/optimize-for-file-size))

;; ----------------------------
;; Safe Grammar Management
;; ----------------------------
(use-package! tree-sitter-langs
  :after tree-sitter
  :when (featurep 'tree-sitter-langs)
  :config
  ;; Grammar storage configuration
  (setq tree-sitter-langs-grammar-dir
        (expand-file-name "tree-sitter/grammars/" doom-data-dir))
  
  ;; Create directory if it doesn't exist
  (unless (file-directory-p tree-sitter-langs-grammar-dir)
    (make-directory tree-sitter-langs-grammar-dir t))
  
  ;; Project languages (simplified list)
  (setq +tree-sitter/project-languages
        '(javascript typescript python rust go c cpp
          html css json yaml markdown bash))
  
  ;; Delayed grammar installation
  (run-with-timer 10 nil #'+tree-sitter/ensure-grammars))

;; ----------------------------
;; Safe Helper Functions
;; ----------------------------
(defun +tree-sitter/lang-available-p (lang)
  "Safely check if Tree-sitter language is available."
  (condition-case nil
      (cond
       ;; Try modern tree-sitter-langs function
       ((fboundp 'tree-sitter-langs-available-p)
        (tree-sitter-langs-available-p lang))
       
       ;; Try legacy function
       ((fboundp 'tree-sitter-lang-available-p)
        (tree-sitter-lang-available-p lang))
       
       ;; Fallback: check if grammar file exists
       ((boundp 'tree-sitter-langs-grammar-dir)
        (let ((grammar-file (expand-file-name 
                            (format "%s.so" lang) 
                            tree-sitter-langs-grammar-dir)))
          (file-exists-p grammar-file)))
       
       ;; Default fallback
       (t nil))
    (error nil)))

(defun +tree-sitter/optimize-for-file-size ()
  "Optimize Tree-sitter for large files."
  (when (> (buffer-size) (* 2 1024 1024)) ; 2MB threshold
    (setq-local tree-sitter-hl-use-font-lock-keywords t)
    (message "🌳 Tree-sitter: Optimized for large file (%s)" 
             (file-size-human-readable (buffer-size)))))

;; ----------------------------
;; Enhanced Code Navigation (Safe)
;; ----------------------------
(defun +tree-sitter/goto-function-start ()
  "Navigate to the start of current function."
  (interactive)
  (condition-case err
      (when (and (bound-and-true-p tree-sitter-mode)
                 (fboundp 'tree-sitter-node-at-point)
                 (tree-sitter-node-at-point))
        (let ((node (+tree-sitter/get-enclosing-function-node)))
          (when node
            (goto-char (tree-sitter-node-start node))
            (message "📍 Function start"))))
    (error (message "Tree-sitter navigation not available: %s" err))))

(defun +tree-sitter/goto-function-end ()
  "Navigate to the end of current function."
  (interactive)
  (condition-case err
      (when (and (bound-and-true-p tree-sitter-mode)
                 (fboundp 'tree-sitter-node-at-point)
                 (tree-sitter-node-at-point))
        (let ((node (+tree-sitter/get-enclosing-function-node)))
          (when node
            (goto-char (tree-sitter-node-end node))
            (message "📍 Function end"))))
    (error (message "Tree-sitter navigation not available: %s" err))))

(defun +tree-sitter/function-info ()
  "Show information about function at point."
  (interactive)
  (condition-case err
      (when (and (bound-and-true-p tree-sitter-mode)
                 (fboundp 'tree-sitter-node-at-point)
                 (tree-sitter-node-at-point))
        (let ((node (+tree-sitter/get-enclosing-function-node)))
          (if node
              (let* ((start (tree-sitter-node-start node))
                     (end (tree-sitter-node-end node))
                     (type (tree-sitter-node-type node))
                     (line-count (count-lines start end))
                     (char-count (- end start)))
                (message "Function: %s | Lines: %d | Chars: %d | Start: L%d"
                        type line-count char-count (line-number-at-pos start)))
            (message "No function found at point"))))
    (error (message "Tree-sitter function info not available: %s" err))))

;; ----------------------------
;; Safe Helper Functions
;; ----------------------------
(defun +tree-sitter/get-enclosing-function-node ()
  "Get the enclosing function node safely."
  (when (and (fboundp 'tree-sitter-node-at-point)
             (fboundp 'tree-sitter-parent-until))
    (when-let ((node (tree-sitter-node-at-point)))
      (tree-sitter-parent-until 
       node
       (lambda (n)
         (when (fboundp 'tree-sitter-node-type)
           (memq (tree-sitter-node-type n)
                 '("function_declaration" "function_definition"
                   "method_definition" "function_item"
                   "arrow_function" "function_expression"))))))))

;; ----------------------------
;; Safe Grammar Management
;; ----------------------------
(defun +tree-sitter/ensure-grammars ()
  "Ensure essential grammars are installed safely."
  (interactive)
  (when (and (featurep 'tree-sitter-langs)
             (boundp '+tree-sitter/project-languages))
    (let ((missing-grammars '())
          (total-checked 0)
          (available-count 0))
      
      (dolist (lang +tree-sitter/project-languages)
        (setq total-checked (1+ total-checked))
        (if (+tree-sitter/lang-available-p lang)
            (setq available-count (1+ available-count))
          (push lang missing-grammars)))
      
      (message "🌳 Tree-sitter: %d/%d grammars available" 
               available-count total-checked)
      
      (when missing-grammars
        (message "🌳 Missing grammars: %s" 
                 (string-join (mapcar #'symbol-name missing-grammars) ", "))
        
        (when (y-or-n-p "Install missing grammars? ")
          (dolist (lang missing-grammars)
            (condition-case err
                (cond
                 ((fboundp 'tree-sitter-langs-install-grammars)
                  (tree-sitter-langs-install-grammars lang)
                  (message "✅ Installed grammar for %s" lang))
                 (t
                  (message "⚠️ No installation function available for %s" lang)))
              (error (message "❌ Failed to install grammar for %s: %s" lang err)))))))))

(defun +tree-sitter/check-grammar-status ()
  "Check status of Tree-sitter grammars safely."
  (interactive)
  (with-current-buffer (get-buffer-create "*Tree-sitter Status*")
    (erase-buffer)
    (insert "# Tree-sitter Grammar Status\n\n")
    (insert (format "**Date:** %s\n" (format-time-string "%Y-%m-%d %H:%M")))
    (insert (format "**User:** %s\n" (user-login-name)))
    (insert (format "**Tree-sitter Available:** %s\n" 
                    (if (featurep 'tree-sitter) "✅ Yes" "❌ No")))
    (insert (format "**Tree-sitter Langs Available:** %s\n\n" 
                    (if (featurep 'tree-sitter-langs) "✅ Yes" "❌ No")))
    
    (when (boundp 'tree-sitter-langs-grammar-dir)
      (insert (format "**Grammar Directory:** %s\n" tree-sitter-langs-grammar-dir))
      (insert (format "**Directory Exists:** %s\n\n" 
                      (if (file-directory-p tree-sitter-langs-grammar-dir) "✅ Yes" "❌ No"))))
    
    (when (boundp '+tree-sitter/project-languages)
      (insert "## Project Languages\n\n")
      (dolist (lang +tree-sitter/project-languages)
        (let ((available (+tree-sitter/lang-available-p lang)))
          (insert (format "- **%s**: %s\n" 
                         lang 
                         (if available "✅ Available" "❌ Missing"))))))
    
    (insert "\n## Quick Actions\n\n")
    (insert "- `SPC e t s g` - Install missing grammars\n")
    (insert "- `SPC e t s s` - Check grammar status\n")
    (insert "- `SPC e t s f` - Function info at point\n")
    
    (markdown-mode)
    (display-buffer (current-buffer))))

;; ----------------------------
;; Project Integration (Safe)
;; ----------------------------
(defun +tree-sitter/setup-project-grammars ()
  "Setup Tree-sitter grammars for current project safely."
  (when (and (featurep 'tree-sitter)
             (featurep 'projectile)
             (fboundp 'projectile-project-root)
             (projectile-project-root))
    (let ((project-name (projectile-project-name))
          (project-root (projectile-project-root))
          (detected-languages '()))
      
      ;; Detect languages based on project files
      (when (file-exists-p (expand-file-name "package.json" project-root))
        (setq detected-languages (append detected-languages '(javascript typescript json))))
      
      (when (file-exists-p (expand-file-name "Cargo.toml" project-root))
        (setq detected-languages (append detected-languages '(rust))))
      
      (when (directory-files project-root nil "\\.py$")
        (setq detected-languages (append detected-languages '(python))))
      
      (when (file-exists-p (expand-file-name "go.mod" project-root))
        (setq detected-languages (append detected-languages '(go))))
      
      (when (or (file-exists-p (expand-file-name "README.md" project-root))
                (file-exists-p (expand-file-name "_config.yml" project-root)))
        (setq detected-languages (append detected-languages '(markdown yaml))))
      
      ;; Report detected languages
      (when detected-languages
        (message "🌳 Tree-sitter: Detected languages for %s: %s" 
                 project-name
                 (string-join (mapcar #'symbol-name detected-languages) ", "))))))

;; ----------------------------
;; Project Hook (Safe)
;; ----------------------------
(when (featurep 'projectile)
  (add-hook 'projectile-after-switch-project-hook #'+tree-sitter/setup-project-grammars))

;; ----------------------------
;; Error Recovery
;; ----------------------------
(defun +tree-sitter/recover-from-errors ()
  "Recover from Tree-sitter errors."
  (interactive)
  (condition-case err
      (progn
        (when (bound-and-true-p tree-sitter-mode)
          (tree-sitter-mode -1))
        (run-with-timer 1 nil
                        (lambda ()
                          (when (and (featurep 'tree-sitter)
                                     (memq major-mode '(js-mode js2-mode python-mode rust-mode)))
                            (tree-sitter-mode 1)
                            (message "🌳 Tree-sitter: Mode restarted")))))
    (error (message "Tree-sitter recovery failed: %s" err))))

;; ----------------------------
;; Safe Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("s" . "tree-sitter")
                         ;; Navigation
                         :desc "Function start" "f" #'+tree-sitter/goto-function-start
                         :desc "Function end" "e" #'+tree-sitter/goto-function-end
                         :desc "Function info" "i" #'+tree-sitter/function-info
                         
                         ;; Management
                         :desc "Recover from errors" "r" #'+tree-sitter/recover-from-errors
                         :desc "Grammar status" "s" #'+tree-sitter/check-grammar-status
                         :desc "Install grammars" "g" #'+tree-sitter/ensure-grammars))))

(provide 'tools-tree-sitter-config)

;;; tools-tree-sitter-config.el ends here
