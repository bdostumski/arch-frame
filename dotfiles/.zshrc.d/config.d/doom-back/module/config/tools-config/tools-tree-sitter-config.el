;;; module/config/tools-config/tools-tree-sitter-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Enhanced Tree-sitter integration for Doom Emacs
;; Advanced syntax highlighting, code navigation, and intelligent parsing
;; Optimized for multi-language development with progressive performance tuning

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
  
  ;; Expanded language mapping
  (when (boundp 'tree-sitter-major-mode-language-alist)
    (setq tree-sitter-major-mode-language-alist
          '((bash-mode . bash)
            (sh-mode . bash)
            (c-mode . c)
            (c++-mode . cpp)
            (cmake-mode . cmake)
            (css-mode . css)
            (scss-mode . css)
            (go-mode . go)
            (html-mode . html)
            (mhtml-mode . html)
            (java-mode . java)
            (js-mode . javascript)
            (js2-mode . javascript)
            (typescript-mode . typescript)
            (tsx-mode . tsx)
            (jsx-mode . jsx)
            (json-mode . json)
            (python-mode . python)
            (ruby-mode . ruby)
            (rust-mode . rust)
            (rustic-mode . rust)
            (typescript-mode . typescript)
            (yaml-mode . yaml)
            (markdown-mode . markdown)
            (php-mode . php)
            (swift-mode . swift)
            (elixir-mode . elixir)
            (haskell-mode . haskell)
            (lua-mode . lua))))
  
  ;; Enhanced file handling
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
  
  ;; Expanded project languages list
  (setq +tree-sitter/project-languages
        '(javascript typescript python rust go c cpp java
          html css json yaml markdown bash php kotlin swift
          elixir haskell lua))
  
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

;; ----------------------------
;; ENHANCED: Improved Optimization for Different File Sizes
;; ----------------------------
(defun +tree-sitter/optimize-for-file-size ()
  "Optimize Tree-sitter based on file size with progressive adjustments."
  (let ((size (buffer-size))
        (size-mb (/ (buffer-size) (* 1024 1024.0))))
    (cond
     ;; Small files (<500KB): Full tree-sitter functionality
     ((< size (* 500 1024))
      (setq-local tree-sitter-hl-use-font-lock-keywords nil))
     
     ;; Medium files (500KB-2MB): Balanced optimization
     ((< size (* 2 1024 1024))
      (setq-local tree-sitter-hl-use-font-lock-keywords t))
     
     ;; Large files (2MB-10MB): High optimization
     ((< size (* 10 1024 1024))
      (setq-local tree-sitter-hl-use-font-lock-keywords t)
      ;; Disable some expensive operations
      (setq-local tree-sitter-after-change-functions nil)
      (setq-local tree-sitter--outgoing-ranges-invalid nil))
     
     ;; Huge files (>10MB): Maximum optimization or disable
     (t
      (if (yes-or-no-p 
           (format "File is very large (%.1fMB). Disable Tree-sitter? " size-mb))
          (tree-sitter-mode -1)
        ;; Apply maximum optimizations
        (setq-local tree-sitter-hl-use-font-lock-keywords t)
        (setq-local tree-sitter-after-change-functions nil)
        (setq-local tree-sitter--outgoing-ranges-invalid nil)
        (setq-local tree-sitter-hl-default-patterns nil))))
    
    ;; Report optimization level
    (when tree-sitter-mode
      (let ((level (cond
                    ((< size (* 500 1024)) "none")
                    ((< size (* 2 1024 1024)) "light")
                    ((< size (* 10 1024 1024)) "medium")
                    (t "maximum"))))
        (message "🌳 Tree-sitter: Applied %s optimization for %.1fMB file" 
                 level size-mb)))))

;; ----------------------------
;; Enhanced Code Navigation
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
                     (char-count (- end start))
                     (complexity (+tree-sitter/estimate-complexity node)))
                (message "Function: %s | Lines: %d | Chars: %d | Complexity: %s | Start: L%d"
                        type line-count char-count complexity (line-number-at-pos start)))
            (message "No function found at point"))))
    (error (message "Tree-sitter function info not available: %s" err))))

;; ----------------------------
;; NEW: Code Folding Based on Tree-sitter
;; ----------------------------
(defun +tree-sitter/fold-defun ()
  "Fold the current function using tree-sitter."
  (interactive)
  (condition-case err
      (when (and (bound-and-true-p tree-sitter-mode)
                 (fboundp 'tree-sitter-node-at-point))
        (let ((node (+tree-sitter/get-enclosing-function-node)))
          (when node
            (let ((beg (tree-sitter-node-start node))
                  (end (tree-sitter-node-end node)))
              ;; Find the first newline after the function start
              (save-excursion
                (goto-char beg)
                (when (search-forward "{" (+ beg 100) t)
                  (setq beg (point))))
              ;; Create the fold
              (when (fboundp 'vimish-fold)
                (vimish-fold beg end))
              (message "🌳 Function folded")))))
    (error (message "Tree-sitter folding failed: %s" err))))

(defun +tree-sitter/fold-all-defuns ()
  "Fold all functions in buffer using tree-sitter."
  (interactive)
  (condition-case err
      (when (and (bound-and-true-p tree-sitter-mode)
                 (fboundp 'tree-sitter-node-at-point))
        (let ((nodes (+tree-sitter/get-all-function-nodes))
              (count 0))
          (dolist (node nodes)
            (when node
              (let ((beg (tree-sitter-node-start node))
                    (end (tree-sitter-node-end node)))
                ;; Find the first newline after the function start
                (save-excursion
                  (goto-char beg)
                  (when (search-forward "{" (+ beg 100) t)
                    (setq beg (point))))
                ;; Create the fold
                (when (fboundp 'vimish-fold)
                  (vimish-fold beg end)
                  (setq count (1+ count))))))
          (message "🌳 Folded %d functions" count)))
    (error (message "Tree-sitter folding failed: %s" err))))

;; ----------------------------
;; NEW: Enhanced Helper Functions
;; ----------------------------
(defun +tree-sitter/get-enclosing-function-node ()
  "Get the enclosing function node safely with expanded node types."
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
                   "arrow_function" "function_expression"
                   "class_declaration" "class_definition"
                   "method_declaration" "lambda_expression"
                   "constructor_declaration" "anonymous_function"
                   "async_function" "async_arrow_function"))))))))

(defun +tree-sitter/get-all-function-nodes ()
  "Get all function nodes in buffer."
  (when (and (fboundp 'tree-sitter-root-node)
             (fboundp 'tree-sitter-node-children))
    (let ((root (tree-sitter-root-node))
          (function-types '("function_declaration" "function_definition"
                          "method_definition" "function_item"
                          "arrow_function" "function_expression"
                          "class_declaration" "class_definition"))
          (results '()))
      (when root
        ;; Simple query for top-level functions
        (dolist (child (tree-sitter-node-children root))
          (when (memq (tree-sitter-node-type child) function-types)
            (push child results))))
      (nreverse results))))

(defun +tree-sitter/estimate-complexity (node)
  "Estimate complexity of a function node."
  (when (and node (fboundp 'tree-sitter-node-text))
    (let* ((text (tree-sitter-node-text node))
           (if-count (how-many "if\\s-*(" text))
           (for-count (how-many "for\\s-*(" text))
           (while-count (how-many "while\\s-*(" text))
           (switch-count (how-many "switch\\s-*(" text))
           (total-complexity (+ if-count for-count while-count switch-count)))
      (cond
       ((< total-complexity 5) "Low")
       ((< total-complexity 15) "Medium")
       (t "High")))))

;; ----------------------------
;; NEW: Syntax Tree Visualization
;; ----------------------------
(defun +tree-sitter/visualize-current-node ()
  "Visualize syntax tree for the current node."
  (interactive)
  (when (and (bound-and-true-p tree-sitter-mode)
             (fboundp 'tree-sitter-node-at-point))
    (let* ((node (tree-sitter-node-at-point))
           (buf-name "*Tree-sitter Node*"))
      (when node
        (with-current-buffer (get-buffer-create buf-name)
          (erase-buffer)
          (insert "Tree-sitter Node Visualization\n")
          (insert "============================\n\n")
          (insert (format "Type: %s\n" (tree-sitter-node-type node)))
          (insert (format "Start: %d\n" (tree-sitter-node-start node)))
          (insert (format "End: %d\n" (tree-sitter-node-end node)))
          (insert (format "Named: %s\n" (tree-sitter-node-named-p node)))
          (insert "\nText:\n----\n")
          (insert (tree-sitter-node-text node))
          (insert "\n\nTree structure:\n--------------\n")
          (+tree-sitter/print-node-structure node 0)
          (special-mode)
          (display-buffer (current-buffer))))
      (message "Visualizing syntax tree node: %s" 
               (tree-sitter-node-type node)))))

(defun +tree-sitter/print-node-structure (node depth)
  "Print NODE structure with DEPTH indentation."
  (when (and node (fboundp 'tree-sitter-node-children))
    (insert (format "%s%s\n" 
                   (make-string (* depth 2) ?\s)
                   (tree-sitter-node-type node)))
    (dolist (child (tree-sitter-node-children node))
      (+tree-sitter/print-node-structure child (1+ depth)))))

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
    
    (insert "\n## Performance Statistics\n\n")
    (let ((active-buffers 0)
          (optimized-buffers 0))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (bound-and-true-p tree-sitter-mode)
            (setq active-buffers (1+ active-buffers))
            (when (local-variable-p 'tree-sitter-hl-use-font-lock-keywords)
              (setq optimized-buffers (1+ optimized-buffers))))))
      (insert (format "- **Active Tree-sitter Buffers:** %d\n" active-buffers))
      (insert (format "- **Optimized Buffers:** %d\n" optimized-buffers)))
    
    (insert "\n## Quick Actions\n\n")
    (insert "- `SPC e t s g` - Install missing grammars\n")
    (insert "- `SPC e t s s` - Check grammar status\n")
    (insert "- `SPC e t s f` - Function info at point\n")
    (insert "- `SPC e t s v` - Visualize syntax tree\n")
    (insert "- `SPC e t s c` - Fold current function\n")
    (insert "- `SPC e t s a` - Fold all functions\n")
    
    (markdown-mode)
    (display-buffer (current-buffer))))

;; ----------------------------
;; NEW: Performance Monitoring
;; ----------------------------
(defun +tree-sitter/performance-report ()
  "Generate performance report for Tree-sitter."
  (interactive)
  (with-current-buffer (get-buffer-create "*Tree-sitter Performance*")
    (erase-buffer)
    (insert "# Tree-sitter Performance Report\n\n")
    (insert (format "**Generated:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
    
    ;; Buffer statistics
    (let ((total-buffers 0)
          (ts-buffers 0)
          (large-buffers 0)
          (optimized-buffers 0))
      
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (buffer-file-name)
                     (not (string-match-p "^\s*\\*" (buffer-name))))
            (setq total-buffers (1+ total-buffers))
            (when (bound-and-true-p tree-sitter-mode)
              (setq ts-buffers (1+ ts-buffers))
              (when (> (buffer-size) (* 1 1024 1024))
                (setq large-buffers (1+ large-buffers)))
              (when (local-variable-p 'tree-sitter-hl-use-font-lock-keywords)
                (setq optimized-buffers (1+ optimized-buffers)))))))
      
      (insert "## Buffer Statistics\n\n")
      (insert (format "- **Total file buffers:** %d\n" total-buffers))
      (insert (format "- **Tree-sitter enabled:** %d (%.1f%%)\n" 
                     ts-buffers
                     (if (> total-buffers 0)
                         (* 100.0 (/ ts-buffers (float total-buffers)))
                       0.0)))
      (insert (format "- **Large files (>1MB):** %d\n" large-buffers))
      (insert (format "- **Optimized buffers:** %d\n\n" optimized-buffers)))
    
    ;; Performance recommendations
    (insert "## Performance Recommendations\n\n")
    (if (bound-and-true-p tree-sitter-hl-use-font-lock-keywords)
        (insert "⚠️ **Global optimization active:** Tree-sitter using font-lock keywords globally, which may affect highlighting quality.\n\n")
      (insert "✅ **Normal configuration:** Tree-sitter using optimal settings globally.\n\n"))
    
    ;; Language statistics
    (insert "## Language Distribution\n\n")
    (let ((lang-counts (make-hash-table :test 'eq)))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (bound-and-true-p tree-sitter-mode)
            (when-let ((lang (tree-sitter-language)))
              (puthash lang 
                       (1+ (or (gethash lang lang-counts) 0))
                       lang-counts)))))
      
      (let ((langs nil))
        (maphash (lambda (k v) (push (cons k v) langs)) lang-counts)
        (setq langs (sort langs (lambda (a b) (> (cdr a) (cdr b)))))
        
        (if langs
            (dolist (pair langs)
              (insert (format "- **%s:** %d buffer(s)\n" (car pair) (cdr pair))))
          (insert "- No Tree-sitter languages active\n"))))
    
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
      
      ;; Enhanced project language detection
      (when (file-exists-p (expand-file-name "package.json" project-root))
        (setq detected-languages (append detected-languages '(javascript typescript json))))
      
      (when (or (directory-files project-root nil "\\.tsx?$")
                (file-exists-p (expand-file-name "tsconfig.json" project-root)))
        (setq detected-languages (append detected-languages '(typescript tsx))))
      
      (when (directory-files project-root nil "\\.jsx?$")
        (setq detected-languages (append detected-languages '(javascript jsx))))
      
      (when (file-exists-p (expand-file-name "Cargo.toml" project-root))
        (setq detected-languages (append detected-languages '(rust))))
      
      (when (directory-files project-root nil "\\.py$")
        (setq detected-languages (append detected-languages '(python))))
      
      (when (file-exists-p (expand-file-name "go.mod" project-root))
        (setq detected-languages (append detected-languages '(go))))
      
      (when (directory-files project-root nil "\\.php$")
        (setq detected-languages (append detected-languages '(php))))
      
      (when (directory-files project-root nil "\\.rb$")
        (setq detected-languages (append detected-languages '(ruby))))
      
      (when (directory-files project-root nil "\\.java$")
        (setq detected-languages (append detected-languages '(java))))
      
      (when (directory-files project-root nil "\\.kt$")
        (setq detected-languages (append detected-languages '(kotlin))))
      
      (when (or (file-exists-p (expand-file-name "README.md" project-root))
                (directory-files project-root nil "\\.md$"))
        (setq detected-languages (append detected-languages '(markdown))))
      
      (when (or (file-exists-p (expand-file-name "_config.yml" project-root))
                (directory-files project-root nil "\\.ya?ml$"))
        (setq detected-languages (append detected-languages '(yaml))))
      
      ;; Report detected languages
      (when detected-languages
        (message "🌳 Tree-sitter: Detected languages for %s: %s" 
                 project-name
                 (string-join (mapcar #'symbol-name 
                                      (delete-dups detected-languages)) ", "))))))

;; ----------------------------
;; Project Hook (Safe)
;; ----------------------------
(when (featurep 'projectile)
  (add-hook 'projectile-after-switch-project-hook #'+tree-sitter/setup-project-grammars))

;; ----------------------------
;; Error Recovery
;; ----------------------------
(defun +tree-sitter/recover-from-errors ()
  "Recover from Tree-sitter errors with detailed diagnostics."
  (interactive)
  (condition-case err
      (progn
        ;; Capture current state
        (let ((was-active (bound-and-true-p tree-sitter-mode))
              (buffer-name (buffer-name))
              (major-mode-name (symbol-name major-mode))
              (file-size (buffer-size)))
          
          ;; Try to disable first if active
          (when was-active
            (tree-sitter-mode -1)
            (message "🌳 Disabled Tree-sitter in %s" buffer-name))
          
          ;; Wait and try to restart
          (run-with-timer 1 nil
                          (lambda (buf active size mode)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (when (and (featurep 'tree-sitter)
                                          (not (bound-and-true-p tree-sitter-mode))
                                          active)
                                  (tree-sitter-mode 1)
                                  (message "🌳 Tree-sitter: Restarted in %s (%s, %.1fKB)"
                                          (buffer-name) mode (/ size 1024.0))))))
                          (current-buffer) was-active file-size major-mode-name)))
    (error (message "🌳 Tree-sitter recovery failed: %s" err))))

;; ----------------------------
;; NEW: Mode Status Integration
;; ----------------------------
(defun +tree-sitter/mode-line-indicator ()
  "Return a string indicating Tree-sitter status for mode line."
  (when (bound-and-true-p tree-sitter-mode)
    (let* ((lang (tree-sitter-language))
           (optimized (and (local-variable-p 'tree-sitter-hl-use-font-lock-keywords)
                          tree-sitter-hl-use-font-lock-keywords))
           (indicator (if optimized "🌲" "🌳")))
      (format " %s%s" indicator (if lang (concat ":" (symbol-name lang)) "")))))

(when (and (boundp 'doom-modeline-def-modeline) 
           (featurep 'doom-modeline))
  (add-to-list 'doom-modeline-fn-alist
               '(tree-sitter-status . +tree-sitter/mode-line-indicator))
  (doom-modeline-def-segment tree-sitter-status
    "Tree-sitter status indicator."
    (+tree-sitter/mode-line-indicator))
  
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host
          buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
                 repl lsp minor-modes input-method indent-info buffer-encoding
                 major-mode process vcs checker tree-sitter-status)))

(provide 'tools-tree-sitter-config)

;;; tools-tree-sitter-config.el ends here
