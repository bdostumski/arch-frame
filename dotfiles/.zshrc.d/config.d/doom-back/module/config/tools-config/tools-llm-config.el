;;; module/config/tools-config/tools-llm-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Safe LLM and AI integration for Doom Emacs
;; Works without external dependencies, provides useful AI-assisted tools

;;; Code:

;; ----------------------------
;; Safe AI Tools (No External Dependencies)
;; ----------------------------

;; Skip Copilot entirely to avoid errors
;; We'll build useful AI-assisted development tools instead

;; ----------------------------
;; Code Analysis and Documentation Tools
;; ----------------------------
(defun +ai/explain-code ()
  "Create a template to explain selected code."
  (interactive)
  (if (use-region-p)
      (let ((code (buffer-substring-no-properties (region-beginning) (region-end)))
            (lang (or (cdr (assq major-mode
                                '((js2-mode . "javascript")
                                  (typescript-mode . "typescript")
                                  (python-mode . "python")
                                  (rust-mode . "rust")
                                  (emacs-lisp-mode . "elisp")
                                  (go-mode . "go"))))
                     "code")))
        (with-current-buffer (get-buffer-create "*Code Explanation*")
          (erase-buffer)
          (insert (format "# Code Explanation\n\n"))
          (insert (format "**Language:** %s\n" lang))
          (insert (format "**Project:** %s\n\n" (or (projectile-project-name) "Unknown")))
          (insert "## Original Code\n\n")
          (insert (format "```%s\n" lang))
          (insert code)
          (insert "\n```\n\n")
          (insert "## What it does:\n\n")
          (insert "## How it works:\n\n")
          (insert "1. \n")
          (insert "2. \n")
          (insert "3. \n\n")
          (insert "## Key concepts:\n\n")
          (insert "- \n")
          (insert "- \n\n")
          (insert "## Potential improvements:\n\n")
          (insert "- \n")
          (markdown-mode)
          (goto-char (point-min))
          (search-forward "## What it does:")
          (end-of-line)
          (display-buffer (current-buffer))))
    (message "Please select code to explain")))

(defun +ai/create-function-docs ()
  "Create documentation template for function at point."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (project (projectile-project-name))
         (lang (cdr (assq major-mode
                         '((js2-mode . "JavaScript")
                           (typescript-mode . "TypeScript")
                           (python-mode . "Python")
                           (rust-mode . "Rust")
                           (emacs-lisp-mode . "Emacs Lisp")
                           (go-mode . "Go"))))))
    (with-current-buffer (get-buffer-create "*Function Documentation*")
      (erase-buffer)
      (insert (format "# Function Documentation: %s\n\n" (or symbol "Function")))
      (when lang
        (insert (format "**Language:** %s\n" lang)))
      (when project
        (insert (format "**Project:** %s\n" project)))
      (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d")))
      
      ;; Language-specific documentation templates
      (cond
       ((eq major-mode 'js2-mode)
        (insert "## JSDoc Format\n\n")
        (insert "```javascript\n")
        (insert "/**\n")
        (insert " * Brief description of the function\n")
        (insert " * @param {type} paramName - Description\n")
        (insert " * @returns {type} Description of return value\n")
        (insert " * @example\n")
        (insert " * // Usage example\n")
        (insert " */\n")
        (insert "```\n\n"))
       
       ((eq major-mode 'python-mode)
        (insert "## Python Docstring Format\n\n")
        (insert "```python\n")
        (insert "\"\"\"\n")
        (insert "Brief description of the function.\n\n")
        (insert "Args:\n")
        (insert "    param_name (type): Description\n\n")
        (insert "Returns:\n")
        (insert "    type: Description of return value\n\n")
        (insert "Example:\n")
        (insert "    >>> example_usage()\n")
        (insert "    expected_output\n")
        (insert "\"\"\"\n")
        (insert "```\n\n"))
       
       (t
        (insert "## Documentation Format\n\n")
        (insert "```\n")
        (insert "Purpose: \n")
        (insert "Parameters: \n")
        (insert "Returns: \n")
        (insert "Example: \n")
        (insert "```\n\n")))
      
      (insert "## Purpose\n\n")
      (insert "## Parameters\n\n")
      (insert "| Parameter | Type | Description | Required |\n")
      (insert "|-----------|------|-------------|----------|\n")
      (insert "| | | | |\n\n")
      (insert "## Returns\n\n")
      (insert "## Examples\n\n")
      (insert "## Notes\n\n")
      (markdown-mode)
      (display-buffer (current-buffer)))))

;; ----------------------------
;; Smart Commit Message Generation
;; ----------------------------
(defun +ai/smart-commit-message ()
  "Generate commit message suggestions based on project and changes."
  (interactive)
  (when (and (executable-find "git") (magit-git-repo-p))
    (let* ((project-root (projectile-project-root))
           (project-name (projectile-project-name))
           (staged-files (split-string (shell-command-to-string "git diff --staged --name-only") "\n" t))
           (file-types (mapcar (lambda (f) (file-name-extension f)) staged-files)))
      
      (if (null staged-files)
          (message "No staged changes to analyze")
        (with-current-buffer (get-buffer-create "*Commit Message Generator*")
          (erase-buffer)
          (insert "# Smart Commit Message Generator\n\n")
          (insert (format "**Project:** %s\n" (or project-name "Unknown")))
          (insert (format "**Files changed:** %d\n" (length staged-files)))
          (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
          
          ;; Project-specific commit message templates
          (cond
           ;; arch-frame repository - system configuration
           ((and project-root (string-match-p "arch-frame" project-root))
            (insert "## Arch Linux Configuration Templates\n\n")
            (insert "### System Configuration\n")
            (insert "```\n")
            (insert "config(system): update [component] configuration\n")
            (insert "config(wm): configure [window manager] settings\n")
            (insert "config(shell): update shell configuration\n")
            (insert "config(dev): setup development environment\n")
            (insert "```\n\n")
            (insert "### Package Management\n")
            (insert "```\n")
            (insert "pkg: add [package] for [purpose]\n")
            (insert "pkg: remove unused [package]\n")
            (insert "pkg: update package configuration\n")
            (insert "```\n\n")
            (insert "### Fixes and Improvements\n")
            (insert "```\n")
            (insert "fix(system): resolve [issue] in [component]\n")
            (insert "perf(system): optimize [component] performance\n")
            (insert "docs(setup): update installation documentation\n")
            (insert "```\n\n"))
           
           ;; Learning repositories
           ((and project-root (string-match-p "learning-" project-root))
            (insert "## Learning Repository Templates\n\n")
            (insert "### Learning Progress\n")
            (insert "```\n")
            (insert "learn([topic]): [what you learned/practiced]\n")
            (insert "practice([concept]): implement [exercise/example]\n")
            (insert "notes([topic]): add learning notes and examples\n")
            (insert "exercise([concept]): complete [specific exercise]\n")
            (insert "```\n\n")
            (insert "### Code Examples\n")
            (insert "```\n")
            (insert "example([topic]): add [concept] demonstration\n")
            (insert "demo([feature]): create working example\n")
            (insert "snippet([concept]): add useful code snippet\n")
            (insert "```\n\n")
            (insert "### Documentation\n")
            (insert "```\n")
            (insert "docs([topic]): document [concept] with examples\n")
            (insert "readme: update learning progress\n")
            (insert "guide([topic]): create step-by-step guide\n")
            (insert "```\n\n"))
           
           ;; GitHub Pages
           ((and project-root (string-match-p "bdostumski\\.github\\.io" project-root))
            (insert "## GitHub Pages Templates\n\n")
            (insert "### Content Updates\n")
            (insert "```\n")
            (insert "content: add [post/page] about [topic]\n")
            (insert "content: update [section] with [new info]\n")
            (insert "post: add blog post - [title]\n")
            (insert "update: refresh [page] content\n")
            (insert "```\n\n")
            (insert "### Design and Layout\n")
            (insert "```\n")
            (insert "style: improve [component] styling\n")
            (insert "layout: update [page] layout\n")
            (insert "design: enhance visual [element]\n")
            (insert "responsive: improve mobile [component]\n")
            (insert "```\n\n")
            (insert "### Site Maintenance\n")
            (insert "```\n")
            (insert "fix: resolve [issue] on [page]\n")
            (insert "seo: improve [page] SEO\n")
            (insert "perf: optimize [component] performance\n")
            (insert "```\n\n"))
           
           ;; Profile repository
           ((and project-root (string-match-p "/bdostumski$" project-root))
            (insert "## Profile Repository Templates\n\n")
            (insert "```\n")
            (insert "profile: update README with [new info]\n")
            (insert "stats: add [achievement/project] to profile\n")
            (insert "skills: update [technology] experience\n")
            (insert "projects: add [project] to showcase\n")
            (insert "docs: improve profile documentation\n")
            (insert "```\n\n"))
           
           ;; JavaScript learning
           ((and project-root (string-match-p "learning-javascript" project-root))
            (insert "## JavaScript Learning Templates\n\n")
            (insert "```\n")
            (insert "js([topic]): learn [concept/feature]\n")
            (insert "es6([feature]): practice [ES6+ feature]\n")
            (insert "async([concept]): study [async concept]\n")
            (insert "dom([manipulation]): practice DOM [operation]\n")
            (insert "framework([name]): explore [framework/library]\n")
            (insert "project([name]): build [project description]\n")
            (insert "```\n\n"))
           
           ;; General templates
           (t
            (insert "## General Templates\n\n")
            (insert "```\n")
            (insert "feat: add [new feature]\n")
            (insert "fix: resolve [issue description]\n")
            (insert "docs: update documentation\n")
            (insert "style: improve code formatting\n")
            (insert "refactor: restructure [component]\n")
            (insert "test: add tests for [component]\n")
            (insert "chore: update [maintenance task]\n")
            (insert "```\n\n")))
          
          ;; File analysis
          (when staged-files
            (insert "## Changed Files Analysis\n\n")
            (insert "```\n")
            (dolist (file staged-files)
              (insert (format "%s\n" file)))
            (insert "```\n\n")
            
            ;; Suggest commit type based on file extensions
            (insert "## Suggested Commit Types\n\n")
            (cond
             ((member "md" file-types)
              (insert "- `docs:` - Documentation changes detected\n"))
             ((member "js" file-types)
              (insert "- `feat:` or `fix:` - JavaScript changes detected\n"))
             ((member "css" file-types)
              (insert "- `style:` - Styling changes detected\n"))
             ((member "json" file-types)
              (insert "- `config:` - Configuration changes detected\n")))
            (insert "\n"))
          
          (insert "## Quick Actions\n\n")
          (insert "1. Copy a template above\n")
          (insert "2. Customize with your specific changes\n")
          (insert "3. Use in your commit message\n\n")
          (insert "**Tip:** Be specific about what changed and why!\n")
          
          (markdown-mode)
          (display-buffer (current-buffer)))))))

;; ----------------------------
;; Learning Tools
;; ----------------------------
(defun +ai/create-learning-notes ()
  "Create learning notes template optimized."
  (interactive)
  (let* ((topic (or (thing-at-point 'symbol t)
                   (read-string "Learning topic: ")))
         (project (projectile-project-name))
         (is-js-learning (and project (string-match-p "javascript" project))))
    
    (with-current-buffer (get-buffer-create "*Learning Notes*")
      (erase-buffer)
      (insert (format "# Learning Notes: %s\n\n" topic))
      (insert (format "**Date:** %s\n" (format-time-string "%Y-%m-%d")))
      (insert (format "**Project:** %s\n" (or project "General Learning")))
      (when is-js-learning
        (insert "**Focus:** JavaScript Learning\n"))
      (insert "\n---\n\n")
      
      ;; JavaScript-specific template
      (if is-js-learning
          (progn
            (insert "## JavaScript Concept Overview\n\n")
            (insert "### What is it?\n")
            (insert "- \n\n")
            (insert "### Why is it important?\n")
            (insert "- \n\n")
            (insert "### How does it work?\n")
            (insert "1. \n")
            (insert "2. \n")
            (insert "3. \n\n")
            (insert "## Code Examples\n\n")
            (insert "### Basic Example\n")
            (insert "```javascript\n")
            (insert "// Basic usage\n")
            (insert "\n")
            (insert "```\n\n")
            (insert "### Advanced Example\n")
            (insert "```javascript\n")
            (insert "// More complex usage\n")
            (insert "\n")
            (insert "```\n\n")
            (insert "### Real-world Application\n")
            (insert "```javascript\n")
            (insert "// How you might use this in a project\n")
            (insert "\n")
            (insert "```\n\n")
            (insert "## Common Patterns\n\n")
            (insert "- \n")
            (insert "- \n\n")
            (insert "## Common Mistakes\n\n")
            (insert "- ❌ \n")
            (insert "- ❌ \n\n")
            (insert "## Best Practices\n\n")
            (insert "- ✅ \n")
            (insert "- ✅ \n\n"))
        
        ;; General learning template
        (progn
          (insert "## Key Concepts\n\n")
          (insert "### Main Ideas\n")
          (insert "- \n")
          (insert "- \n")
          (insert "- \n\n")
          (insert "### Important Details\n")
          (insert "- \n")
          (insert "- \n\n")
          (insert "## Code Examples\n\n")
          (insert "```\n")
          (insert "\n")
          (insert "```\n\n")
          (insert "## How it Works\n\n")
          (insert "1. \n")
          (insert "2. \n")
          (insert "3. \n\n")))
      
      ;; Common sections for all learning notes
      (insert "## Practice Tasks\n\n")
      (insert "- [ ] Read documentation\n")
      (insert "- [ ] Try basic examples\n")
      (insert "- [ ] Build a small project\n")
      (insert "- [ ] Explain to someone else\n\n")
      
      (insert "## Questions to Explore\n\n")
      (insert "- [ ] \n")
      (insert "- [ ] \n\n")
      
      (insert "## Resources\n\n")
      (insert "### Documentation\n")
      (insert "- \n\n")
      (insert "### Tutorials\n")
      (insert "- \n\n")
      (insert "### Examples\n")
      (insert "- \n\n")
      
      (insert "## Progress Log\n\n")
      (insert (format "**%s:** Started learning %s\n" 
                     (format-time-string "%Y-%m-%d") topic))
      (insert "- \n\n")
      
      (insert "---\n\n")
      (insert "*Keep learning and practicing! 🚀*\n")
      
      (markdown-mode)
      (goto-char (point-min))
      (search-forward "What is it?" nil t)
      (end-of-line)
      (display-buffer (current-buffer)))))

(defun +ai/generate-practice-exercises ()
  "Generate practice exercises template for current programming concept."
  (interactive)
  (let* ((concept (or (thing-at-point 'symbol t)
                     (read-string "Programming concept: ")))
         (project (projectile-project-name))
         (lang (cond
                ((string-match-p "javascript" (or project "")) "JavaScript")
                ((eq major-mode 'python-mode) "Python")
                ((eq major-mode 'rust-mode) "Rust")
                (t "Programming"))))
    
    (with-current-buffer (get-buffer-create "*Practice Exercises*")
      (erase-buffer)
      (insert (format "# Practice Exercises: %s\n\n" concept))
      (insert (format "**Language:** %s\n" lang))
      (insert (format "**Difficulty:** Progressive (Beginner → Advanced)\n"))
      (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d")))
      
      (insert "## 🟢 Beginner Level\n\n")
      (insert "### Exercise 1: Basic Understanding\n")
      (insert (format "**Goal:** Understand the fundamentals of %s\n\n" concept))
      (insert "**Tasks:**\n")
      (insert "- [ ] Read about the concept\n")
      (insert "- [ ] Write a simple example\n")
      (insert "- [ ] Test your understanding\n\n")
      (insert "**Starter Code:**\n")
      (insert (format "```%s\n" (downcase lang)))
      (insert "// Write your basic example here\n")
      (insert "\n")
      (insert "```\n\n")
      (insert "**Success Criteria:**\n")
      (insert "- [ ] Code runs without errors\n")
      (insert "- [ ] You can explain what happens\n\n")
      
      (insert "## 🟡 Intermediate Level\n\n")
      (insert "### Exercise 2: Practical Application\n")
      (insert (format "**Goal:** Apply %s in a realistic scenario\n\n" concept))
      (insert "**Tasks:**\n")
      (insert "- [ ] Create a more complex example\n")
      (insert "- [ ] Handle edge cases\n")
      (insert "- [ ] Add error handling\n\n")
      (insert "**Challenge:**\n")
      (insert (format "```%s\n" (downcase lang)))
      (insert "// Implement a practical solution using the concept\n")
      (insert "\n")
      (insert "```\n\n")
      (insert "**Success Criteria:**\n")
      (insert "- [ ] Handles multiple scenarios\n")
      (insert "- [ ] Robust error handling\n")
      (insert "- [ ] Clean, readable code\n\n")
      
      (insert "## 🔴 Advanced Level\n\n")
      (insert "### Exercise 3: Expert Implementation\n")
      (insert (format "**Goal:** Master %s with optimization and best practices\n\n" concept))
      (insert "**Tasks:**\n")
      (insert "- [ ] Optimize for performance\n")
      (insert "- [ ] Follow best practices\n")
      (insert "- [ ] Add comprehensive testing\n")
      (insert "- [ ] Document your solution\n\n")
      (insert "**Real-world Challenge:**\n")
      (insert "Build a production-ready implementation that could be used in a real project.\n\n")
      (insert "**Success Criteria:**\n")
      (insert "- [ ] Performant and scalable\n")
      (insert "- [ ] Well-tested\n")
      (insert "- [ ] Properly documented\n")
      (insert "- [ ] Follows industry standards\n\n")
      
      (insert "## 📝 Learning Checklist\n\n")
      (insert "Track your progress through these exercises:\n\n")
      (insert "- [ ] **Understand:** Can explain the concept clearly\n")
      (insert "- [ ] **Apply:** Can use it in simple scenarios\n")
      (insert "- [ ] **Analyze:** Can identify when to use it\n")
      (insert "- [ ] **Create:** Can build complex solutions\n")
      (insert "- [ ] **Teach:** Can explain it to others\n\n")
      
      (insert "## 🎯 Next Steps\n\n")
      (insert "After completing these exercises:\n\n")
      (insert "1. [ ] Review your solutions\n")
      (insert "2. [ ] Refactor for improvement\n")
      (insert "3. [ ] Share your learning\n")
      (insert "4. [ ] Find related concepts to explore\n\n")
      
      (insert "---\n\n")
      (insert "*Remember: The goal is understanding, not just completion! 🧠*\n")
      
      (markdown-mode)
      (display-buffer (current-buffer)))))

;; ----------------------------
;; Project Analyzer
;; ----------------------------
(defun +ai/analyze-project-structure ()
  "Analyze current project structure and suggest improvements."
  (interactive)
  (when-let ((root (projectile-project-root)))
    (let* ((project-name (projectile-project-name))
           (files (projectile-current-project-files))
           (file-extensions (delete-dups 
                           (mapcar #'file-name-extension 
                                  (remove nil files))))
           (has-readme (seq-some (lambda (f) (string-match-p "readme" (downcase f))) files))
           (has-gitignore (member ".gitignore" files))
           (has-license (seq-some (lambda (f) (string-match-p "license" (downcase f))) files)))
      
      (with-current-buffer (get-buffer-create "*Project Analysis*")
        (erase-buffer)
        (insert (format "# Project Analysis: %s\n\n" project-name))
        (insert (format "**Path:** %s\n" root))
        (insert (format "**Files:** %d\n" (length files)))
        (insert (format "**Date:** %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        
        (insert "## Project Type Detection\n\n")
        (cond
         ((string-match-p "arch-frame" root)
          (insert "🐧 **Type:** Arch Linux Configuration\n")
          (insert "**Purpose:** System configuration and dotfiles\n"))
         ((string-match-p "learning-" root)
          (insert "📚 **Type:** Learning Repository\n")
          (insert "**Purpose:** Educational content and practice\n"))
         ((string-match-p "bdostumski\\.github\\.io" root)
          (insert "🌐 **Type:** GitHub Pages Website\n")
          (insert "**Purpose:** Personal website and blog\n"))
         ((string-match-p "/bdostumski$" root)
          (insert "👤 **Type:** Profile Repository\n")
          (insert "**Purpose:** GitHub profile README\n"))
         (t
          (insert "💻 **Type:** General Project\n")))
        (insert "\n")
        
        (insert "## File Analysis\n\n")
        (insert "### Languages/Technologies Detected\n")
        (dolist (ext file-extensions)
          (when ext
            (insert (format "- `.%s` files\n" ext))))
        (insert "\n")
        
        (insert "### Project Health Check\n")
        (insert (format "- %s README file\n" (if has-readme "✅" "❌")))
        (insert (format "- %s .gitignore file\n" (if has-gitignore "✅" "❌")))
        (insert (format "- %s License file\n" (if has-license "✅" "❌")))
        (insert "\n")
        
        (insert "## Suggestions\n\n")
        (unless has-readme
          (insert "📝 **Add README.md** - Document your project purpose and usage\n"))
        (unless has-gitignore
          (insert "🚫 **Add .gitignore** - Exclude unnecessary files from version control\n"))
        (unless has-license
          (insert "⚖️ **Add LICENSE** - Clarify how others can use your code\n"))
        
        ;; Project-specific suggestions
        (cond
         ((string-match-p "arch-frame" root)
          (insert "🔧 **Arch-specific suggestions:**\n")
          (insert "- Document installation steps\n")
          (insert "- Create backup scripts\n")
          (insert "- Add system requirements\n"))
         ((string-match-p "learning-" root)
          (insert "📖 **Learning-specific suggestions:**\n")
          (insert "- Track learning progress\n")
          (insert "- Add code examples\n")
          (insert "- Document key concepts\n"))
         ((string-match-p "bdostumski\\.github\\.io" root)
          (insert "🌐 **Website-specific suggestions:**\n")
          (insert "- Optimize for SEO\n")
          (insert "- Add analytics\n")
          (insert "- Ensure mobile responsiveness\n")))
        
        (insert "\n## Quick Actions\n\n")
        (insert "1. **Create missing files** using the suggestions above\n")
        (insert "2. **Organize code** into logical directories\n")
        (insert "3. **Document** your project thoroughly\n")
        (insert "4. **Test** your setup on a fresh system\n\n")
        
        (markdown-mode)
        (display-buffer (current-buffer))))))

;; ----------------------------
;; Auto-enable for workflow
;; ----------------------------
(defun +ai/show-project-welcome ()
  "Show welcome message for user repositories."
  (when-let ((root (projectile-project-root)))
    (when (string-match-p "\\(arch-frame\\|learning-\\|bdostumski\\)" root)
      (run-with-timer 1 nil
                      (lambda ()
                        (message "💡 AI tools available! Use SPC a for assistance in %s" 
                                (projectile-project-name)))))))

(add-hook 'projectile-after-switch-project-hook #'+ai/show-project-welcome)

;; ----------------------------
;; Status and Info
;; ----------------------------
(defun +ai/status ()
  "Show AI tools status and available commands."
  (interactive)
  (with-current-buffer (get-buffer-create "*AI Tools Status*")
    (erase-buffer)
    (insert "# AI Tools Status\n\n")
    (insert (format "**Date:** %s\n" (format-time-string "%Y-%m-%d %H:%M")))
    (insert (format "**User:** %s\n" user-login-name))
    (insert (format "**Project:** %s\n\n" (or (projectile-project-name) "None")))
    
    (insert "## Available Tools\n\n")
    (insert "### Code Assistance\n")
    (insert "- `SPC a c e` - Explain selected code\n")
    (insert "- `SPC a c d` - Generate function documentation\n")
    (insert "- `SPC a c c` - Smart commit message generator\n\n")
    
    (insert "### Learning Tools\n")
    (insert "- `SPC a l n` - Create learning notes\n")
    (insert "- `SPC a l e` - Generate practice exercises\n\n")
    
    (insert "### Project Tools\n")
    (insert "- `SPC a p a` - Analyze project structure\n\n")
    
    (insert "## Repository-Specific Features\n\n")
    (insert "- **arch-frame:** System configuration templates\n")
    (insert "- **learning-*:** Educational content tools\n")
    (insert "- **bdostumski.github.io:** Website content tools\n")
    (insert "- **bdostumski:** Profile management tools\n\n")
    
    (insert "## Status\n\n")
    (insert "✅ All tools loaded successfully\n")
    (insert "✅ No external dependencies required\n")
    (insert "✅ Ready for use!\n")
    
    (markdown-mode)
    (display-buffer (current-buffer))))

(provide 'tools-llm-config)

;;; tools-llm-config.el ends here
