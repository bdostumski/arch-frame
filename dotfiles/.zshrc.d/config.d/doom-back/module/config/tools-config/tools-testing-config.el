;;; module/config/tools-config/tools-testing-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive testing configuration for Doom Emacs
;; Multi-language test framework integration with intelligent detection and execution
;; Tailored for modern development workflows including web, systems, and documentation

;;; Code:

;; ----------------------------
;; Enhanced Testing Configuration
;; ----------------------------
(after! :tools testing
  ;; Python testing (pytest with enhanced options)
  (setq +python-pytest-command "pytest --maxfail=3 --tb=short -v --color=yes")
  
  ;; JavaScript/TypeScript testing
  (setq +javascript-test-runner 'jest
        +typescript-test-runner 'jest
        +js-test-command "npm test"
        +jest-command "npx jest --verbose --colors")
  
  ;; Web development testing
  (setq +web-test-runner 'jest
        +react-test-runner 'jest
        +vue-test-runner 'jest)
  
  ;; Systems programming
  (setq +rust-test-runner 'cargo
        +go-test-runner 'go-test
        +cc-test-runner 'ctest)
  
  ;; Documentation and markup
  (setq +latex-test-runner 'latexmk
        +markdown-test-runner 'markdownlint)
  
  ;; Shell scripting
  (setq +sh-test-runner 'bats
        +bash-test-runner 'bats))

;; ----------------------------
;; Test Framework Detection
;; ----------------------------
(defvar +testing/framework-files
  '((jest . ("jest.config.js" "jest.config.json" "package.json"))
    (mocha . ("mocha.opts" ".mocharc.json" "test/mocha.opts"))
    (vitest . ("vitest.config.js" "vitest.config.ts"))
    (pytest . ("pytest.ini" "pyproject.toml" "setup.cfg" "tox.ini"))
    (cargo . ("Cargo.toml"))
    (go-test . ("go.mod" "*_test.go"))
    (rspec . ("spec" ".rspec" "Gemfile"))
    (phpunit . ("phpunit.xml" "phpunit.xml.dist"))
    (ctest . ("CMakeLists.txt" "CTestTestfile.cmake"))
    (maven . ("pom.xml"))
    (gradle . ("build.gradle" "build.gradle.kts")))
  "Mapping of test frameworks to their configuration files.")

(defun +testing/detect-framework ()
  "Detect the appropriate test framework for current project."
  (when-let ((project-root (projectile-project-root)))
    (catch 'found
      (dolist (framework +testing/framework-files)
        (let ((framework-name (car framework))
              (files (cdr framework)))
          (dolist (file files)
            (when (or (file-exists-p (expand-file-name file project-root))
                     (and (string-match-p "\\*" file)
                          (directory-files project-root nil (replace-regexp-in-string "\\*" ".*" file))))
              (throw 'found framework-name))))))))

;; ----------------------------
;; Smart Test Execution
;; ----------------------------
(defun +testing/run-test-at-point ()
  "Run test at point with framework detection."
  (interactive)
  (let ((framework (+testing/detect-framework)))
    (pcase framework
      ('jest (+testing/run-jest-at-point))
      ('pytest (+testing/run-pytest-at-point))
      ('cargo (+testing/run-cargo-test-at-point))
      ('go-test (+testing/run-go-test-at-point))
      ('rspec (+testing/run-rspec-at-point))
      (_ (+testing/run-generic-test-at-point)))))

(defun +testing/run-test-file ()
  "Run all tests in current file."
  (interactive)
  (let ((framework (+testing/detect-framework))
        (file-path (buffer-file-name)))
    (when file-path
      (pcase framework
        ('jest (+testing/run-jest-file file-path))
        ('pytest (+testing/run-pytest-file file-path))
        ('cargo (+testing/run-cargo-test-file))
        ('go-test (+testing/run-go-test-file file-path))
        ('rspec (+testing/run-rspec-file file-path))
        (_ (+testing/run-generic-test-file file-path))))))

(defun +testing/run-project-tests ()
  "Run all tests in project."
  (interactive)
  (let ((framework (+testing/detect-framework)))
    (pcase framework
      ('jest (+testing/run-jest-project))
      ('pytest (+testing/run-pytest-project))
      ('cargo (+testing/run-cargo-test-project))
      ('go-test (+testing/run-go-test-project))
      ('rspec (+testing/run-rspec-project))
      (_ (+testing/run-generic-project-tests)))))

;; ----------------------------
;; Framework-Specific Implementations
;; ----------------------------

;; Jest (JavaScript/TypeScript)
(defun +testing/run-jest-at-point ()
  "Run Jest test at point."
  (let ((test-name (+testing/get-js-test-name)))
    (if test-name
        (compile (format "npx jest --testNamePattern='%s' --verbose" test-name))
      (message "No test found at point"))))

(defun +testing/run-jest-file (file-path)
  "Run Jest tests for specific file."
  (compile (format "npx jest %s --verbose" (shell-quote-argument file-path))))

(defun +testing/run-jest-project ()
  "Run all Jest tests in project."
  (compile "npx jest --verbose --colors"))

(defun +testing/get-js-test-name ()
  "Extract JavaScript test name at point."
  (save-excursion
    (when (re-search-backward "\\(test\\|it\\|describe\\)\\s-*(['\"]\\([^'\"]+\\)['\"]" nil t)
      (match-string 2))))

;; Pytest (Python)
(defun +testing/run-pytest-at-point ()
  "Run pytest test at point."
  (let ((test-name (+testing/get-python-test-name)))
    (if test-name
        (compile (format "pytest %s::%s -v" (buffer-file-name) test-name))
      (message "No test found at point"))))

(defun +testing/run-pytest-file (file-path)
  "Run pytest for specific file."
  (compile (format "pytest %s -v" (shell-quote-argument file-path))))

(defun +testing/run-pytest-project ()
  "Run all pytest tests in project."
  (compile "pytest -v --tb=short"))

(defun +testing/get-python-test-name ()
  "Extract Python test function name at point."
  (save-excursion
    (when (re-search-backward "^\\s-*def \\(test_[a-zA-Z0-9_]+\\)" nil t)
      (match-string 1))))

;; Cargo (Rust)
(defun +testing/run-cargo-test-at-point ()
  "Run Rust test at point."
  (let ((test-name (+testing/get-rust-test-name)))
    (if test-name
        (compile (format "cargo test %s -- --nocapture" test-name))
      (message "No test found at point"))))

(defun +testing/run-cargo-test-file ()
  "Run Rust tests in current module."
  (let ((module-name (+testing/get-rust-module-name)))
    (if module-name
        (compile (format "cargo test %s -- --nocapture" module-name))
      (compile "cargo test -- --nocapture"))))

(defun +testing/run-cargo-test-project ()
  "Run all Rust tests in project."
  (compile "cargo test -- --nocapture"))

(defun +testing/get-rust-test-name ()
  "Extract Rust test function name at point."
  (save-excursion
    (when (re-search-backward "#\\[test\\]\\s-*\\(?:\\s-*#\\[.*\\]\\s-*\\)*fn \\([a-zA-Z0-9_]+\\)" nil t)
      (match-string 1))))

(defun +testing/get-rust-module-name ()
  "Extract Rust module name for testing."
  (save-excursion
    (when (re-search-backward "^mod \\([a-zA-Z0-9_]+\\)" nil t)
      (match-string 1))))

;; Go testing
(defun +testing/run-go-test-at-point ()
  "Run Go test at point."
  (let ((test-name (+testing/get-go-test-name)))
    (if test-name
        (compile (format "go test -run %s -v" test-name))
      (message "No test found at point"))))

(defun +testing/run-go-test-file (file-path)
  "Run Go tests in specific file."
  (compile (format "go test -v %s" (file-name-directory file-path))))

(defun +testing/run-go-test-project ()
  "Run all Go tests in project."
  (compile "go test -v ./..."))

(defun +testing/get-go-test-name ()
  "Extract Go test function name at point."
  (save-excursion
    (when (re-search-backward "^func \\(Test[a-zA-Z0-9_]+\\)" nil t)
      (match-string 1))))

;; Generic fallbacks
(defun +testing/run-generic-test-at-point ()
  "Generic test runner for unsupported frameworks."
  (message "Framework-specific test at point not supported. Use file or project tests."))

(defun +testing/run-generic-test-file (file-path)
  "Generic file test runner."
  (compile (format "make test FILE=%s" (shell-quote-argument file-path))))

(defun +testing/run-generic-project-tests ()
  "Generic project test runner."
  (cond
   ((file-exists-p "Makefile") (compile "make test"))
   ((file-exists-p "package.json") (compile "npm test"))
   ((file-exists-p "Cargo.toml") (compile "cargo test"))
   ((file-exists-p "go.mod") (compile "go test ./..."))
   (t (message "No test command found for this project"))))

;; ----------------------------
;; Test Coverage and Reporting
;; ----------------------------
(defun +testing/run-with-coverage ()
  "Run tests with coverage reporting."
  (interactive)
  (let ((framework (+testing/detect-framework)))
    (pcase framework
      ('jest (compile "npx jest --coverage --verbose"))
      ('pytest (compile "pytest --cov=. --cov-report=html --cov-report=term"))
      ('cargo (compile "cargo tarpaulin --out Html --output-dir coverage"))
      ('go-test (compile "go test -coverprofile=coverage.out ./... && go tool cover -html=coverage.out"))
      (_ (message "Coverage not configured for this framework")))))

(defun +testing/view-coverage-report ()
  "Open coverage report in browser."
  (interactive)
  (let ((coverage-files '("coverage/index.html"
                         "htmlcov/index.html" 
                         "coverage.html"
                         "coverage/tarpaulin-report.html")))
    (catch 'found
      (dolist (file coverage-files)
        (when (file-exists-p file)
          (browse-url (concat "file://" (expand-file-name file)))
          (throw 'found t)))
      (message "No coverage report found"))))

;; ----------------------------
;; Test Discovery and Navigation
;; ----------------------------
(defun +testing/discover-tests ()
  "Discover and display test files in current project."
  (interactive)
  (when-let ((project-root (projectile-project-root)))
    (let ((test-patterns '("*test*.js" "*test*.ts" "*test*.py" "*_test.go" "*_test.rs" "test_*.py" "spec/**/*.rb"))
          (test-files '()))
      
      (dolist (pattern test-patterns)
        (setq test-files (append test-files 
                                (directory-files-recursively project-root pattern))))
      
      (with-current-buffer (get-buffer-create "*Test Files*")
        (erase-buffer)
        (insert "# Test Files in Project\n\n")
        (insert (format "**Project:** %s\n" (projectile-project-name)))
        (insert (format "**Framework:** %s\n" (+testing/detect-framework)))
        (insert (format "**Total Files:** %d\n\n" (length test-files)))
        
        (insert "## Test Files\n\n")
        (dolist (file test-files)
          (let ((relative-path (file-relative-name file project-root)))
            (insert (format "- `%s`\n" relative-path))))
        
        (insert "\n## Quick Actions\n\n")
        (insert "- `SPC e T t` - Run test at point\n")
        (insert "- `SPC e T f` - Run test file\n")
        (insert "- `SPC e T p` - Run project tests\n")
        (insert "- `SPC e T c` - Run with coverage\n")
        
        (markdown-mode)
        (display-buffer (current-buffer))))))

;; ----------------------------
;; Test History and Favorites
;; ----------------------------
(defvar +testing/command-history '()
  "History of test commands.")

(defvar +testing/favorite-tests '()
  "List of favorite test commands.")

(defun +testing/run-last-test ()
  "Run the last executed test command."
  (interactive)
  (if +testing/command-history
      (let ((last-command (car +testing/command-history)))
        (message "🔄 Re-running: %s" last-command)
        (compile last-command))
    (message "No previous test to run")
    (+testing/run-project-tests)))

(defun +testing/save-to-history (command)
  "Save test command to history."
  (push command +testing/command-history)
  (when (> (length +testing/command-history) 10)
    (setq +testing/command-history (seq-take +testing/command-history 10))))

(defun +testing/add-to-favorites ()
  "Add last test command to favorites."
  (interactive)
  (when +testing/command-history
    (let ((command (car +testing/command-history)))
      (unless (member command +testing/favorite-tests)
        (push command +testing/favorite-tests)
        (message "⭐ Added test to favorites: %s" command)))))

(defun +testing/run-favorite ()
  "Run a favorite test command."
  (interactive)
  (if +testing/favorite-tests
      (let ((command (completing-read "Run favorite test: " +testing/favorite-tests)))
        (when command
          (compile command)
          (+testing/save-to-history command)))
    (message "No favorite tests saved")))

;; ----------------------------
;; Test File Templates
;; ----------------------------
(defun +testing/create-test-file ()
  "Create a test file template."
  (interactive)
  (let* ((framework (+testing/detect-framework))
         (current-file (buffer-file-name))
         (test-file (+testing/generate-test-filename current-file framework)))
    
    (when (and test-file (not (file-exists-p test-file)))
      (with-temp-file test-file
        (pcase framework
          ('jest (+testing/insert-jest-template current-file))
          ('pytest (+testing/insert-pytest-template current-file))
          ('cargo (+testing/insert-rust-test-template))
          ('go-test (+testing/insert-go-test-template current-file))
          (_ (+testing/insert-generic-test-template current-file))))
      
      (find-file test-file)
      (message "✅ Created test file: %s" test-file))))

(defun +testing/generate-test-filename (source-file framework)
  "Generate test filename based on source file and framework."
  (when source-file
    (let* ((dir (file-name-directory source-file))
           (name (file-name-sans-extension (file-name-nondirectory source-file)))
           (ext (file-name-extension source-file)))
      (pcase framework
        ('jest (expand-file-name (format "%s.test.%s" name ext) dir))
        ('pytest (expand-file-name (format "test_%s.py" name) dir))
        ('go-test (expand-file-name (format "%s_test.go" name) dir))
        (_ (expand-file-name (format "%s_test.%s" name ext) dir))))))

(defun +testing/insert-jest-template (source-file)
  "Insert Jest test template."
  (let ((module-name (file-name-sans-extension (file-name-nondirectory source-file))))
    (insert (format "import { %s } from './%s';\n\n" module-name module-name))
    (insert (format "describe('%s', () => {\n" module-name))
    (insert "  test('should work correctly', () => {\n")
    (insert "    // Arrange\n")
    (insert "    \n")
    (insert "    // Act\n")
    (insert "    \n")
    (insert "    // Assert\n")
    (insert "    expect(true).toBe(true);\n")
    (insert "  });\n")
    (insert "});\n")))

(defun +testing/insert-pytest-template (source-file)
  "Insert pytest template."
  (let ((module-name (file-name-sans-extension (file-name-nondirectory source-file))))
    (insert (format "import pytest\nfrom %s import *\n\n" module-name))
    (insert "class TestExample:\n")
    (insert "    def test_should_work_correctly(self):\n")
    (insert "        # Arrange\n")
    (insert "        \n")
    (insert "        # Act\n")
    (insert "        \n")
    (insert "        # Assert\n")
    (insert "        assert True\n")))

(defun +testing/insert-rust-test-template ()
  "Insert Rust test template."
  (insert "#[cfg(test)]\n")
  (insert "mod tests {\n")
  (insert "    use super::*;\n\n")
  (insert "    #[test]\n")
  (insert "    fn test_example() {\n")
  (insert "        // Arrange\n")
  (insert "        \n")
  (insert "        // Act\n")
  (insert "        \n")
  (insert "        // Assert\n")
  (insert "        assert_eq!(2 + 2, 4);\n")
  (insert "    }\n")
  (insert "}\n"))

(defun +testing/insert-go-test-template (source-file)
  "Insert Go test template."
  (insert "package main\n\n")
  (insert "import \"testing\"\n\n")
  (insert "func TestExample(t *testing.T) {\n")
  (insert "    // Arrange\n")
  (insert "    \n")
  (insert "    // Act\n")
  (insert "    \n")
  (insert "    // Assert\n")
  (insert "    if 2+2 != 4 {\n")
  (insert "        t.Error(\"Math is broken\")\n")
  (insert "    }\n")
  (insert "}\n"))

(defun +testing/insert-generic-test-template (source-file)
  "Insert generic test template."
  (insert "// Test file for " (file-name-nondirectory source-file) "\n\n")
  (insert "// Add your tests here\n"))

;; ----------------------------
;; Advanced Testing Features
;; ----------------------------
(defun +testing/toggle-test-file ()
  "Toggle between source and test file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (is-test-file (string-match-p "test\\|spec" current-file)))
    (if is-test-file
        (+testing/find-source-file current-file)
      (+testing/find-test-file current-file))))

(defun +testing/find-test-file (source-file)
  "Find corresponding test file for source file."
  (let ((framework (+testing/detect-framework)))
    (let ((test-file (+testing/generate-test-filename source-file framework)))
      (if (file-exists-p test-file)
          (find-file test-file)
        (when (y-or-n-p "Test file doesn't exist. Create it? ")
          (+testing/create-test-file))))))

(defun +testing/find-source-file (test-file)
  "Find corresponding source file for test file."
  (let* ((base-name (replace-regexp-in-string "test[._]\\|[._]test\\|spec[._]\\|[._]spec" "" 
                                             (file-name-nondirectory test-file)))
         (dir (file-name-directory test-file))
         (possible-files (directory-files dir nil (concat "^" (regexp-quote base-name) "\\..*$"))))
    (if possible-files
        (find-file (expand-file-name (car possible-files) dir))
      (message "No corresponding source file found"))))

;; ----------------------------
;; Integration Hooks
;; ----------------------------
(defun +testing/setup-project-testing ()
  "Setup testing environment when switching projects."
  (when-let ((framework (+testing/detect-framework)))
    (run-with-timer 1 nil
                    (lambda ()
                      (message "🧪 Testing framework detected: %s | Use SPC e T d to discover tests" framework)))))

(when (featurep 'projectile)
  (add-hook 'projectile-after-switch-project-hook #'+testing/setup-project-testing))

;; Advice to track test execution
(advice-add 'compile :before
            (lambda (command &rest _)
              (when (string-match-p "test\\|jest\\|pytest\\|cargo test\\|go test" command)
                (+testing/save-to-history command))))

(provide 'tools-testing-config)

;;; tools-testing-config.el ends here
