;;; module/config/tools-config/tools-testing-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Testing configuration for Doom Emacs.
;; Provides integration with language-specific test frameworks like pytest for Python
;; and Jest for JavaScript/TypeScript. Also defines leader keybindings for running
;; tests at point, per file, last run, or for the entire project.

;;; Code:

(after! :tools testing
  ;; Python pytest configuration
  (setq +python-pytest-command "pytest --maxfail=1 --disable-warnings -q")

  ;; JavaScript / TypeScript test runner
  (setq +javascript-test-runner 'jest))

;; ----------------------------
;; Leader keybindings for tests
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("t" . "testing")
;;       :desc "Run test at point" "t" #'doom/test-function
;;       :desc "Run test file" "f" #'doom/test-file
;;       :desc "Run last test" "l" #'doom/test-last
;;       :desc "Run project tests" "p" #'doom/test-project))

(provide 'tools-testing-config)

;;; tools-testing-config.el ends here
