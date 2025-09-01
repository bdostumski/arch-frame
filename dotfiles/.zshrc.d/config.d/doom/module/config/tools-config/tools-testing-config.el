;;; module/config/tools-config/tools-testing-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Testing configuration for Doom Emacs.
;; Provides integration with language-specific test frameworks for all active languages:
;; - Python (pytest)
;; - JavaScript/TypeScript (jest)
;; - Ruby (RSpec)
;; - Go (go test)
;; - Rust (cargo)
;; - Elixir (ExUnit)
;; - Scala (sbt)
;; - Java (JUnit)
;; - C/C++ (CTest/Make)
;; - PHP (phpunit)
;; - Shell (shunit2, bats)
;; - Haskell (cabal, stack)
;; - LaTeX (latexmk)
;; - Web (Jest, Mocha, etc.)
;;
;; Leader keybindings for running tests at point, per file, last run, or the entire project.

;;; Code:

(after! :tools testing
  ;; Python pytest configuration
  (setq +python-pytest-command "pytest --maxfail=1 --disable-warnings -q")

  ;; JavaScript / TypeScript test runner
  (setq +javascript-test-runner 'jest)
  (setq +typescript-test-runner 'jest)

  ;; Ruby RSpec test runner
  (setq +ruby-test-runner 'rspec)

  ;; Go test runner
  (setq +go-test-runner 'go-test)

  ;; Rust test runner
  (setq +rust-test-runner 'cargo)

  ;; Elixir test runner
  (setq +elixir-test-runner 'exunit)

  ;; Scala test runner
  (setq +scala-test-runner 'sbt)

  ;; Java test runner
  (setq +java-test-runner 'junit)

  ;; C/C++ test runner (CTest or Make)
  (setq +cc-test-runner 'ctest)

  ;; PHP test runner
  (setq +php-test-runner 'phpunit)

  ;; Shell test runner
  (setq +sh-test-runner 'shunit2)

  ;; Haskell test runner
  (setq +haskell-test-runner 'cabal)

  ;; LaTeX test runner (for build/testing)
  (setq +latex-test-runner 'latexmk)

  ;; Web test runner (Jest/Mocha etc.)
  (setq +web-test-runner 'jest))

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
