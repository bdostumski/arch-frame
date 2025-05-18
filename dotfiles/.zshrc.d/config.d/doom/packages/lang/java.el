;;; packages/lang/java.el -*- lexical-binding: t; -*-

;; Setup JDK path
(setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk/")

;; Setup jdtls
(after! lsp-java
  (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java"
        lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication")))



;; LSP Setup with lsp-java
(use-package! lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-save-action-organize-imports t
        lsp-java-format-on-type-enabled t
        lsp-java-completion-enabled t
        lsp-java-content-provider-preferred "fernflower"
        lsp-java-import-gradle-enabled nil
        lsp-java-import-maven-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-java-references-code-lens-enabled t
        lsp-java-format-enabled t)
  (add-hook 'java-mode-hook #'lsp))

;; Testing Support
(map! :after lsp-mode
      :map java-mode-map
      :leader
      :desc "Run test class" "t C" #'lsp-java-run-test-class
      :desc "Run test method" "t M" #'lsp-java-run-test-method)

;; Debugging with DAP
(after! dap-mode
  (require 'dap-java)
  (let ((test-runner-path (expand-file-name "eclipse.jdt.ls/test-runner.jar" doom-data-dir)))
    (setq dap-java-test-runner test-runner-path)))

;; Maven Integration
(setq lsp-java-import-maven-enabled t)

;; Maven key-bindings
(map! :leader
      :desc "Compile project with Maven" "c m" (lambda () (interactive) (compile "mvn compile")))

;; Bonus: Enable Lombok Support
(setq lsp-java-vmargs
      '("-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        "-javaagent:/path/to/lombok.jar"
        "-Xbootclasspath/a:/path/to/lombok.jar"))

;; Enable Code Formatting with Google Java Format
(setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
      lsp-java-format-settings-profile "GoogleStyle")
