;;; module/config/tools-config/tools-pass-config.el -*- lexical-binding: t -*-
 
;;; Commentary:
;; Comprehensive password store configuration for Doom Emacs with extended keybindings
;; 
;; GPG configuration to excecute in shell
;; gpg --full-generate-key                      ;; Generate GPG key
;; gpg --list-secret-keys --keyid-format=long   ;; Get information about generated GPG key, and copy key from rsa4096/ABCDEF1234567890
;; pass init ABCDEF1234567890                   ;; Add gpg key to pass
;; pass generate test-user 16                   ;; Test configuration

;;; Code:

;; ----------------------------
;; Safe Password Store Setup
;; ----------------------------

(defvar +pass/default-store-dir "~/.password-store"
  "Default password store directory.")

(defun +pass/store-exists-p ()
  "Check if password store directory exists."
  (file-directory-p (expand-file-name +pass/default-store-dir)))

(defun +pass/store-initialized-p ()
  "Check if password store is properly initialized."
  (and (+pass/store-exists-p)
       (file-exists-p (expand-file-name ".gpg-id" +pass/default-store-dir))))

(defun +pass/get-gpg-keys ()
  "Get list of available GPG keys."
  (let ((output (shell-command-to-string "gpg --list-secret-keys --with-colons")))
    (when (string-match-p "sec:" output)
      (mapcar (lambda (line)
                (when (string-match "^uid:.*:.*:.*:.*:.*:.*:.*:.*:\\([^:]+\\):" line)
                  (match-string 1 line)))
              (split-string output "\n")))))

;; ----------------------------
;; Conditional Pass Configuration
;; ----------------------------

(when (+pass/store-initialized-p)
  ;; Only configure if properly initialized
  (use-package! pass
    :defer nil
    :init
    (setq password-store-dir (expand-file-name +pass/default-store-dir))
    :config
    (setq auth-source-pass-enable t)
    (auth-source-pass-enable)
    (message "✅ Password store configured and ready")
    (setq auth-sources '(password-store default))))

;; If not properly set up, provide guidance
(unless (+pass/store-initialized-p)
  (setq auth-source-pass-enable nil
        password-store-dir nil
        auth-sources '(default))
  (if (+pass/store-exists-p)
      (message "⚠️ Password store exists but not initialized - run SPC e t p I")
    (message "⚠️ Password store not found - run SPC e t p I to create")))

;; ----------------------------
;; Enhanced Management Functions
;; ----------------------------

(defun +pass/status ()
  "Show comprehensive password store status."
  (interactive)
  (let ((exists (+pass/store-exists-p))
        (initialized (+pass/store-initialized-p))
        (pass-loaded (featurep 'pass))
        (gpg-keys (ignore-errors (+pass/get-gpg-keys))))
    (with-current-buffer (get-buffer-create "*Pass Status*")
      (erase-buffer)
      (insert "🔐 Password Store Status\n")
      (insert "========================\n\n")
      (insert (format "Directory exists: %s\n" (if exists "✅ Yes" "❌ No")))
      (insert (format "Properly initialized: %s\n" (if initialized "✅ Yes" "❌ No")))
      (insert (format "Pass package loaded: %s\n" (if pass-loaded "✅ Yes" "❌ No")))
      (insert (format "Available GPG keys: %d\n" (length (remove nil gpg-keys))))
      (when gpg-keys
        (insert "\nGPG Keys found:\n")
        (dolist (key (remove nil gpg-keys))
          (insert (format "  • %s\n" key))))
      (unless initialized
        (insert "\n🔧 SETUP REQUIRED:\n")
        (if exists
            (insert "Run: SPC e t p I (Initialize existing directory)\n")
          (insert "Run: SPC e t p I (Create new password store)\n")))
      (display-buffer (current-buffer)))))

(defun +pass/initialize ()
  "Initialize password store with guided setup."
  (interactive)
  (cond
   ((+pass/store-initialized-p)
    (message "✅ Password store already properly initialized"))
   
   ;; Directory exists but not initialized
   ((+pass/store-exists-p)
    (let* ((gpg-keys (remove nil (+pass/get-gpg-keys)))
           (email (if gpg-keys
                     (completing-read "Select GPG key: " gpg-keys nil nil (car gpg-keys))
                   (read-string "GPG email (will create key if needed): " user-mail-address))))
      (message "🔄 Initializing existing password store directory...")
      (if (= 0 (shell-command (format "pass init '%s'" email)))
          (progn
            (message "✅ Password store initialized successfully!")
            ;; Reload configuration
            (+pass/reload-config))
        (message "❌ Failed to initialize password store. Check GPG setup."))))
   
   ;; Directory doesn't exist - create new
   (t
    (when (y-or-n-p "Create new password store? ")
      (let* ((gpg-keys (remove nil (+pass/get-gpg-keys)))
             (email (if gpg-keys
                       (completing-read "Select GPG key: " gpg-keys nil nil (car gpg-keys))
                     (read-string "GPG email (will create key if needed): " user-mail-address))))
        (message "🔄 Creating new password store...")
        (if (= 0 (shell-command (format "pass init '%s'" email)))
            (progn
              (message "✅ Password store created and initialized!")
              (+pass/reload-config))
          (message "❌ Failed to create password store. Check GPG setup.")))))))

(defun +pass/reload-config ()
  "Reload password store configuration."
  (interactive)
  (when (featurep 'pass)
    (unload-feature 'pass t))
  (when (+pass/store-initialized-p)
    (require 'pass)
    (setq password-store-dir (expand-file-name +pass/default-store-dir)
          auth-source-pass-enable t
          auth-sources '(password-store default))
    (auth-source-pass-enable)
    (message "✅ Password store configuration reloaded"))
  (+pass/status))

(defun +pass/setup-gpg ()
  "Setup GPG key if none exists."
  (interactive)
  (let ((gpg-keys (remove nil (+pass/get-gpg-keys))))
    (if gpg-keys
        (message "✅ GPG keys already available: %s" (mapconcat 'identity gpg-keys ", "))
      (when (y-or-n-p "No GPG keys found. Create one? ")
        (let* ((name (read-string "Full name: " user-full-name))
               (email (read-string "Email: " user-mail-address))
               (passphrase (read-passwd "Passphrase (optional): ")))
          (with-temp-buffer
            (insert (format "Key-Type: RSA\n"))
            (insert (format "Key-Length: 4096\n"))
            (insert (format "Subkey-Type: RSA\n"))
            (insert (format "Subkey-Length: 4096\n"))
            (insert (format "Name-Real: %s\n" name))
            (insert (format "Name-Email: %s\n" email))
            (insert (format "Expire-Date: 0\n"))
            (when (and passphrase (not (string-empty-p passphrase)))
              (insert (format "Passphrase: %s\n" passphrase)))
            (write-file "/tmp/gpg-batch")
            (message "🔄 Generating GPG key... (this may take a while)")
            (if (= 0 (shell-command "gpg --batch --generate-key /tmp/gpg-batch"))
                (progn
                  (delete-file "/tmp/gpg-batch")
                  (message "✅ GPG key generated successfully!"))
              (message "❌ Failed to generate GPG key"))))))))

(defun +pass/ensure-loaded ()
  "Ensure pass package is loaded before operations."
  (unless (featurep 'pass)
    (if (+pass/store-initialized-p)
        (progn
          (require 'pass)
          (message "🔄 Pass package loaded"))
      (message "❌ Password store not initialized. Run SPC e t p I first"))))

(defun +pass/copy-password ()
  "Copy password to clipboard."
  (interactive)
  (if (+pass/store-initialized-p)
      (progn
        (+pass/ensure-loaded)
        (condition-case err
            (call-interactively #'password-store-copy)
          (error (message "❌ Error: %s" (error-message-string err)))))
    (message "❌ Password store not initialized. Run SPC e t p I first")))

(defun +pass/generate-password ()
  "Generate new password using CLI (most reliable)."
  (interactive)
  (if (+pass/store-initialized-p)
      (let* ((entry-name (read-string "Entry name: "))
             (length (read-number "Password length: " 16))
             (options (list))
             (no-symbols (y-or-n-p "No symbols? "))
             (force-overwrite nil))
        
        (when no-symbols
          (push "--no-symbols" options))
        
        ;; Check if entry exists
        (when (file-exists-p 
               (expand-file-name (concat entry-name ".gpg") +pass/default-store-dir))
          (setq force-overwrite (y-or-n-p (format "Entry '%s' exists. Overwrite? " entry-name)))
          (when force-overwrite
            (push "--force" options)))
        
        (when (or force-overwrite 
                  (not (file-exists-p 
                        (expand-file-name (concat entry-name ".gpg") +pass/default-store-dir))))
          (let ((command (format "pass generate %s '%s' %d" 
                                (mapconcat 'identity options " ")
                                entry-name 
                                length)))
            (message "🔄 Generating password: %s" command)
            (if (= 0 (shell-command command))
                (message "✅ Password generated for '%s'" entry-name)
              (message "❌ Failed to generate password")))))
    (message "❌ Password store not initialized. Run SPC e t p I first")))

;; Add other functions with the same pattern...
(defun +pass/list-passwords ()
  "List all passwords."
  (interactive)
  (if (+pass/store-initialized-p)
      (progn
        (+pass/ensure-loaded)
        (condition-case err
            (password-store-list)
          (error (message "❌ Error: %s" (error-message-string err)))))
    (message "❌ Password store not initialized. Run SPC e t p I first")))

(defun +pass/insert-password ()
  "Insert new password entry."
  (interactive)
  (if (+pass/store-initialized-p)
      (progn
        (+pass/ensure-loaded)
        (condition-case err
            (call-interactively #'password-store-insert)
          (error (message "❌ Error: %s" (error-message-string err)))))
    (message "❌ Password store not initialized. Run SPC e t p I first")))

(provide 'tools-pass-config)

;;; tools-pass-config.el ends here
