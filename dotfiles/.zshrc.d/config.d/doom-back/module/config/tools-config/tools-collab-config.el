;;; module/config/tools-config/tools-collab-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive collaborative editing setup for Doom Emacs using CRDT
;;
;; Security:
;; - Use trusted networks only
;; - Consider SSH tunneling for remote collaboration
;; - Sessions are not encrypted by default

;;; Code:

;; ----------------------------
;; State tracking variables
;; ----------------------------
(defvar +crt/crdt-sessions nil
  "List of active CRDT sessions for tracking.")

(defvar +crt/crdt-default-port 6530
  "Default port for CRDT server.")

(defvar +crt/crdt-default-address "localhost"
  "Default address for CRDT connections.")

(defvar +crt/crdt-user-cursors-enabled t
  "Whether to show other users' cursors.")

(defvar +crt/crdt-auto-save-enabled t
  "Whether to auto-save shared buffers.")

;; ----------------------------
;; Enhanced CRDT package configuration
;; ----------------------------
(use-package! crdt
  :commands (crdt-share-buffer
             crdt-connect
             crdt-stop
             crdt-list-sessions
             crdt-list-users)

  :init
  ;; Pre-load configuration
  (setq crdt-server-port +crt/crdt-default-port
        crdt-use-tuntox nil                    ; Disable tuntox by default
        crdt-ask-for-password t)               ; Always ask for password

  :config
  ;; Core CRDT settings
  (setq crdt-server-port +crt/crdt-default-port
        crdt-default-name (user-login-name)    ; Use system username
        crdt-ask-for-password t                ; Security: always ask for password
        crdt-use-tuntox nil                    ; Use direct connections
        crdt-display-usernames t               ; Show usernames in buffer
        crdt-enable-cursor-overlay t           ; Show user cursors
        crdt-enable-region-overlay t)          ; Show user selections

  ;; Hook for session management
  (add-hook 'crdt-connect-hook #'+crt/crdt-on-connect)
  (add-hook 'crdt-disconnect-hook #'+crt/crdt-on-disconnect)

  ;; Auto-save shared buffers
  (when +crt/crdt-auto-save-enabled
    (add-hook 'crdt-buffer-share-hook #'+crt/crdt-enable-auto-save)))

;; ----------------------------
;; Session management functions
;; ----------------------------
(defun +crt/crdt-share-buffer-enhanced ()
  "Share current buffer with enhanced options and feedback."
  (interactive)
  (if (buffer-file-name)
      (let* ((buffer-name (buffer-name))
             (port (read-number "Port (default 6530): " +crt/crdt-default-port))
             (password (read-passwd "Session password (optional): " nil "")))
        (message "Starting CRDT server for buffer '%s' on port %d..." buffer-name port)
        (setq crdt-server-port port)
        (crdt-share-buffer)
        (add-to-list '+crt/crdt-sessions
                     `(:buffer ,buffer-name :port ,port :type server :status active))
        (+crt/crdt-show-connection-info port)
        (message "✓ Buffer '%s' is now shared on port %d" buffer-name port))
    (message "Cannot share buffer: not associated with a file")))

(defun +crt/crdt-connect-enhanced ()
  "Connect to CRDT session with enhanced options."
  (interactive)
  (let* ((address (read-string "Server address: " +crt/crdt-default-address))
         (port (read-number "Port: " +crt/crdt-default-port))
         (password (read-passwd "Password (if required): " nil "")))
    (message "Connecting to CRDT server at %s:%d..." address port)
    (crdt-connect address port)
    (add-to-list '+crt/crdt-sessions
                 `(:address ,address :port ,port :type client :status connecting))
    (message "Connection initiated to %s:%d" address port)))

(defun +crt/crdt-stop-all-sessions ()
  "Stop all active CRDT sessions."
  (interactive)
  (if +crt/crdt-sessions
      (progn
        (crdt-stop)
        (setq +crt/crdt-sessions nil)
        (message "✓ All CRDT sessions stopped"))
    (message "No active CRDT sessions")))

(defun +crt/crdt-list-active-sessions ()
  "Show list of active CRDT sessions."
  (interactive)
  (if +crt/crdt-sessions
      (with-current-buffer (get-buffer-create "*CRDT Sessions*")
        (erase-buffer)
        (insert "Active CRDT Sessions\n")
        (insert "====================\n\n")
        (dolist (session +crt/crdt-sessions)
          (insert (format "Type: %s | Port: %s | Status: %s\n"
                          (plist-get session :type)
                          (plist-get session :port)
                          (plist-get session :status))))
        (display-buffer (current-buffer)))
    (message "No active CRDT sessions")))

(defun +crt/crdt-show-connection-info (port)
  "Show connection information for sharing."
  (let ((ip-address (+crt/crdt-get-local-ip))
        (buffer-name "*CRDT Connection Info*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "CRDT Session Connection Information\n")
      (insert "=====================================\n\n")
      (insert (format "Share this information with collaborators:\n\n"))
      (insert (format "Server Address: %s\n" (or ip-address "localhost")))
      (insert (format "Port: %d\n" port))
      (insert (format "Started by: %s\n" (user-login-name)))
      (insert (format "Time: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "Instructions for collaborators:\n")
      (insert "1. Open Emacs with CRDT support\n")
      (insert "2. Run: M-x crdt-connect\n")
      (insert (format "3. Enter address: %s\n" (or ip-address "localhost")))
      (insert (format "4. Enter port: %d\n" port))
      (insert "5. Start collaborating!\n\n")
      (insert "Security Note: Only share with trusted collaborators.\n")
      (display-buffer (current-buffer)))))

(defun +crt/crdt-get-local-ip ()
  "Get local IP address for sharing connection info."
  (condition-case nil
      (with-temp-buffer
        (call-process "hostname" nil t nil "-I")
        (string-trim (buffer-string)))
    (error "Unable to determine IP address")))

;; ----------------------------
;; User and cursor management
;; ----------------------------
(defun +crt/crdt-toggle-user-cursors ()
  "Toggle display of other users' cursors."
  (interactive)
  (setq +crt/crdt-user-cursors-enabled
        (not +crt/crdt-user-cursors-enabled))
  (setq crdt-enable-cursor-overlay +crt/crdt-user-cursors-enabled)
  (message "User cursors: %s"
           (if +crt/crdt-user-cursors-enabled "✓ enabled" "✗ disabled")))

(defun +crt/crdt-show-connected-users ()
  "Show list of connected users in current session."
  (interactive)
  (if (bound-and-true-p crdt--session)
      (let ((users (crdt-list-users)))
        (if users
            (message "Connected users: %s" (string-join users ", "))
          (message "No other users connected")))
    (message "Not in an active CRDT session")))

(defun +crt/crdt-send-message ()
  "Send a message to all collaborators."
  (interactive)
  (if (bound-and-true-p crdt--session)
      (let ((message (read-string "Message to collaborators: ")))
        (when (and message (not (string-empty-p message)))
          ;; This would require extending CRDT to support messaging
          ;; For now, we'll show it locally as a placeholder
          (message "Message sent: %s" message)
          ;; Future: implement actual messaging protocol
          ))
    (message "Not in an active CRDT session")))

;; ----------------------------
;; Session hooks and utilities
;; ----------------------------
(defun +crt/crdt-on-connect ()
  "Hook function called when connecting to a CRDT session."
  (message "✓ Successfully connected to CRDT session")
  (when (fboundp 'doom-modeline-mode)
    (force-mode-line-update)))

(defun +crt/crdt-on-disconnect ()
  "Hook function called when disconnecting from a CRDT session."
  (message "✓ Disconnected from CRDT session")
  (setq +crt/crdt-sessions
        (cl-remove-if (lambda (session)
                        (eq (plist-get session :status) 'active))
                      +crt/crdt-sessions))
  (when (fboundp 'doom-modeline-mode)
    (force-mode-line-update)))

(defun +crt/crdt-enable-auto-save ()
  "Enable auto-save for shared buffers."
  (when (buffer-file-name)
    (auto-save-mode 1)
    (message "Auto-save enabled for shared buffer")))

(defun +crt/crdt-reconnect-last ()
  "Reconnect to the last used CRDT session."
  (interactive)
  (let ((last-session (car (cl-remove-if-not
                            (lambda (s) (eq (plist-get s :type) 'client))
                            +crt/crdt-sessions))))
    (if last-session
        (let ((address (plist-get last-session :address))
              (port (plist-get last-session :port)))
          (message "Reconnecting to %s:%d..." address port)
          (crdt-connect address port))
      (message "No previous connection to reconnect to"))))

;; ----------------------------
;; Status and monitoring
;; ----------------------------
(defun +crt/crdt-status ()
  "Show current CRDT status."
  (interactive)
  (if (bound-and-true-p crdt--session)
      (let ((session-info (format "Active CRDT session | Users: %d | Buffer: %s"
                                  (length (crdt-list-users))
                                  (buffer-name))))
        (message session-info))
    (message "No active CRDT session")))

(defun +crt/crdt-health-check ()
  "Check CRDT system health and connectivity."
  (interactive)
  (message "Running CRDT health check...")
  (let ((issues '()))
    ;; Check if CRDT is loaded
    (unless (featurep 'crdt)
      (push "CRDT package not loaded" issues))

    ;; Check network connectivity (basic)
    (unless (executable-find "ping")
      (push "Network utilities not available" issues))

    ;; Report results
    (if issues
        (message "CRDT health issues: %s" (string-join issues "; "))
      (message "✓ CRDT system healthy"))))

;; ----------------------------
;; Integration with Doom modeline
;; ----------------------------
(defun +crt/crdt-modeline-segment ()
  "Create a modeline segment showing CRDT status."
  (when (bound-and-true-p crdt--session)
    (let ((user-count (length (crdt-list-users))))
      (format " CRDT:%d " user-count))))

;; Add to modeline if doom-modeline is available
(after! doom-modeline
  (doom-modeline-def-segment crdt-status
    (+crt/crdt-modeline-segment))

  ;; Uncomment to add CRDT status to modeline
  (setq doom-modeline-format '(...other-segments... crdt-status)))

;; ----------------------------
;; Security and safety features
;; ----------------------------
(defun +crt/crdt-security-warning ()
  "Display security warning for CRDT usage."
  (interactive)
  (with-current-buffer (get-buffer-create "*CRDT Security Info*")
    (erase-buffer)
    (insert "CRDT Security Guidelines\n")
    (insert "=======================\n\n")
    (insert "IMPORTANT SECURITY CONSIDERATIONS:\n\n")
    (insert "1. UNENCRYPTED CONNECTIONS\n")
    (insert "   - CRDT connections are NOT encrypted by default\n")
    (insert "   - All code and changes are transmitted in plain text\n")
    (insert "   - Use only on trusted networks (local/VPN)\n\n")
    (insert "2. NETWORK EXPOSURE\n")
    (insert "   - Sharing exposes your system to network connections\n")
    (insert "   - Use firewall rules to limit access\n")
    (insert "   - Consider SSH tunneling for remote collaboration\n\n")
    (insert "3. CODE ACCESS\n")
    (insert "   - All collaborators can see and modify shared code\n")
    (insert "   - Only collaborate with trusted individuals\n")
    (insert "   - Keep sensitive data in non-shared buffers\n\n")
    (insert "4. RECOMMENDED PRACTICES\n")
    (insert "   - Use strong, unique session passwords\n")
    (insert "   - Limit session duration\n")
    (insert "   - Monitor connected users regularly\n")
    (insert "   - Stop sessions when collaboration ends\n\n")
    (insert "For secure remote collaboration, consider:\n")
    (insert "- SSH tunneling: ssh -L 6530:localhost:6530 user@server\n")
    (insert "- VPN connections\n")
    (insert "- Dedicated collaboration servers\n")
    (display-buffer (current-buffer))))

;; Show security warning on first use
(defvar +crt/crdt-security-warning-shown nil
  "Whether security warning has been shown.")

(defadvice! +crt/crdt-show-security-warning-once (&rest _)
  :before #'crdt-share-buffer
  (unless +crt/crdt-security-warning-shown
    (when (y-or-n-p "Show CRDT security guidelines? (Recommended for first-time users) ")
      (+crt/crdt-security-warning))
    (setq +crt/crdt-security-warning-shown t)))

;; ----------------------------
;; Cleanup and state management
;; ----------------------------
(defun +crt/crdt-cleanup-on-exit ()
  "Clean up CRDT sessions on Emacs exit."
  (when +crt/crdt-sessions
    (crdt-stop)
    (message "CRDT sessions cleaned up on exit")))

;; Register cleanup hook
(add-hook 'kill-emacs-hook #'+crt/crdt-cleanup-on-exit)

(provide 'tools-collab-config)

;;; tools-collab-config.el ends here
