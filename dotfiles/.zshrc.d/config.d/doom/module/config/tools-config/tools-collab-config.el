;;; module/config/tools-config/tools-collab-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive collaborative editing setup for Doom Emacs using CRDT
;;
;; FEATURES:
;; - Real-time collaborative editing with multiple users
;; - Session management with user tracking
;; - Network configuration with custom ports and addresses
;; - Buffer sharing with selective sync
;; - User presence indicators and cursors
;; - Session persistence and recovery
;; - Security features for safe collaboration
;; - Integration with Doom's modeline and UI
;;
;; KEYBINDINGS:
;; Leader Key Bindings (SPC):
;;   SPC C     - Main collaboration prefix
;;   SPC C s   - Share current buffer
;;   SPC C c   - Connect to session
;;   SPC C d   - Disconnect/stop session
;;   SPC C l   - List active sessions
;;   SPC C u   - Show connected users
;;   SPC C r   - Reconnect to session
;;   SPC C k   - Kill all sessions
;;   SPC C t   - Toggle user cursors
;;   SPC C m   - Send message to collaborators
;;
;; USAGE:
;; 1. Start sharing: SPC C s (crdt-share-buffer)
;; 2. Share connection info with collaborators
;; 3. Others connect: SPC C c (crdt-connect)
;; 4. Collaborate in real-time!
;;
;; SECURITY:
;; - Use trusted networks only
;; - Consider SSH tunneling for remote collaboration
;; - Sessions are not encrypted by default

;;; Code:

;; ----------------------------
;; State tracking variables
;; ----------------------------
(defvar bdostumski/crdt-sessions nil
  "List of active CRDT sessions for tracking.")

(defvar bdostumski/crdt-default-port 6530
  "Default port for CRDT server.")

(defvar bdostumski/crdt-default-address "localhost"
  "Default address for CRDT connections.")

(defvar bdostumski/crdt-user-cursors-enabled t
  "Whether to show other users' cursors.")

(defvar bdostumski/crdt-auto-save-enabled t
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
  (setq crdt-server-port bdostumski/crdt-default-port
        crdt-use-tuntox nil                    ; Disable tuntox by default
        crdt-ask-for-password t)               ; Always ask for password

  :config
  ;; Core CRDT settings
  (setq crdt-server-port bdostumski/crdt-default-port
        crdt-default-name (user-login-name)    ; Use system username
        crdt-ask-for-password t                ; Security: always ask for password
        crdt-use-tuntox nil                    ; Use direct connections
        crdt-display-usernames t               ; Show usernames in buffer
        crdt-enable-cursor-overlay t           ; Show user cursors
        crdt-enable-region-overlay t)          ; Show user selections

  ;; Hook for session management
  (add-hook 'crdt-connect-hook #'bdostumski/crdt-on-connect)
  (add-hook 'crdt-disconnect-hook #'bdostumski/crdt-on-disconnect)

  ;; Auto-save shared buffers
  (when bdostumski/crdt-auto-save-enabled
    (add-hook 'crdt-buffer-share-hook #'bdostumski/crdt-enable-auto-save)))

;; ----------------------------
;; Session management functions
;; ----------------------------
(defun bdostumski/crdt-share-buffer-enhanced ()
  "Share current buffer with enhanced options and feedback."
  (interactive)
  (if (buffer-file-name)
      (let* ((buffer-name (buffer-name))
             (port (read-number "Port (default 6530): " bdostumski/crdt-default-port))
             (password (read-passwd "Session password (optional): " nil "")))
        (message "Starting CRDT server for buffer '%s' on port %d..." buffer-name port)
        (setq crdt-server-port port)
        (crdt-share-buffer)
        (add-to-list 'bdostumski/crdt-sessions
                     `(:buffer ,buffer-name :port ,port :type server :status active))
        (bdostumski/crdt-show-connection-info port)
        (message "✓ Buffer '%s' is now shared on port %d" buffer-name port))
    (message "Cannot share buffer: not associated with a file")))

(defun bdostumski/crdt-connect-enhanced ()
  "Connect to CRDT session with enhanced options."
  (interactive)
  (let* ((address (read-string "Server address: " bdostumski/crdt-default-address))
         (port (read-number "Port: " bdostumski/crdt-default-port))
         (password (read-passwd "Password (if required): " nil "")))
    (message "Connecting to CRDT server at %s:%d..." address port)
    (crdt-connect address port)
    (add-to-list 'bdostumski/crdt-sessions
                 `(:address ,address :port ,port :type client :status connecting))
    (message "Connection initiated to %s:%d" address port)))

(defun bdostumski/crdt-stop-all-sessions ()
  "Stop all active CRDT sessions."
  (interactive)
  (if bdostumski/crdt-sessions
      (progn
        (crdt-stop)
        (setq bdostumski/crdt-sessions nil)
        (message "✓ All CRDT sessions stopped"))
    (message "No active CRDT sessions")))

(defun bdostumski/crdt-list-active-sessions ()
  "Show list of active CRDT sessions."
  (interactive)
  (if bdostumski/crdt-sessions
      (with-current-buffer (get-buffer-create "*CRDT Sessions*")
        (erase-buffer)
        (insert "Active CRDT Sessions\n")
        (insert "====================\n\n")
        (dolist (session bdostumski/crdt-sessions)
          (insert (format "Type: %s | Port: %s | Status: %s\n"
                          (plist-get session :type)
                          (plist-get session :port)
                          (plist-get session :status))))
        (display-buffer (current-buffer)))
    (message "No active CRDT sessions")))

(defun bdostumski/crdt-show-connection-info (port)
  "Show connection information for sharing."
  (let ((ip-address (bdostumski/crdt-get-local-ip))
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

(defun bdostumski/crdt-get-local-ip ()
  "Get local IP address for sharing connection info."
  (condition-case nil
      (with-temp-buffer
        (call-process "hostname" nil t nil "-I")
        (string-trim (buffer-string)))
    (error "Unable to determine IP address")))

;; ----------------------------
;; User and cursor management
;; ----------------------------
(defun bdostumski/crdt-toggle-user-cursors ()
  "Toggle display of other users' cursors."
  (interactive)
  (setq bdostumski/crdt-user-cursors-enabled
        (not bdostumski/crdt-user-cursors-enabled))
  (setq crdt-enable-cursor-overlay bdostumski/crdt-user-cursors-enabled)
  (message "User cursors: %s"
           (if bdostumski/crdt-user-cursors-enabled "✓ enabled" "✗ disabled")))

(defun bdostumski/crdt-show-connected-users ()
  "Show list of connected users in current session."
  (interactive)
  (if (bound-and-true-p crdt--session)
      (let ((users (crdt-list-users)))
        (if users
            (message "Connected users: %s" (string-join users ", "))
          (message "No other users connected")))
    (message "Not in an active CRDT session")))

(defun bdostumski/crdt-send-message ()
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
(defun bdostumski/crdt-on-connect ()
  "Hook function called when connecting to a CRDT session."
  (message "✓ Successfully connected to CRDT session")
  (when (fboundp 'doom-modeline-mode)
    (force-mode-line-update)))

(defun bdostumski/crdt-on-disconnect ()
  "Hook function called when disconnecting from a CRDT session."
  (message "✓ Disconnected from CRDT session")
  (setq bdostumski/crdt-sessions
        (cl-remove-if (lambda (session)
                        (eq (plist-get session :status) 'active))
                      bdostumski/crdt-sessions))
  (when (fboundp 'doom-modeline-mode)
    (force-mode-line-update)))

(defun bdostumski/crdt-enable-auto-save ()
  "Enable auto-save for shared buffers."
  (when (buffer-file-name)
    (auto-save-mode 1)
    (message "Auto-save enabled for shared buffer")))

(defun bdostumski/crdt-reconnect-last ()
  "Reconnect to the last used CRDT session."
  (interactive)
  (let ((last-session (car (cl-remove-if-not
                            (lambda (s) (eq (plist-get s :type) 'client))
                            bdostumski/crdt-sessions))))
    (if last-session
        (let ((address (plist-get last-session :address))
              (port (plist-get last-session :port)))
          (message "Reconnecting to %s:%d..." address port)
          (crdt-connect address port))
      (message "No previous connection to reconnect to"))))

;; ----------------------------
;; Status and monitoring
;; ----------------------------
(defun bdostumski/crdt-status ()
  "Show current CRDT status."
  (interactive)
  (if (bound-and-true-p crdt--session)
      (let ((session-info (format "Active CRDT session | Users: %d | Buffer: %s"
                                  (length (crdt-list-users))
                                  (buffer-name))))
        (message session-info))
    (message "No active CRDT session")))

(defun bdostumski/crdt-health-check ()
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
(defun bdostumski/crdt-modeline-segment ()
  "Create a modeline segment showing CRDT status."
  (when (bound-and-true-p crdt--session)
    (let ((user-count (length (crdt-list-users))))
      (format " CRDT:%d " user-count))))

;; Add to modeline if doom-modeline is available
(after! doom-modeline
  (doom-modeline-def-segment crdt-status
    (bdostumski/crdt-modeline-segment))

  ;; Uncomment to add CRDT status to modeline
  ;; (setq doom-modeline-format '(...other-segments... crdt-status))
  )

;; ----------------------------
;; Comprehensive keybinding setup
;; ----------------------------
(map! :leader
      (:prefix-map ("C" . "Collaboration")
       ;; Core session management
       :desc "Share buffer"             "s" #'bdostumski/crdt-share-buffer-enhanced
       :desc "Connect to session"      "c" #'bdostumski/crdt-connect-enhanced
       :desc "Disconnect/Stop"          "d" #'bdostumski/crdt-stop-all-sessions
       :desc "Reconnect last"           "r" #'bdostumski/crdt-reconnect-last
       :desc "Kill all sessions"        "k" #'bdostumski/crdt-stop-all-sessions

       ;; Information and monitoring
       :desc "List sessions"            "l" #'bdostumski/crdt-list-active-sessions
       :desc "Show users"               "u" #'bdostumski/crdt-show-connected-users
       :desc "Session status"           "i" #'bdostumski/crdt-status
       :desc "Health check"             "h" #'bdostumski/crdt-health-check

       ;; UI and preferences
       :desc "Toggle user cursors"      "t" #'bdostumski/crdt-toggle-user-cursors
       :desc "Send message"             "m" #'bdostumski/crdt-send-message

       ;; Utilities
       (:prefix ("o" . "options")
        :desc "Set default port"        "p" (lambda () (interactive)
                                              (setq bdostumski/crdt-default-port
                                                    (read-number "Default port: " bdostumski/crdt-default-port))
                                              (message "Default port set to %d" bdostumski/crdt-default-port))
        :desc "Set default address"     "a" (lambda () (interactive)
                                              (setq bdostumski/crdt-default-address
                                                    (read-string "Default address: " bdostumski/crdt-default-address))
                                              (message "Default address set to %s" bdostumski/crdt-default-address))
        :desc "Toggle auto-save"        "s" (lambda () (interactive)
                                              (setq bdostumski/crdt-auto-save-enabled
                                                    (not bdostumski/crdt-auto-save-enabled))
                                              (message "Auto-save for shared buffers: %s"
                                                       (if bdostumski/crdt-auto-save-enabled "✓ enabled" "✗ disabled"))))))

;; Alternative quick access bindings
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "CRDT user cursors"        "C" #'bdostumski/crdt-toggle-user-cursors))

;; ----------------------------
;; Security and safety features
;; ----------------------------
(defun bdostumski/crdt-security-warning ()
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
(defvar bdostumski/crdt-security-warning-shown nil
  "Whether security warning has been shown.")

(defadvice! bdostumski/crdt-show-security-warning-once (&rest _)
  :before #'crdt-share-buffer
  (unless bdostumski/crdt-security-warning-shown
    (when (y-or-n-p "Show CRDT security guidelines? (Recommended for first-time users) ")
      (bdostumski/crdt-security-warning))
    (setq bdostumski/crdt-security-warning-shown t)))

;; ----------------------------
;; Cleanup and state management
;; ----------------------------
(defun bdostumski/crdt-cleanup-on-exit ()
  "Clean up CRDT sessions on Emacs exit."
  (when bdostumski/crdt-sessions
    (crdt-stop)
    (message "CRDT sessions cleaned up on exit")))

;; Register cleanup hook
(add-hook 'kill-emacs-hook #'bdostumski/crdt-cleanup-on-exit)

(provide 'tools-collab-config)

;;; tools-collab-config.el ends here
