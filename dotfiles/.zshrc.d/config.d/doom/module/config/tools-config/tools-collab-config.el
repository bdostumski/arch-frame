;;; module/config/tools-config/tools-collab-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Collaborative editing setup using CRDT for Doom Emacs.
;; Provides commands to share, connect, and stop collaborative sessions.

;;; Code:

(use-package! crdt
  :commands (crdt-share-buffer crdt-connect crdt-stop)
  :config
  ;; Optional: set a default server port
  (setq crdt-server-port 6530))

(provide 'tools-collab-config)

;;; tools-collab-config.el ends here
