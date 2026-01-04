;;; module/emacs-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module configures Emacs' core behavior and built-in tools.
;; It is responsible for essential editing and workflow enhancements,
;; as well as safe integration of built-in and third-party tools.
;;
;; The module includes:
;;   - Core utilities and editing helpers (Dirvish, Electric, Undo)
;;   - Browsing and navigation (EWW, Ibuffer, Ranger)
;;   - Version control (VC)
;;   - Snippets and template management (YASnippet)
;;
;; Load order is designed for safety:
;;   1. Core editor utilities (Dirvish, Electric)
;;   2. Navigation and browsing tools (EWW, Ibuffer, Ranger)
;;   3. Undo/redo system
;;   4. Version control integration
;;   5. Optional snippet/template management

;;; Code:

;; ---------------------------------------------------------------------------
;; Core editor utilities
;; These features provide essential enhancements to editing behavior.
;; ---------------------------------------------------------------------------

;; File management with Dirvish (modern replacement for Dired)
(load! "config/emacs-config/emacs-dirvish-config.el")

;; Electric pair management (auto-insert and balance brackets, quotes, etc.)
(load! "config/emacs-config/emacs-electric-config.el")

;; ---------------------------------------------------------------------------
;; Navigation and browsing
;; Tools for managing buffers, exploring files, and browsing the web.
;; ---------------------------------------------------------------------------

;; EWW web browser integration (text-based browsing inside Emacs)
(load! "config/emacs-config/emacs-eww-config.el")

;; Ibuffer for advanced buffer management and filtering
(load! "config/emacs-config/emacs-ibuffer-config.el")

;; Ranger file manager integration (optional, complements Dirvish)
(load! "config/emacs-config/emacs-ranger-config.el")

;; ---------------------------------------------------------------------------
;; Undo/redo system
;; Enhanced undo/redo system (load optionally if desired)
;; ---------------------------------------------------------------------------
;;(load! "config/emacs-config/emacs-undo-config.el")  ; disabled by default

;; ---------------------------------------------------------------------------
;; Version control integration
;; Built-in support for Git, SVN, and other VCS systems
;; ---------------------------------------------------------------------------
(load! "config/emacs-config/emacs-vc-config.el")

;; ---------------------------------------------------------------------------
;; Snippets and templates
;; Provides YASnippet for code and text templates
;; ---------------------------------------------------------------------------
(load! "config/emacs-config/emacs-yasnippet-config.el")

(provide 'emacs-module)

;;; emacs-module.el ends here
