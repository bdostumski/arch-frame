;;; module/config/tools-config/tools-taskrunner-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Taskrunner configuration for Doom Emacs.
;; Provides integration with project task runners like Make, Rake, npm scripts, etc.
;; Enables auto-refresh of tasks and leader keybindings for running, editing, and rerunning tasks.

;;; Code:

;; ----------------------------
;; Taskrunner core configuration
;; ----------------------------
(after! taskrunner
  ;; Set default runner (e.g., make)
  (setq +taskrunner-default-runner 'make)

  ;; Auto-refresh tasks on project changes
  (setq +taskrunner-auto-refresh t))

;; ----------------------------
;; Leader keybindings for taskrunner
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("t" . "tasks")
;;       :desc "Run task" "r" #'+taskrunner/run
;;       :desc "Run last task" "l" #'+taskrunner/run-last
;;       :desc "Edit tasks" "e" #'+taskrunner/edit))

(provide 'tools-taskrunner-config)

;;; tools-taskrunner-config.el ends here
