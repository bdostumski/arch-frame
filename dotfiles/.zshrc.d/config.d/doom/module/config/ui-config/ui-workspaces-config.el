;;; module/config/ui-config/ui-workspaces-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized workspaces configuration for Doom Emacs.
;; Provides tab-bar workspaces, auto-resume, auto-save, and Vim-like navigation.

;;; Code:

(after! workspaces
  ;; Workspace behavior
  (setq +workspaces-on-switch-project-behavior t   ;; auto-switch when changing projects
        +workspaces-use-tab-bar t                  ;; display tab-bar for workspaces
        +workspaces-auto-resume t                  ;; restore last session workspaces
        +workspaces-auto-save t)                   ;; save workspace state automatically

  ;; Exclude special buffers (e.g., *Messages*, *scratch*) from workspace switching
  (setq +workspaces-buffer-list-filter
        (lambda (buffer) (not (string-match-p "^\\*" (buffer-name buffer)))))

  ;; Vim-like workspace navigation keybindings
   (map! :leader
          (:prefix-map ("e" . "editor")
                (:prefix-map ("u" . "ui")
                       (:prefix ("w" . "workspace")
                        :desc "Next workspace"      "n"  #'+workspace/switch-right
                        :desc "Previous workspace"  "b"  #'+workspace/switch-left
                        :desc "Switch workspace"    "w"  #'+workspace/switch-to
                        :desc "New workspace"       "W"  #'+workspace/new
                        :desc "Delete workspace"    "D"  #'+workspace/delete)))))

(provide 'ui-workspaces-config)

;;; ui-workspaces-config.el ends here
