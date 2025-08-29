;;; module/config/tools-config/tools-docker-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Docker and Docker Compose integration for Emacs.
;; Provides syntax highlighting, minor modes, and automatic mode switching for Docker-related files.

;;; Code:

(after! docker
  ;; Use dockerfile-mode for Dockerfiles
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

  ;; Docker Compose integration for YAML files
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "docker-compose\\.yml\\|docker-compose\\.yaml" buffer-file-name))
                (docker-compose-mode 1)))))

(provide 'tools-docker-config)

;;; tools-docker-config.el ends here
