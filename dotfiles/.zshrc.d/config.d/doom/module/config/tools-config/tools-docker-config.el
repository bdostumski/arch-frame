;;; module/config/tools-config/tools-docker-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Docker and container development integration for Doom Emacs
;;
;; CHANGES IN THIS FIX:
;; - Fixed unbalanced parentheses in mode setup hooks
;; - Replaced potentially-missing `docker-container-exec` with a robust wrapper
;; - Added smart detection for `docker compose` vs `docker-compose`
;; - Added wrapper commands for compose build/pull/restart to avoid missing fns
;; - Made interactive run use vterm when available; otherwise async shell
;; - Minor robustness tweaks & messages
;;
;; FEATURES:
;; - Docker and Docker Compose syntax highlighting and editing
;; - Container management (list, start, stop, logs, exec)
;; - Image management and building workflows
;; - Docker Compose orchestration with multi-service support
;; - Development environment integration
;; - Dockerfile and docker-compose.yml templates
;; - Real-time container monitoring and logs
;; - Integration with project management and LSP
;; - Security scanning and best practices validation
;;
;; SUPPORTED FILES:
;; - Dockerfile, Dockerfile.*
;; - docker-compose.yml, docker-compose.yaml
;; - .dockerignore
;; - Multi-stage Dockerfiles
;; - Docker Swarm configurations
;;
;; DEVELOPMENT WORKFLOW:
;; 1. Create Dockerfile/docker-compose.yml with templates
;; 2. Build and test containers locally
;; 3. Manage running containers and services
;; 4. Monitor logs and debug issues
;; 5. Clean up resources when done

;;; Code:

(require 'cl-lib)

;; ----------------------------
;; State tracking and configuration
;; ----------------------------
(defvar +docker/docker-initialized nil
  "Track if Docker integration has been properly initialized.")

(defvar +docker/docker-compose-files
  '("docker-compose.yml" "docker-compose.yaml"
    "docker-compose.dev.yml" "docker-compose.prod.yml"
    "docker-compose.test.yml" "docker-compose.override.yml")
  "List of Docker Compose file patterns to recognize.")

(defvar +docker/dockerfile-templates
  '(("node" . "# Node.js Dockerfile template\nFROM node:18-alpine\n\nWORKDIR /app\nCOPY package*.json ./\nRUN npm ci --only=production\n\nCOPY . .\nEXPOSE 3000\n\nUSER node\nCMD [\"npm\", \"start\"]\n")
    ("python" . "# Python Dockerfile template\nFROM python:3.11-slim\n\nWORKDIR /app\nCOPY requirements.txt .\nRUN pip install --no-cache-dir -r requirements.txt\n\nCOPY . .\nEXPOSE 8000\n\nCMD [\"python\", \"app.py\"]\n")
    ("golang" . "# Go Dockerfile template\nFROM golang:1.21-alpine AS builder\n\nWORKDIR /app\nCOPY go.mod go.sum ./\nRUN go mod download\n\nCOPY . .\nRUN CGO_ENABLED=0 GOOS=linux go build -o main .\n\nFROM alpine:latest\nRUN apk --no-cache add ca-certificates\nWORKDIR /root/\nCOPY --from=builder /app/main .\nEXPOSE 8080\nCMD [\"./main\"]\n")
    ("nginx" . "# Nginx Dockerfile template\nFROM nginx:alpine\n\nCOPY nginx.conf /etc/nginx/nginx.conf\nCOPY dist/ /usr/share/nginx/html/\n\nEXPOSE 80\n\nCMD [\"nginx\", \"-g\", \"daemon off;\"]\n")
    ("java" . "# Java Dockerfile template\nFROM openjdk:17-jre-slim\n\nWORKDIR /app\nCOPY target/*.jar app.jar\n\nEXPOSE 8080\n\nCMD [\"java\", \"-jar\", \"app.jar\"]\n"))
  "Templates for common Dockerfile configurations.")

(defvar +docker/docker-compose-templates
  '(("webapp" . "# Web application docker-compose template\nversion: '3.8'\n\nservices:\n  app:\n    build: .\n    ports:\n      - \"3000:3000\"\n    environment:\n      - NODE_ENV=development\n    volumes:\n      - .:/app\n      - /app/node_modules\n    depends_on:\n      - db\n\n  db:\n    image: postgres:15-alpine\n    environment:\n      - POSTGRES_DB=myapp\n      - POSTGRES_USER=user\n      - POSTGRES_PASSWORD=password\n    volumes:\n      - postgres_data:/var/lib/postgresql/data\n    ports:\n      - \"5432:5432\"\n\nvolumes:\n  postgres_data:\n")
    ("microservices" . "# Microservices docker-compose template\nversion: '3.8'\n\nservices:\n  frontend:\n    build: ./frontend\n    ports:\n      - \"3000:3000\"\n    depends_on:\n      - api\n\n  api:\n    build: ./api\n    ports:\n      - \"8000:8000\"\n    environment:\n      - DATABASE_URL=postgres://user:password@db:5432/myapp\n    depends_on:\n      - db\n      - redis\n\n  db:\n    image: postgres:15-alpine\n    environment:\n      - POSTGRES_DB=myapp\n      - POSTGRES_USER=user\n      - POSTGRES_PASSWORD=password\n    volumes:\n      - postgres_data:/var/lib/postgresql/data\n\n  redis:\n    image: redis:7-alpine\n    ports:\n      - \"6379:6379\"\n\nvolumes:\n  postgres_data:\n"))
  "Templates for common Docker Compose configurations.")

;; ----------------------------
;; Enhanced Docker package configuration
;; ----------------------------
(use-package! docker
  :commands (docker docker-containers docker-images docker-volumes docker-networks)
  :init
  (setq docker-show-status t
        docker-show-messages t)
  :config
  (setq docker-container-shell-file-name "/bin/bash"
        docker-container-default-sort-key "name"
        docker-image-default-sort-key "repository")
  (add-hook 'docker-container-mode-hook #'+docker/docker-container-mode-setup)
  (add-hook 'docker-image-mode-hook #'+docker/docker-image-mode-setup)
  (message "✓ Docker integration loaded"))

;; ----------------------------
;; Dockerfile mode configuration
;; ----------------------------
(use-package! dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("Dockerfile\\..*\\'" . dockerfile-mode)
         ("\\.dockerfile\\'" . dockerfile-mode))
  :hook (dockerfile-mode . +docker/dockerfile-mode-setup)
  :config
  (setq dockerfile-mode-command "docker")
  (font-lock-add-keywords 'dockerfile-mode
                          '(("^\\(HEALTHCHECK\\|SHELL\\|STOPSIGNAL\\)" 1 font-lock-keyword-face)))
  (message "✓ Dockerfile mode configured"))

;; ----------------------------
;; Docker Compose mode configuration
;; ----------------------------
(use-package! docker-compose-mode
  :mode (("docker-compose\\.ya?ml\\'" . docker-compose-mode)
         ("docker-compose\\..+\\.ya?ml\\'" . docker-compose-mode))
  :hook (docker-compose-mode . +docker/docker-compose-mode-setup)
  :config
  (message "✓ Docker Compose mode configured"))

;; ----------------------------
;; Mode setup hooks (FIXED parens)
;; ----------------------------
(defun +docker/dockerfile-mode-setup ()
  "Setup hook for Dockerfile mode."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (when (fboundp 'aggressive-indent-mode)
    (aggressive-indent-mode 1))
  ;; Local keybindings can be added here
  ;; (+docker/docker-setup-dockerfile-keys)
  )

(defun +docker/docker-compose-mode-setup ()
  "Setup hook for Docker Compose mode."
  (setq-local tab-width 2)
  (setq-local yaml-indent-offset 2)
  ;; Local keybindings can be added here
  ;; (+docker/docker-setup-compose-keys)
  )

(defun +docker/docker-container-mode-setup ()
  "Setup hook for Docker container list mode."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

(defun +docker/docker-image-mode-setup ()
  "Setup hook for Docker image list mode."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

;; ----------------------------
;; Helpers
;; ----------------------------
(defun +docker/compose-cmd ()
  "Return the preferred docker compose executable."
  (cond ((executable-find "docker") "docker compose")
        ((executable-find "docker-compose") "docker-compose")
        (t (user-error "Neither 'docker' nor 'docker-compose' found in PATH"))))

;; ----------------------------
;; Container management functions
;; ----------------------------
(defun +docker/docker-containers ()
  "Show Docker containers with enhanced interface."
  (interactive)
  (docker-containers)
  (message "Docker containers - Use 'h' for help, 'r' to refresh"))

(defun +docker/docker-images ()
  "Show Docker images with enhanced interface."
  (interactive)
  (docker-images)
  (message "Docker images - Use 'h' for help, 'r' to refresh"))

(defun +docker/docker-quick-exec ()
  "Quickly exec into a running container (robust wrapper)."
  (interactive)
  (let* ((containers (+docker/docker-get-running-containers))
         (container (cond ((null containers) nil)
                          ((= (length containers) 1) (car containers))
                          (t (completing-read "Container: " containers nil t))))
         (shell (read-string "Shell (default: /bin/bash): " "/bin/bash")))
    (if container
        (let ((cmd (format "docker exec -it %s %s" container shell)))
          (if (featurep 'vterm)
              (progn (vterm (generate-new-buffer-name (format "*docker-exec: %s*" container)))
                     (vterm-send-string cmd) (vterm-send-return))
            (async-shell-command cmd (format "*Docker Exec %s*" container))))
      (message "No running containers found"))))

(defun +docker/docker-quick-logs ()
  "Quickly view logs for a container."
  (interactive)
  (let* ((containers (+docker/docker-get-all-containers))
         (container (and containers (completing-read "Container: " containers nil t))))
    (when container
      (docker-container-logs container))))

(defun +docker/docker-get-running-containers ()
  "Get list of running container names."
  (let ((output (shell-command-to-string "docker ps --format '{{.Names}}'")))
    (if (string-empty-p (string-trim output))
        nil
      (split-string (string-trim output) "\n" t))) )

(defun +docker/docker-get-all-containers ()
  "Get list of all container names."
  (let ((output (shell-command-to-string "docker ps -a --format '{{.Names}}'")))
    (if (string-empty-p (string-trim output))
        nil
      (split-string (string-trim output) "\n" t))) )

;; ----------------------------
;; Docker Compose operations (use smart cmd)
;; ----------------------------
(defun +docker/docker-compose-up ()
  "Run compose up with options."
  (interactive)
  (let* ((compose-file (+docker/docker-find-compose-file))
         (detached (y-or-n-p "Run in detached mode? "))
         (build (y-or-n-p "Rebuild images? "))
         (cmd (concat (+docker/compose-cmd)
                      (if compose-file (format " -f %s" compose-file) "")
                      " up"
                      (if detached " -d" "")
                      (if build " --build" ""))))
    (if compose-file
        (progn (message "Running: %s" cmd)
               (async-shell-command cmd "*Docker Compose*"))
      (message "No docker-compose.yml file found"))))

(defun +docker/docker-compose-down ()
  "Run compose down."
  (interactive)
  (let* ((compose-file (+docker/docker-find-compose-file))
         (volumes (y-or-n-p "Remove volumes? "))
         (cmd (concat (+docker/compose-cmd)
                      (if compose-file (format " -f %s" compose-file) "")
                      " down"
                      (if volumes " -v" ""))))
    (if compose-file
        (progn (message "Running: %s" cmd)
               (async-shell-command cmd "*Docker Compose*"))
      (message "No docker-compose.yml file found"))))

(defun +docker/docker-compose-logs ()
  "View compose logs."
  (interactive)
  (let* ((compose-file (+docker/docker-find-compose-file))
         (follow (y-or-n-p "Follow logs? "))
         (cmd (concat (+docker/compose-cmd)
                      (if compose-file (format " -f %s" compose-file) "")
                      " logs"
                      (if follow " -f" ""))))
    (if compose-file
        (async-shell-command cmd "*Docker Compose Logs*")
      (message "No docker-compose.yml file found"))))

(defun +docker/docker-compose-build ()
  "Compose build wrapper."
  (interactive)
  (let* ((compose-file (+docker/docker-find-compose-file))
         (no-cache (y-or-n-p "No cache? "))
         (cmd (concat (+docker/compose-cmd)
                      (if compose-file (format " -f %s" compose-file) "")
                      " build"
                      (if no-cache " --no-cache" ""))))
    (if compose-file
        (async-shell-command cmd "*Docker Compose Build*")
      (message "No docker-compose.yml file found"))))

(defun +docker/docker-compose-pull ()
  "Compose pull wrapper."
  (interactive)
  (let* ((compose-file (+docker/docker-find-compose-file))
         (cmd (concat (+docker/compose-cmd)
                      (if compose-file (format " -f %s" compose-file) "")
                      " pull")))
    (if compose-file
        (async-shell-command cmd "*Docker Compose Pull*")
      (message "No docker-compose.yml file found"))))

(defun +docker/docker-compose-restart ()
  "Compose restart wrapper."
  (interactive)
  (let* ((compose-file (+docker/docker-find-compose-file))
         (cmd (concat (+docker/compose-cmd)
                      (if compose-file (format " -f %s" compose-file) "")
                      " restart")))
    (if compose-file
        (async-shell-command cmd "*Docker Compose Restart*")
      (message "No docker-compose.yml file found"))))

(defun +docker/docker-find-compose-file ()
  "Find the nearest docker-compose file name at project root or default dir."
  (let ((project-root (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          default-directory)))
    (cl-loop for file in +docker/docker-compose-files
             for path = (expand-file-name file project-root)
             when (file-exists-p path) return file)))

;; ----------------------------
;; Build and development operations
;; ----------------------------
(defun +docker/docker-build ()
  "Build Docker image with options."
  (interactive)
  (let* ((dockerfile (or (and (eq major-mode 'dockerfile-mode)
                              (buffer-file-name))
                         "Dockerfile"))
         (tag (read-string "Image tag: "
                           (format "%s:latest"
                                   (file-name-nondirectory
                                    (directory-file-name default-directory)))))
         (context (read-directory-name "Build context: " default-directory))
         (no-cache (y-or-n-p "No cache? "))
         (cmd (format "docker build -t %s%s -f %s %s"
                      tag
                      (if no-cache " --no-cache" "")
                      dockerfile
                      context)))
    (message "Building image: %s" cmd)
    (async-shell-command cmd "*Docker Build*")))

(defun +docker/docker-run ()
  "Run Docker container with interactive options."
  (interactive)
  (let* ((images (+docker/docker-get-images))
         (image (completing-read "Image: " images nil t))
         (interactive-mode (y-or-n-p "Interactive mode? "))
         (remove-after (y-or-n-p "Remove container after exit? "))
         (port-mapping (read-string "Port mapping (e.g., 8080:80): "))
         (cmd (format "docker run%s%s%s %s"
                      (if interactive-mode " -it" "")
                      (if remove-after " --rm" "")
                      (if (and port-mapping (not (string-empty-p port-mapping)))
                          (format " -p %s" port-mapping) "")
                      image)))
    (message "Running: %s" cmd)
    (if (and interactive-mode (featurep 'vterm))
        (progn (vterm (generate-new-buffer-name "*Docker Run*"))
               (vterm-send-string cmd) (vterm-send-return))
      (async-shell-command cmd "*Docker Run*"))))

(defun +docker/docker-get-images ()
  "Get list of Docker images."
  (let ((output (shell-command-to-string "docker images --format '{{.Repository}}:{{.Tag}}'")))
    (if (string-empty-p (string-trim output))
        nil
      (split-string (string-trim output) "\n" t))) )

;; ----------------------------
;; Template and scaffolding functions
;; ----------------------------
(defun +docker/docker-create-dockerfile ()
  "Create a Dockerfile with template selection."
  (interactive)
  (let* ((template-name (completing-read "Dockerfile template: "
                                         (mapcar #'car +docker/dockerfile-templates)
                                         nil t))
         (template-content (cdr (assoc template-name +docker/dockerfile-templates)))
         (dockerfile-path (expand-file-name "Dockerfile" default-directory)))
    (if (file-exists-p dockerfile-path)
        (if (y-or-n-p "Dockerfile already exists. Overwrite? ")
            (+docker/docker-write-dockerfile dockerfile-path template-content)
          (message "Cancelled"))
      (+docker/docker-write-dockerfile dockerfile-path template-content))
    (find-file dockerfile-path)))

(defun +docker/docker-create-compose-file ()
  "Create a docker-compose.yml with template selection."
  (interactive)
  (let* ((template-name (completing-read "Docker Compose template: "
                                         (mapcar #'car +docker/docker-compose-templates)
                                         nil t))
         (template-content (cdr (assoc template-name +docker/docker-compose-templates)))
         (compose-path (expand-file-name "docker-compose.yml" default-directory)))
    (if (file-exists-p compose-path)
        (if (y-or-n-p "docker-compose.yml already exists. Overwrite? ")
            (+docker/docker-write-compose-file compose-path template-content)
          (message "Cancelled"))
      (+docker/docker-write-compose-file compose-path template-content))
    (find-file compose-path)))

(defun +docker/docker-write-dockerfile (path content)
  "Write Dockerfile with given content."
  (with-temp-file path
    (insert content)
    (insert (format "\n# Generated by %s on %s\n"
                    user-login-name
                    (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (message "✓ Created Dockerfile: %s" (abbreviate-file-name path)))

(defun +docker/docker-write-compose-file (path content)
  "Write docker-compose.yml with given content."
  (with-temp-file path
    (insert content)
    (insert (format "\n# Generated by %s on %s\n"
                    user-login-name
                    (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (message "✓ Created docker-compose.yml: %s" (abbreviate-file-name path)))

;; ----------------------------
;; System and cleanup operations
;; ----------------------------
(defun +docker/docker-system-prune ()
  "Clean up Docker system with options."
  (interactive)
  (let* ((all (y-or-n-p "Remove all unused objects (not just dangling)? "))
         (volumes (y-or-n-p "Remove unused volumes? "))
         (cmd (concat "docker system prune"
                      (if all " -a" "")
                      (if volumes " --volumes" "")
                      " -f")))
    (when (y-or-n-p (format "This will run: %s. Continue? " cmd))
      (message "Cleaning up Docker system...")
      (async-shell-command cmd "*Docker Cleanup*"))))

(defun +docker/docker-show-system-info ()
  "Show Docker system information."
  (interactive)
  (async-shell-command "docker system df && echo '\n--- Docker Info ---\n' && docker system info"
                       "*Docker System Info*"))

;; ----------------------------
;; Monitoring and utilities
;; ----------------------------
(defun +docker/docker-stats ()
  "Show Docker container stats."
  (interactive)
  (async-shell-command "docker stats" "*Docker Stats*"))

(defun +docker/docker-top ()
  "Show processes in Docker containers."
  (interactive)
  (let* ((containers (+docker/docker-get-running-containers))
         (container (and containers (completing-read "Container: " containers nil t))))
    (when container
      (async-shell-command (format "docker top %s" container)
                           (format "*Docker Top - %s*" container)))))

;; ----------------------------
;; Comprehensive keybinding setup
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("o" . "devops")
                                         (:prefix ("d" . "docker")
                                                  ;; Container management
                                                  (:prefix ("c" . "containers")
                                                   :desc "List containers"         "c" #'+docker/docker-containers
                                                   :desc "Quick exec"              "e" #'+docker/docker-quick-exec
                                                   :desc "Quick logs"              "l" #'+docker/docker-quick-logs
                                                   :desc "Container stats"         "s" #'+docker/docker-stats
                                                   :desc "Container top"           "t" #'+docker/docker-top)
                                                  ;; Image management
                                                  (:prefix ("i" . "images")
                                                   :desc "List images"             "i" #'+docker/docker-images
                                                   :desc "Build image"             "b" #'+docker/docker-build
                                                   :desc "Run image"               "r" #'+docker/docker-run
                                                   :desc "Pull image"              "p" #'docker-image-pull)
                                                  ;; Network management
                                                  (:prefix ("n" . "networks")
                                                   :desc "List networks"           "n" #'docker-networks
                                                   :desc "Create network"          "c" #'docker-network-create)
                                                  ;; Volume management
                                                  (:prefix ("v" . "volumes")
                                                   :desc "List volumes"            "v" #'docker-volumes
                                                   :desc "Create volume"           "c" #'docker-volume-create)
                                                  ;; Docker Compose operations
                                                  (:prefix ("C" . "compose")
                                                   :desc "Up"                      "u" #'+docker/docker-compose-up
                                                   :desc "Down"                    "d" #'+docker/docker-compose-down
                                                   :desc "Logs"                    "l" #'+docker/docker-compose-logs
                                                   :desc "Build"                   "b" #'+docker/docker-compose-build
                                                   :desc "Pull"                    "p" #'+docker/docker-compose-pull
                                                   :desc "Restart"                 "r" #'+docker/docker-compose-restart)
                                                  ;; Build operations
                                                  (:prefix ("b" . "build")
                                                   :desc "Build image"             "b" #'+docker/docker-build
                                                   :desc "Compose build"           "c" #'+docker/docker-compose-build)
                                                  ;; Logs and monitoring
                                                  (:prefix ("l" . "logs")
                                                   :desc "Container logs"          "c" #'+docker/docker-quick-logs
                                                   :desc "Compose logs"            "C" #'+docker/docker-compose-logs
                                                   :desc "Container stats"         "s" #'+docker/docker-stats)
                                                  ;; System operations
                                                  (:prefix ("s" . "system")
                                                   :desc "System info"             "i" #'+docker/docker-show-system-info
                                                   :desc "System prune"            "p" #'+docker/docker-system-prune
                                                   :desc "System stats"            "s" #'+docker/docker-stats)
                                                  ;; Templates and scaffolding
                                                  (:prefix ("t" . "templates")
                                                   :desc "Create Dockerfile"       "d" #'+docker/docker-create-dockerfile
                                                   :desc "Create Compose file"     "c" #'+docker/docker-create-compose-file)
                                                  ;; Help and documentation
                                                  (:prefix ("h" . "help")
                                                   :desc "Docker help"             "d"
                                                   (lambda () (interactive) (async-shell-command "docker --help" "*Docker Help*"))
                                                   :desc "Compose help"            "c"
                                                   (lambda () (interactive) (async-shell-command (concat (+docker/compose-cmd) " --help") "*Docker Compose Help*"))
                                                   :desc "System info"             "i" #'+docker/docker-show-system-info))))))

;; ----------------------------
;; Integration with other packages
;; ----------------------------
(after! projectile
  (add-to-list 'projectile-project-root-files "Dockerfile")
  (add-to-list 'projectile-project-root-files "docker-compose.yml")
  (add-to-list 'projectile-project-root-files "docker-compose.yaml"))

(after! flycheck
  (when (executable-find "hadolint")
    (flycheck-define-checker dockerfile-hadolint
      "A Dockerfile syntax checker using hadolint."
      :command ("hadolint" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column " DL" (id (one-or-more digit)) " " (message) line-end))
      :modes (dockerfile-mode))
    (add-to-list 'flycheck-checkers 'dockerfile-hadolint)))

;; ----------------------------
;; Cleanup and state management
;; ----------------------------
(defun +docker/docker-cleanup-on-exit ()
  "Clean up Docker integration on Emacs exit."
  (when +docker/docker-initialized
    (message "Docker integration cleaned up")))

(add-hook 'kill-emacs-hook #'+docker/docker-cleanup-on-exit)
(setq +docker/docker-initialized t)

(provide 'tools-docker-config)

;;; tools-docker-config.el ends here
