;;; module/config/tools-config/tools-docker-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Docker and container development integration for Doom Emacs
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
;; KEYBINDINGS:
;; Leader Key Bindings (SPC):
;;   SPC o d     - Open Docker management interface
;;   SPC d o     - Docker operations menu
;;
;; Docker-specific (SPC D):
;;   SPC D c     - Container management
;;   SPC D i     - Image management
;;   SPC D n     - Network management
;;   SPC D v     - Volume management
;;   SPC D C     - Docker Compose operations
;;   SPC D b     - Build operations
;;   SPC D l     - Logs and monitoring
;;   SPC D r     - Registry operations
;;   SPC D s     - System and cleanup
;;   SPC D t     - Templates and scaffolding
;;   SPC D h     - Help and documentation
;;
;; DEVELOPMENT WORKFLOW:
;; 1. Create Dockerfile/docker-compose.yml with templates
;; 2. Build and test containers locally
;; 3. Manage running containers and services
;; 4. Monitor logs and debug issues
;; 5. Clean up resources when done

;;; Code:

;; ----------------------------
;; State tracking and configuration
;; ----------------------------
(defvar bdostumski/docker-initialized nil
  "Track if Docker integration has been properly initialized.")

(defvar bdostumski/docker-compose-files
  '("docker-compose.yml" "docker-compose.yaml"
    "docker-compose.dev.yml" "docker-compose.prod.yml"
    "docker-compose.test.yml" "docker-compose.override.yml")
  "List of Docker Compose file patterns to recognize.")

(defvar bdostumski/dockerfile-templates
  '(("node" . "# Node.js Dockerfile template\nFROM node:18-alpine\n\nWORKDIR /app\nCOPY package*.json ./\nRUN npm ci --only=production\n\nCOPY . .\nEXPOSE 3000\n\nUSER node\nCMD [\"npm\", \"start\"]\n")
    ("python" . "# Python Dockerfile template\nFROM python:3.11-slim\n\nWORKDIR /app\nCOPY requirements.txt .\nRUN pip install --no-cache-dir -r requirements.txt\n\nCOPY . .\nEXPOSE 8000\n\nCMD [\"python\", \"app.py\"]\n")
    ("golang" . "# Go Dockerfile template\nFROM golang:1.21-alpine AS builder\n\nWORKDIR /app\nCOPY go.mod go.sum ./\nRUN go mod download\n\nCOPY . .\nRUN CGO_ENABLED=0 GOOS=linux go build -o main .\n\nFROM alpine:latest\nRUN apk --no-cache add ca-certificates\nWORKDIR /root/\nCOPY --from=builder /app/main .\nEXPOSE 8080\nCMD [\"./main\"]\n")
    ("nginx" . "# Nginx Dockerfile template\nFROM nginx:alpine\n\nCOPY nginx.conf /etc/nginx/nginx.conf\nCOPY dist/ /usr/share/nginx/html/\n\nEXPOSE 80\n\nCMD [\"nginx\", \"-g\", \"daemon off;\"]\n")
    ("java" . "# Java Dockerfile template\nFROM openjdk:17-jre-slim\n\nWORKDIR /app\nCOPY target/*.jar app.jar\n\nEXPOSE 8080\n\nCMD [\"java\", \"-jar\", \"app.jar\"]\n"))
  "Templates for common Dockerfile configurations.")

(defvar bdostumski/docker-compose-templates
  '(("webapp" . "# Web application docker-compose template\nversion: '3.8'\n\nservices:\n  app:\n    build: .\n    ports:\n      - \"3000:3000\"\n    environment:\n      - NODE_ENV=development\n    volumes:\n      - .:/app\n      - /app/node_modules\n    depends_on:\n      - db\n\n  db:\n    image: postgres:15-alpine\n    environment:\n      - POSTGRES_DB=myapp\n      - POSTGRES_USER=user\n      - POSTGRES_PASSWORD=password\n    volumes:\n      - postgres_data:/var/lib/postgresql/data\n    ports:\n      - \"5432:5432\"\n\nvolumes:\n  postgres_data:\n")
    ("microservices" . "# Microservices docker-compose template\nversion: '3.8'\n\nservices:\n  frontend:\n    build: ./frontend\n    ports:\n      - \"3000:3000\"\n    depends_on:\n      - api\n\n  api:\n    build: ./api\n    ports:\n      - \"8000:8000\"\n    environment:\n      - DATABASE_URL=postgres://user:password@db:5432/myapp\n    depends_on:\n      - db\n      - redis\n\n  db:\n    image: postgres:15-alpine\n    environment:\n      - POSTGRES_DB=myapp\n      - POSTGRES_USER=user\n      - POSTGRES_PASSWORD=password\n    volumes:\n      - postgres_data:/var/lib/postgresql/data\n\n  redis:\n    image: redis:7-alpine\n    ports:\n      - \"6379:6379\"\n\nvolumes:\n  postgres_data:\n"))
  "Templates for common Docker Compose configurations.")

;; ----------------------------
;; Enhanced Docker package configuration
;; ----------------------------
(use-package! docker
  :commands (docker-containers
             docker-images
             docker-volumes
             docker-networks
             docker)

  :init
  ;; Pre-configuration
  (setq docker-show-status t                     ; Show container status
        docker-show-messages t)                  ; Show command messages

  :config
  ;; Core Docker configuration
  (setq docker-container-shell-file-name "/bin/bash"  ; Default shell for exec
        docker-container-default-sort-key "name"      ; Sort containers by name
        docker-image-default-sort-key "repository")   ; Sort images by repo

  ;; Set up Docker mode hooks
  (add-hook 'docker-container-mode-hook #'bdostumski/docker-container-mode-setup)
  (add-hook 'docker-image-mode-hook #'bdostumski/docker-image-mode-setup)

  (message "✓ Docker integration loaded"))

;; ----------------------------
;; Dockerfile mode configuration
;; ----------------------------
(use-package! dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("Dockerfile\\..*\\'" . dockerfile-mode)
         ("\\.dockerfile\\'" . dockerfile-mode))

  :hook (dockerfile-mode . bdostumski/dockerfile-mode-setup)

  :config
  ;; Dockerfile-specific settings
  (setq dockerfile-mode-command "docker")        ; Docker command to use

  ;; Add custom font-lock for better syntax highlighting
  (font-lock-add-keywords 'dockerfile-mode
                          '(("^\\(HEALTHCHECK\\|SHELL\\|STOPSIGNAL\\)" 1 font-lock-keyword-face)))

  (message "✓ Dockerfile mode configured"))

;; ----------------------------
;; Docker Compose mode configuration
;; ----------------------------
(use-package! docker-compose-mode
  :mode (("docker-compose\\.ya?ml\\'" . docker-compose-mode)
         ("docker-compose\\..+\\.ya?ml\\'" . docker-compose-mode))

  :hook (docker-compose-mode . bdostumski/docker-compose-mode-setup)

  :config
  (message "✓ Docker Compose mode configured"))

;; ----------------------------
;; Mode setup hooks
;; ----------------------------
(defun bdostumski/dockerfile-mode-setup ()
  "Setup hook for Dockerfile mode."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)

  ;; Enable aggressive indent for better formatting
  (when (fboundp 'aggressive-indent-mode)
    (aggressive-indent-mode 1))

  ;; Set up local keybindings
  (bdostumski/docker-setup-dockerfile-keys))

(defun bdostumski/docker-compose-mode-setup ()
  "Setup hook for Docker Compose mode."
  (setq-local tab-width 2)
  (setq-local yaml-indent-offset 2)

  ;; Set up local keybindings
  (bdostumski/docker-setup-compose-keys))

(defun bdostumski/docker-container-mode-setup ()
  "Setup hook for Docker container list mode."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

(defun bdostumski/docker-image-mode-setup ()
  "Setup hook for Docker image list mode."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

;; ----------------------------
;; Container management functions
;; ----------------------------
(defun bdostumski/docker-containers ()
  "Show Docker containers with enhanced interface."
  (interactive)
  (docker-containers)
  (message "Docker containers - Use 'h' for help, 'r' to refresh"))

(defun bdostumski/docker-images ()
  "Show Docker images with enhanced interface."
  (interactive)
  (docker-images)
  (message "Docker images - Use 'h' for help, 'r' to refresh"))

(defun bdostumski/docker-quick-exec ()
  "Quickly exec into a running container."
  (interactive)
  (let* ((containers (bdostumski/docker-get-running-containers))
         (container (if (= (length containers) 1)
                        (car containers)
                      (completing-read "Container: " containers))))
    (if container
        (let ((shell (read-string "Shell (default: /bin/bash): " "/bin/bash")))
          (docker-container-exec container shell))
      (message "No running containers found"))))

(defun bdostumski/docker-quick-logs ()
  "Quickly view logs for a container."
  (interactive)
  (let* ((containers (bdostumski/docker-get-all-containers))
         (container (completing-read "Container: " containers)))
    (when container
      (docker-container-logs container))))

(defun bdostumski/docker-get-running-containers ()
  "Get list of running container names."
  (let ((output (shell-command-to-string "docker ps --format '{{.Names}}'")))
    (split-string (string-trim output) "\n" t)))

(defun bdostumski/docker-get-all-containers ()
  "Get list of all container names."
  (let ((output (shell-command-to-string "docker ps -a --format '{{.Names}}'")))
    (split-string (string-trim output) "\n" t)))

;; ----------------------------
;; Docker Compose operations
;; ----------------------------
(defun bdostumski/docker-compose-up ()
  "Run docker-compose up with options."
  (interactive)
  (let* ((compose-file (bdostumski/docker-find-compose-file))
         (detached (y-or-n-p "Run in detached mode? "))
         (build (y-or-n-p "Rebuild images? "))
         (cmd (concat "docker-compose"
                      (if compose-file (format " -f %s" compose-file) "")
                      " up"
                      (if detached " -d" "")
                      (if build " --build" ""))))
    (if compose-file
        (progn
          (message "Running: %s" cmd)
          (async-shell-command cmd "*Docker Compose*"))
      (message "No docker-compose.yml file found"))))

(defun bdostumski/docker-compose-down ()
  "Run docker-compose down."
  (interactive)
  (let* ((compose-file (bdostumski/docker-find-compose-file))
         (volumes (y-or-n-p "Remove volumes? "))
         (cmd (concat "docker-compose"
                      (if compose-file (format " -f %s" compose-file) "")
                      " down"
                      (if volumes " -v" ""))))
    (if compose-file
        (progn
          (message "Running: %s" cmd)
          (async-shell-command cmd "*Docker Compose*"))
      (message "No docker-compose.yml file found"))))

(defun bdostumski/docker-compose-logs ()
  "View Docker Compose logs."
  (interactive)
  (let* ((compose-file (bdostumski/docker-find-compose-file))
         (follow (y-or-n-p "Follow logs? "))
         (cmd (concat "docker-compose"
                      (if compose-file (format " -f %s" compose-file) "")
                      " logs"
                      (if follow " -f" ""))))
    (if compose-file
        (async-shell-command cmd "*Docker Compose Logs*")
      (message "No docker-compose.yml file found"))))

(defun bdostumski/docker-find-compose-file ()
  "Find the nearest docker-compose file."
  (let ((project-root (or (projectile-project-root) default-directory)))
    (cl-loop for file in bdostumski/docker-compose-files
             for path = (expand-file-name file project-root)
             when (file-exists-p path) return file)))

;; ----------------------------
;; Build and development operations
;; ----------------------------
(defun bdostumski/docker-build ()
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

(defun bdostumski/docker-run ()
  "Run Docker container with interactive options."
  (interactive)
  (let* ((images (bdostumski/docker-get-images))
         (image (completing-read "Image: " images))
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
    (if interactive-mode
        (term cmd)
      (async-shell-command cmd "*Docker Run*"))))

(defun bdostumski/docker-get-images ()
  "Get list of Docker images."
  (let ((output (shell-command-to-string "docker images --format '{{.Repository}}:{{.Tag}}'")))
    (split-string (string-trim output) "\n" t)))

;; ----------------------------
;; Template and scaffolding functions
;; ----------------------------
(defun bdostumski/docker-create-dockerfile ()
  "Create a Dockerfile with template selection."
  (interactive)
  (let* ((template-name (completing-read "Dockerfile template: "
                                         (mapcar #'car bdostumski/dockerfile-templates)
                                         nil t))
         (template-content (cdr (assoc template-name bdostumski/dockerfile-templates)))
         (dockerfile-path (expand-file-name "Dockerfile" default-directory)))

    (if (file-exists-p dockerfile-path)
        (if (y-or-n-p "Dockerfile already exists. Overwrite? ")
            (bdostumski/docker-write-dockerfile dockerfile-path template-content)
          (message "Cancelled"))
      (bdostumski/docker-write-dockerfile dockerfile-path template-content))

    (find-file dockerfile-path)))

(defun bdostumski/docker-create-compose-file ()
  "Create a docker-compose.yml with template selection."
  (interactive)
  (let* ((template-name (completing-read "Docker Compose template: "
                                         (mapcar #'car bdostumski/docker-compose-templates)
                                         nil t))
         (template-content (cdr (assoc template-name bdostumski/docker-compose-templates)))
         (compose-path (expand-file-name "docker-compose.yml" default-directory)))

    (if (file-exists-p compose-path)
        (if (y-or-n-p "docker-compose.yml already exists. Overwrite? ")
            (bdostumski/docker-write-compose-file compose-path template-content)
          (message "Cancelled"))
      (bdostumski/docker-write-compose-file compose-path template-content))

    (find-file compose-path)))

(defun bdostumski/docker-write-dockerfile (path content)
  "Write Dockerfile with given content."
  (with-temp-file path
    (insert content)
    (insert (format "\n# Generated by %s on %s\n"
                    user-login-name
                    (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (message "✓ Created Dockerfile: %s" (abbreviate-file-name path)))

(defun bdostumski/docker-write-compose-file (path content)
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
(defun bdostumski/docker-system-prune ()
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

(defun bdostumski/docker-show-system-info ()
  "Show Docker system information."
  (interactive)
  (async-shell-command "docker system df && echo '\n--- Docker Info ---\n' && docker system info"
                       "*Docker System Info*"))

;; ----------------------------
;; Monitoring and utilities
;; ----------------------------
(defun bdostumski/docker-stats ()
  "Show Docker container stats."
  (interactive)
  (async-shell-command "docker stats" "*Docker Stats*"))

(defun bdostumski/docker-top ()
  "Show processes in Docker containers."
  (interactive)
  (let* ((containers (bdostumski/docker-get-running-containers))
         (container (completing-read "Container: " containers)))
    (when container
      (async-shell-command (format "docker top %s" container)
                           (format "*Docker Top - %s*" container)))))

;; ----------------------------
;; Buffer-local keybindings
;; ----------------------------
(defun bdostumski/docker-setup-dockerfile-keys ()
  "Setup buffer-local keybindings for Dockerfile mode."
  (map! :localleader
        :map dockerfile-mode-map
        :desc "Build image"              "b" #'bdostumski/docker-build
        :desc "Run container"            "r" #'bdostumski/docker-run
        :desc "Create template"          "t" #'bdostumski/docker-create-dockerfile))

(defun bdostumski/docker-setup-compose-keys ()
  "Setup buffer-local keybindings for Docker Compose mode."
  (map! :localleader
        :map docker-compose-mode-map
        :desc "Up"                       "u" #'bdostumski/docker-compose-up
        :desc "Down"                     "d" #'bdostumski/docker-compose-down
        :desc "Logs"                     "l" #'bdostumski/docker-compose-logs
        :desc "Create template"          "t" #'bdostumski/docker-create-compose-file))

;; ----------------------------
;; Comprehensive keybinding setup
;; ----------------------------
(map! :leader
      ;; Quick access
      (:prefix-map ("o" . "open")
       :desc "Docker management"        "d" #'docker)

      ;; Alternative quick access
      (:prefix-map ("d" . "debug")
       :desc "Docker operations"        "o" #'docker)

      ;; Main Docker prefix
      (:prefix-map ("D" . "Docker")
                   ;; Container management
                   (:prefix ("c" . "containers")
                    :desc "List containers"         "c" #'bdostumski/docker-containers
                    :desc "Quick exec"              "e" #'bdostumski/docker-quick-exec
                    :desc "Quick logs"              "l" #'bdostumski/docker-quick-logs
                    :desc "Container stats"         "s" #'bdostumski/docker-stats
                    :desc "Container top"           "t" #'bdostumski/docker-top)

                   ;; Image management
                   (:prefix ("i" . "images")
                    :desc "List images"             "i" #'bdostumski/docker-images
                    :desc "Build image"             "b" #'bdostumski/docker-build
                    :desc "Run image"               "r" #'bdostumski/docker-run
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
                    :desc "Up"                      "u" #'bdostumski/docker-compose-up
                    :desc "Down"                    "d" #'bdostumski/docker-compose-down
                    :desc "Logs"                    "l" #'bdostumski/docker-compose-logs
                    :desc "Build"                   "b" #'docker-compose-build
                    :desc "Pull"                    "p" #'docker-compose-pull
                    :desc "Restart"                 "r" #'docker-compose-restart)

                   ;; Build operations
                   (:prefix ("b" . "build")
                    :desc "Build image"             "b" #'bdostumski/docker-build
                    :desc "Compose build"           "c" #'docker-compose-build)

                   ;; Logs and monitoring
                   (:prefix ("l" . "logs")
                    :desc "Container logs"          "c" #'bdostumski/docker-quick-logs
                    :desc "Compose logs"            "C" #'bdostumski/docker-compose-logs
                    :desc "Container stats"         "s" #'bdostumski/docker-stats)

                   ;; System operations
                   (:prefix ("s" . "system")
                    :desc "System info"             "i" #'bdostumski/docker-show-system-info
                    :desc "System prune"            "p" #'bdostumski/docker-system-prune
                    :desc "System stats"            "s" #'bdostumski/docker-stats)

                   ;; Templates and scaffolding
                   (:prefix ("t" . "templates")
                    :desc "Create Dockerfile"       "d" #'bdostumski/docker-create-dockerfile
                    :desc "Create Compose file"     "c" #'bdostumski/docker-create-compose-file)

                   ;; Help and documentation
                   (:prefix ("h" . "help")
                    :desc "Docker help"             "d" (lambda () (interactive) (async-shell-command "docker --help" "*Docker Help*"))
                    :desc "Compose help"            "c" (lambda () (interactive) (async-shell-command "docker-compose --help" "*Docker Compose Help*"))
                    :desc "System info"             "i" #'bdostumski/docker-show-system-info)))

;; ----------------------------
;; Integration with other packages
;; ----------------------------
(after! projectile
  ;; Add Docker files to projectile's project indicators
  (add-to-list 'projectile-project-root-files "Dockerfile")
  (add-to-list 'projectile-project-root-files "docker-compose.yml")
  (add-to-list 'projectile-project-root-files "docker-compose.yaml"))

(after! flycheck
  ;; Add Docker linting if hadolint is available
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
(defun bdostumski/docker-cleanup-on-exit ()
  "Clean up Docker integration on Emacs exit."
  (when bdostumski/docker-initialized
    (message "Docker integration cleaned up")))

;; Register cleanup hook
(add-hook 'kill-emacs-hook #'bdostumski/docker-cleanup-on-exit)

;; Set initialization flag
(setq bdostumski/docker-initialized t)

(provide 'tools-docker-config)

;;; tools-docker-config.el ends here
