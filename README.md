<div align="center">

# 🏗️ arch-frame

### Automated Arch Linux Installation & Configuration Framework

[![Arch Linux](https://img.shields.io/badge/Arch_Linux-1793D1?style=for-the-badge&logo=arch-linux&logoColor=white)](https://archlinux.org)
[![Shell Script](https://img.shields.io/badge/Shell_Script-121011?style=for-the-badge&logo=gnu-bash&logoColor=white)](https://www.gnu.org/software/bash/)
[![Zsh](https://img.shields.io/badge/Zsh-F15A24?style=for-the-badge&logo=zsh&logoColor=white)](https://www.zsh.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)](LICENSE)

> **A professional-grade shell scripting framework that automatically installs and configures 100+ GB of software on Arch Linux — with a single command.**


[📖 About](#-about) •
[🚀 Features](#-features) •
[🗂️ Structure](#️-project-structure) •
[📦 Packages](#-what-gets-installed) •
[⚙️ Installation](#️-installation) •
[🔧 Configuration](#-configuration) •
[🛡️ Security](#️-security) •
[🧠 Doom Emacs](#-doom-emacs) •
[⌨️ Key Bindings](#️-key-bindings) •
[📝 Aliases](#-aliases) •
[🔀 Git Config](#-git-configuration)

</div>

---

## 📖 About

**arch-frame** was born from a passion for learning and real-world automation.

It started as a hands-on project while completing the **[Linux Shell Scripting course on Udemy](https://bdostumski.github.io/certificates/udemy-linux-shell-scripting.html)** — a course that taught the foundations of POSIX shell scripting, automation, and system administration. What began as a learning exercise quickly evolved into a fully-fledged, production-quality framework.

### 💡 The Problem It Solves

Setting up a complete Arch Linux development environment from scratch is a **time-consuming, error-prone, and repetitive** process. It can take days of manual work to:
- Install hundreds of packages
- Configure security tools (antivirus, firewall)
- Set up editors, terminals, shells and plugins
- Configure mail, virtualisation, and development languages
- Personalise dotfiles, aliases, keybindings and themes

**arch-frame eliminates all of that.** It automates the entire process end-to-end.

### 🏢 Enterprise Value

This kind of automation has direct value for companies that need to:
- 🖥️ **Provision developer workstations** consistently across a team
- 🔒 **Enforce security policies** (antivirus, firewall, GPG encryption) at installation time
- ⚙️ **Deploy Linux environments** for specific purposes (data science, web dev, DevOps, etc.)
- 📋 **Standardise tooling** across engineering teams without manual setup
- 🔄 **Rebuild environments** quickly after hardware failure or OS reinstall

---

## 🚀 Features

| Feature | Description |
|---------|-------------|
| 🤖 **One-command setup** | Full system from scratch with a single interactive script |
| 🐚 **Dual shell support** | Shared config works seamlessly in both `zsh` and `bash` |
| 📦 **100+ GB of software** | Packages organised by category — kernel, fonts, dev tools, GUI apps, virtualisation |
| 🎨 **Beautiful terminal** | Powerlevel10k Pure theme + Nerd Fonts + `lsd` + `bat` + `vivid` colours |
| 🔌 **Plugin management** | Zinit with autosuggestions, syntax highlighting, fzf-tab completions |
| 🛡️ **Security built-in** | ClamAV antivirus (with on-access scanning) + UFW firewall + GPG encryption |
| 📧 **Email in terminal** | mu4e + isync + msmtp — full email workflow inside Emacs/terminal |
| 🧠 **Doom Emacs** | Pre-configured IDE with LSP, org-mode, mu4e, Magit, and more |
| 🔀 **Rich Git config** | Auto-generated `.gitconfig` with 100+ aliases, delta diffs, kitty difftool |
| 🖥️ **Virtualisation ready** | QEMU/KVM + VirtualBox fully pre-configured |
| 🔧 **Polyglot dev env** | Python, Node, Go, Rust, Java (11/17/21), Ruby, Kotlin, Clojure, Haskell, Scala |
| 💾 **Safe backups** | All existing configs backed up as `.bak` before replacing |
| 🧩 **Template system** | Built-in templates for Git, Docker, Docker Compose, Kubernetes, Vagrant |
| ⚡ **Idempotent** | Skips already-installed packages, safe to re-run |

---

## 🗂️ Project Structure

```
arch-frame/
│
├── install.sh                          # 🚀 Main entry point (interactive menu)
├── install-config.sh                   # ⚙️  Your personal configuration (fill this in)
│
├── dependencies/
│   ├── pacman-packages.sh              # Main package installer (pacman + dotfiles + tools)
│   ├── yay-packages.sh                 # AUR package installer
│   ├── git-packages.sh                 # Packages installed from git
│   ├── dev-tools.sh                    # Dev tools: Docker, Vagrant, K8s, etc.
│   ├── drivers.sh                      # System drivers & firmware
│   ├── doom-emacs.sh                   # Doom Emacs installer & configuration
│   │
│   ├── packages/
│   │   ├── pkg-pacman.sh               # Pacman package list (100+ packages)
│   │   └── pkg-yay.sh                  # AUR package list (VSCode, IntelliJ, Postman...)
│   │
│   ├── configurations/
│   │   ├── config-env-variables.sh     # Generate ~/.env.sh (GPG-encrypted credentials)
│   │   ├── config-gitconfig.sh         # Generate ~/.gitconfig (100+ git aliases)
│   │   ├── config-ufw.sh               # UFW firewall rules (HTTP/HTTPS/SSH/VNC)
│   │   ├── config-clamav.sh            # ClamAV full setup (daemon, freshclam, clamonacc)
│   │   └── config-vbox.sh              # VirtualBox guest drivers
│   │
│   └── utils/
│       └── install-utils.sh            # Shared utilities: log(), backup_and_copy(),
│                                       # install_pacman_packages(), install_yay_packages()
│
└── dotfiles/
    ├── .zshrc                          # Zsh entry point
    ├── .bashrc                         # Bash entry point
    │
    ├── .zshrc.d/                       # Zsh-specific modules
    │   ├── initialize.zsh              # Zinit bootstrap + log/history dirs
    │   ├── history.zsh                 # History size, deduplication, sharing
    │   ├── plugins.zsh                 # Zinit plugins + Powerlevel10k
    │   ├── key-bindings.zsh            # All custom key bindings
    │   └── config.d/
    │       ├── env/                    # Generated .env.sh (credentials)
    │       ├── gitconf/                # Generated .gitconfig
    │       └── themes/
    │           ├── shell/              # Powerlevel10k theme configs (pure, lean, etc.)
    │           └── ls/                 # lsd colour themes
    │
    └── .shell.d/                       # Shared shell modules (zsh + bash)
        ├── environment.sh              # PATH additions + exports (pyenv, go, emacs, etc.)
        ├── aliases.sh                  # 100+ aliases (editors, git, pacman, ufw, clamav...)
        ├── functions.sh                # Function loader
        ├── templates.sh                # Interactive template menu
        ├── shell-detect.sh             # Detect current shell
        ├── local.sh                    # Machine-local overrides (gitignored)
        │
        ├── functions.d/
        │   ├── archive-extraction.sh   # extract <file> — all formats
        │   ├── change-dir-up.sh        # up <n> — go up N directories
        │   ├── find-file.sh            # find-file <name> — fd + fzf → nvim
        │   ├── find-dir.sh             # find-dir <name> — fd + fzf → nvim
        │   ├── ranger-cd.sh            # ranger with automatic cd on exit
        │   ├── editor-nvim-vim.sh      # nvim → vim fallback
        │   ├── editor-lvim-nvim-vim.sh # lvim → nvim → vim fallback
        │   └── git-logauthor.sh        # logauthor <author> — filtered git log
        │
        ├── templates.d/                # File templates
        │   ├── git/.gitignore          # Universal .gitignore template
        │   ├── docker/                 # Dockerfile templates
        │   ├── docker-compose/         # docker-compose.yml templates
        │   ├── kubernetes/             # K8s manifest templates
        │   └── vagrant/                # Vagrantfile templates
        │
        └── config.d/                   # Application configurations
            ├── arch/pacman.conf        # Pacman configuration
            ├── kitty/                  # Kitty terminal config + themes
            ├── tmux/                   # Tmux config
            ├── ranger/                 # Ranger file manager config
            ├── vim/.vimrc              # Vim configuration
            ├── doom/                   # Doom Emacs config (init.el, config.el, packages.el)
            ├── doom-back/              # Doom Emacs backup config
            ├── clamav/                 # ClamAV clamd.conf + freshclam.conf
            ├── cron/
            │   ├── cron.daily/         # Daily automated virus scans
            │   └── cron.weekly/        # Weekly full system scans
            ├── ufw/before.rules        # UFW custom firewall rules
            └── themes/                 # lsd colour theme files
```

---

## 📦 What Gets Installed

### 🐧 Linux Kernel
| Package | Description |
|---------|-------------|
| `linux-zen` | Performance and latency optimised kernel |
| `linux-zen-headers` | Kernel headers required for module compilation |

### 🔤 Fonts (14 font families)
| Fonts |
|-------|
| DejaVu · Liberation · Roboto · Ubuntu · Noto (+ Emoji + CJK) |
| Fira Code · Fira Mono · Fira Sans · JetBrains Mono |
| Hack · Inconsolata · **Nerd Fonts** · OpenSans · Terminus |

### 🐚 Shell & Terminal
`kitty` · `zsh` · `git` · `github-cli` · `ranger`

### 🛠️ Base Development Tools
`base-devel` · `make` · `gcc` · `clang` · `cmake`

### ✏️ Editors & IDE Support
`vim` · `neovim` · `emacs` + `emacs-apel` · `emacs-haskell-mode` · `emacs-lua-mode` · `emacs-python-mode` · `emacs-slime`

### 🔀 Version Control
`lazygit` · `git-delta` · `kdiff3`

### 🖥️ System Utilities
`tmux` · `fzf` · `fd` · `bat` · `btop` · `htop` · `lsd` · `ripgrep` · `ncdu` · `tldr` · `glances` · `trash-cli` · `neofetch` · `fastfetch` · `onefetch` · `vivid` · `httpie` · `curl` · `reflector` · `stow` · `pass` · `haveged` · `wl-clipboard`

### 🔐 Security
`ufw` · `clamav` · `gnupg` · `openssh`

### 📧 Mail & Communication
`isync` · `offlineimap` · `msmtp` · `mu` · `w3m` · `gnupg`

### 💾 File & Data Management
`sqlite` · `jq` · `direnv`

### ✅ Linting & Formatting
`shfmt` · `shellcheck` · `tidy` · `stylelint`

### 📚 Documentation
`pandoc` · `languagetool` · `hunspell` · `hunspell-en_us`

### 🌐 Programming Languages
| Language | Packages |
|----------|----------|
| **Python** | `python` · `pip` · `pipenv` · `virtualenv` · `pynvim` · `pyenv` · `black` · `pyflakes` · `isort` · `pytest` |
| **JavaScript** | `nodejs` · `npm` · `yarn` |
| **Go** | `go` · `gopls` · `gomodifytags` · `gotests` · `gore` |
| **Rust** | `rust` · `cargo` |
| **Java** | `jdk11-openjdk` · `jdk17-openjdk` · `jdk21-openjdk` · `maven` · `gradle` · `spring` |
| **JVM** | `kotlin` · `clojure` · `leiningen` · `scala` · `scalafmt` · `metals` |
| **Haskell** | `cabal-install` · `haskell-language-server` |
| **Lisp** | `emacs-slime` · `lisp` |
| **Ruby** | `ruby` |
| **Lua** | `luarocks` |
| **PHP** | `php` · `composer` · `lighttpd` |

### 🖼️ GUI Applications
| Category | Applications |
|----------|-------------|
| **Browsers & Mail** | Firefox · Thunderbird |
| **Office & Productivity** | LibreOffice · FileZilla |
| **Media & Design** | GIMP · OBS Studio · Kdenlive · VLC |
| **System Tools** | GParted · DBeaver · system-config-printer |
| **Entertainment** | Steam · Discord |
| **Dev Tools (AUR)** | Visual Studio Code · IntelliJ IDEA · Postman · Stacer |
| **Communication (AUR)** | Viber |
| **System Widget** | eww (Wayland/X11 widget system) |

### 🎬 Multimedia & Graphics
`graphviz` · `gnuplot` · `maim` · `scrot` · `plantuml` · `transmission` · `wine`

### 🖥️ Virtualisation
`qemu` · `virt-manager` · `virt-viewer` · `libvirt` · `dnsmasq` · `bridge-utils` · `virtualbox` · `virtualbox-host-modules-arch`

### 🔧 System Optimisation (AUR)
`auto-cpufreq` · `backintime` · `downgrade`

---

## ⚙️ Installation

> ⚠️ **WARNING:** This script installs 100+ GB of software and modifies system-level files.
> **Always test on a virtual machine or a fresh Arch Linux installation first.**

### Prerequisites

- Fresh Arch Linux installation
- Internet connection
- `git` installed: `sudo pacman -S git`
- `zsh` installed: `sudo pacman -S zsh`

### Step 1 — Clone the repository

```sh
git clone https://github.com/bdostumski/arch-frame.git
cd arch-frame
```

### Step 2 — Configure your personal details

Edit `install-config.sh` and fill in your real values:

```sh
USER_NAME="your-username"          # Your Linux username
FIRST_NAME="Your"                  # Your first name
LAST_NAME="Name"                   # Your last name
GIT_USER="your-github-username"    # Your GitHub username
GMAIL_EMAIL="your@gmail.com"       # Your Gmail address
GMAIL_USER="your-gmail-user"       # Your Gmail username
GMAIL_PASSWORD="gpg-encrypted"     # Your GPG-encrypted Gmail password
DEFAULT_SHELL="zsh"                # Default shell: zsh | bash | fish
```

> 🔐 **Gmail Password** should be GPG-encrypted. The framework stores it base64+GPG encrypted and decrypts it at runtime.

The installer **validates all placeholders** before running — it will refuse to proceed if default values like `"johndoe"` or `"john.doe@gmail.com"` are still in place.

### Step 3 — Run the installer

```sh
chmod +x install.sh
./install.sh
```

You will see an **interactive menu**:

```
INSTALL DEPENDENCIES:
1) Main Packages Installation [Neovim, Emacs, System, ClamAV, UFW, etc]
2) Dev Packages Installation [Docker, Vagrant, K8s, etc]
3) System Drivers & Firmware Installation
x) Exit
```

- **Option 1** — Full workstation setup (recommended first run)
- **Option 2** — DevOps tools: Docker, Vagrant, Kubernetes
- **Option 3** — Hardware drivers and firmware

### Step 4 — Restart in Kitty terminal

After Option 1 completes, **restart your terminal using Kitty** and re-run for Option 2 if needed:

```sh
exec zsh
```

---

## 🎨 Shell Configuration

### Powerlevel10k — Pure Theme

The terminal uses **Powerlevel10k** with the **Pure** style — a clean, minimalist prompt inspired by [sindresorhus/pure](https://github.com/sindresorhus/pure).

```
~/projects/arch-frame main*:⇡                              5s user@host 14:32:01
❯
```

| Prompt Element | Position | Description |
|----------------|----------|-------------|
| `dir` | Left | Current directory in **blue** |
| `vcs` | Left | Git branch + status in **grey** · `*` dirty · `⇡` ahead · `⇣` behind |
| `prompt_char` | Left line 2 | `❯` **magenta** = success · **red** = error |
| `command_execution_time` | Right | Duration shown only if ≥ 5s, in **yellow** |
| `virtualenv` | Right | Python venv name in **grey** |
| `context` | Right | `user@host` — only shown when root or SSH |
| `time` | Right | Current time `HH:MM:SS` in **blue** |

**Features:**
- ⚡ **Instant prompt** — shell loads immediately, git status updates async
- 🎯 **Transient prompt** — previous prompts are trimmed to save screen space
- 🔔 No automatic `git fetch` — keeps prompt fast

To reconfigure:
```sh
zsh-theme    # runs: p10k configure
```

### Zinit Plugin Manager

Plugins are managed with **Zinit** and loaded in the correct order:

| Plugin | Purpose |
|--------|---------|
| `OMZ::plugins/z` | Jump to frecent directories |
| `zsh-colored-man-pages` | Coloured `man` pages |
| `zsh-autosuggestions` | Fish-like command suggestions |
| `fzf-tab` | Replace zsh completions with fzf |
| `powerlevel10k` | Prompt theme |
| `zsh-completions` | Extra completion definitions |
| `zsh-syntax-highlighting` | Real-time syntax highlighting (loaded **last**) |

### History Configuration

```
HISTSIZE=50000   # Lines in memory
SAVEHIST=50000   # Lines on disk
```

Options: `appendhistory` · `sharehistory` · `inc_append_history` · `hist_ignore_dups` · `hist_ignore_space` · `hist_verify` · `hist_reduce_blanks`

---

## 🔧 Configuration

### 📁 Config Directory (`config.d/`)

All application configurations live in `~/.shell.d/config.d/` and are managed by the framework:

| Directory | Contents |
|-----------|----------|
| `arch/` | `pacman.conf` — custom Arch Linux package manager config |
| `kitty/` | Kitty terminal config, colour themes, fonts |
| `tmux/` | Tmux prefix, panes, status bar, plugins |
| `ranger/` | Ranger file manager settings, key bindings, colour schemes |
| `vim/` | `.vimrc` with syntax highlighting, indentation, plugins |
| `doom/` | Doom Emacs `init.el`, `config.el`, `packages.el` |
| `doom-back/` | Backup of previous Doom Emacs configuration |
| `clamav/` | `clamd.conf` + `freshclam.conf` — antivirus configuration |
| `cron/` | `cron.daily/` and `cron.weekly/` automated scan scripts |
| `ufw/` | `before.rules` — custom UFW packet filter rules |
| `env/` | Generated `.env.sh` with GPG-encrypted credentials |
| `gitconf/` | Generated `.gitconfig` with your user details + 100+ aliases |
| `themes/` | Powerlevel10k themes · lsd colour themes |

### 🧩 Template System

The `templates` command opens an interactive menu to scaffold project files:

```sh
templates
```

```
= = = = = = = = =
T e m p l a t e s
= = = = = = = = =

1. Git
2. Docker
3. Docker Compose
4. Kubernetes
5. Vagrant
0. Exit
```

Selecting a category generates the appropriate template file directly into your current directory.

---

## 🛡️ Security

### 🔥 UFW Firewall

UFW is configured automatically with the following rules:

| Rule | Value | Description |
|------|-------|-------------|
| Default incoming | `DENY` | Block all unsolicited inbound traffic |
| Default outgoing | `ALLOW` | Allow all outbound traffic |
| HTTP (port 80) | `ALLOW` | Web browsing |
| HTTPS (port 443) | `ALLOW` | Secure web browsing |
| SSH (port 22/tcp) | `LIMIT` | Rate-limited SSH (brute-force protection) |
| VNC (port 5900) | `DENY` | Block remote desktop |
| Logging | `HIGH` | Full UFW event logging |

Custom rules from `ufw/before.rules` are applied on top, allowing packet-level filtering.

**UFW Aliases:**
```sh
ufw-on                  # Enable the firewall
ufw-off                 # Disable the firewall
ufw-status              # Show all rules (verbose)
ufw-list                # Show numbered rules
ufw-allow <port>        # Allow a port or service
ufw-deny <port>         # Deny a port or service
ufw-delete <rule>       # Delete a rule by number
ufw-reload              # Reload all rules
ufw-reset               # Reset all rules to default
ufw-logs                # Follow UFW logs (journalctl)
ufw-deny-out-to <ip>    # Block outgoing traffic to an IP

# Email ports: 25, 465, 587, 143, 993, 110, 995
ufw-mail-allow          # Allow all mail ports
ufw-mail-deny           # Deny all mail ports
ufw-mail-status         # Check mail port status
```

---

### 🦠 ClamAV Antivirus

ClamAV is installed and fully configured with **three layers of protection**:

| Service | Description |
|---------|-------------|
| `clamav-freshclam` | Automatic virus database updates |
| `clamav-daemon` | Background scanning daemon (clamd) |
| `clamav-clamonacc` | **Real-time on-access scanning** — files are scanned as they are accessed |

The installer handles all of the complex setup:
- Creates `clamav` system user and correct group permissions
- Sets up `/var/lib/clamav`, `/var/log/clamav`, quarantine directories
- Downloads the virus database (waits up to 5 minutes on first install)
- Writes a custom `clamonacc.service` systemd unit
- Enables desktop notifications via sudoers
- Enables all three services at boot

**Automated scanning** runs via:
- `cron.daily` — daily home directory scan
- `cron.weekly` — weekly full system scan

**ClamAV Aliases:**
```sh
clamscan-home                  # Scan home directory
clamscan-full                  # Full system scan (excludes /sys /proc /dev)
clamscan-home-quarantine       # Scan home + move threats to quarantine
clamscan-root-quarantine       # Full scan + quarantine
clamscan-update                # Update virus definitions (freshclam)
clamscan-logs                  # Follow ClamAV daemon logs
```

---

### 🔑 GPG & SSH Encryption

```sh
# SSH
ssh-ed <comment>        # Generate new ed25519 SSH key
ssh-copy <host>         # Copy public key to remote host
ssh-add                 # Add key to SSH agent
ssh-agent               # Start SSH agent
ssh-kill                # Kill SSH agent

# GPG
gpg-encrypt <file>      # Encrypt a file
gpg-decrypt <file>      # Decrypt a file
gpg-sign <file>         # Sign a file
gpg-verify <file>       # Verify a signature
gpg-list                # List public keys
gpg-list-secret         # List secret keys
gpg-import <file>       # Import a key
gpg-export <keyid>      # Export a key
gpg-delete <keyid>      # Delete a key
```

---

## 🧠 Doom Emacs

Doom Emacs is installed and pre-configured as a **full-featured IDE and personal productivity system**.

### Installation

The framework clones Doom Emacs and runs `doom install` automatically:

```sh
git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### Configured Modules

| Module | Description |
|--------|-------------|
| 📝 **org-mode** | Note-taking, task management, agendas, literate programming |
| 📧 **mu4e** | Full email client — read, write, search email without leaving Emacs |
| 🔀 **Magit** | The most powerful Git interface ever built |
| 🔍 **LSP (eglot/lsp-mode)** | Language Server Protocol for all major languages |
| 🐍 **Python** | `python-mode`, `pyflakes`, `black`, `isort`, `pytest` integration |
| ☕ **Java/Kotlin** | Full JVM language support with `metals`/`LSP` |
| ⚡ **Scala** | `metals` language server + `scalafmt` |
| 🌐 **Web** | HTML, CSS, JavaScript, PHP support |
| 🦀 **Rust** | `rust-analyzer` LSP integration |
| 🐹 **Go** | `gopls` + `gomodifytags` + `gotests` |
| 🎯 **Haskell** | `haskell-language-server` |
| λ **Lisp/Clojure** | SLIME + CIDER integration |
| 🎨 **UI** | Nerd Fonts icons, modeline, dashboard, doom-themes |
| 📋 **Org-roam** | Zettelkasten-style knowledge management |

### Email with mu4e

The framework configures a complete email workflow:

| Component | Role |
|-----------|------|
| `isync` / `mbsync` | Sync Gmail IMAP to local Maildir |
| `msmtp` | Send email via Gmail SMTP |
| `mu` / `mu4e` | Index and search email in Emacs |
| `gnupg` | Sign and encrypt emails |

Gmail credentials are stored **GPG-encrypted** in `~/.env.sh` and decrypted at runtime — never stored in plaintext.

### Emacs Aliases

```sh
emacs           # Open Emacs GUI client (emacsclient -c)
emacs.          # Open Emacs in terminal mode (emacsclient -c -a 'emacs -nw')
emacs-kill      # Kill Emacs server and restart daemon
```

---

## 🔀 Git Configuration

The framework auto-generates a complete `.gitconfig` with your personal details and the following configuration:

### Core Settings
| Setting | Value |
|---------|-------|
| Default branch | `main` |
| Editor | `vim` |
| Pager | `bat` (syntax highlighted) |
| Diff tool | `kitty` (Kitty terminal diff viewer) |
| Merge tool | `vimdiff` |
| Browser | `firefox` |

### Delta Diff Viewer

Git diffs are displayed using **delta** with:
- Line numbers
- Coloured additions/deletions (green/red bold)
- Magenta hunk headers with box decorations
- Yellow file names with box decorations

### Git Aliases (100+)

A sample of the most useful:

```sh
# Quick shortcuts
g a          # git add --all
g cm "msg"   # git commit -m
g s          # git status
g sb         # git status -s -b
g l          # git log --oneline
g lg         # git log --oneline --graph --decorate

# Branching
g b          # branch
g ob <name>  # checkout -b (create new branch)
g bc         # show current branch name
g bd <name>  # delete branch

# Push / Pull
g psuoc      # push -u origin <current-branch>
g psoc       # push origin <current-branch>
g ploc       # pull origin <current-branch>
g pboc       # pull --rebase origin <current-branch>

# Stash
g ss "msg"   # stash save
g sp         # stash pop
g sl         # stash list

# Useful utilities
g whois "name"    # Find author details
g aliases         # List all git aliases
g snap            # Snapshot stash (non-destructive)
g ahead           # Commits ahead of upstream
g behind          # Commits behind upstream
g assume <file>   # Ignore local changes to a tracked file
g ours <file>     # Resolve conflict with our version
g theirs <file>   # Resolve conflict with their version
```

---

## ⌨️ Key Bindings

### Shell Navigation
| Key | Action |
|-----|--------|
| `Alt + h` | Move backward one character |
| `Alt + l` | Move forward one character |
| `Alt + u` | Move backward one word |
| `Alt + p` | Move forward one word |
| `Alt + n` | Move to beginning of line |
| `Alt + m` | Move to end of line |

### Editing
| Key | Action |
|-----|--------|
| `Ctrl + Backspace` | Delete word before cursor |
| `Ctrl + Delete` | Delete word after cursor |
| `Alt + j` | Delete character before cursor |
| `Alt + k` | Delete character under cursor |
| `Alt + i` | Delete word before cursor |
| `Alt + o` | Delete word after cursor |

### Shell Features
| Key | Action |
|-----|--------|
| `Ctrl + F` | Accept autosuggestion |
| `Ctrl + D` | Accept and execute autosuggestion |
| `Ctrl + R` | FZF fuzzy history search |
| `Ctrl + E` | FZF file search |
| `Alt + [` | Search history backward |
| `Alt + ]` | Search history forward |

### GitHub Copilot (optional)
| Key | Action |
|-----|--------|
| `Alt + \` | Copilot suggest |
| `Alt + Shift + \` | Copilot explain |

---

## 📝 Aliases

### ✏️ Editors
```sh
v               # /bin/vim (raw, no config)
vi / vim        # nvim → vim fallback
emacs           # emacsclient GUI
emacs.          # emacsclient terminal (no window)
emacs-kill      # Kill + restart Emacs daemon
```

### 📁 Navigation
```sh
c.              # cd ..
c..             # cd ../..
c...            # cd ../../..
up <n>          # Go up N directories (e.g. up 3)
c <pattern>     # z fuzzy jump to directory
rr              # Ranger with automatic cd on exit
```

### 📋 File Listing (lsd)
```sh
ls              # lsd -al (full list, hidden files, colours)
ll              # lsd -l  (long format)
lt              # lsd -a --tree (tree view)
la              # lsd -a  (all files)
l. / l.. / l... # List parent directories
```

### 🔍 Search
```sh
find-text <pat>     # ripgrep recursive search
find-file <name>    # fd + fzf → open result in nvim
find-dir  <name>    # fd + fzf → open result in nvim
```

### 📦 Archive Extraction
```sh
extract <file>      # Auto-detect format and extract
                    # Supports: tar, gz, bz2, xz, zip, rar,
                    #           7z, deb, iso, exe, cpio, ace...
```

### 🔀 Git
```sh
g                   # git
git.                # lazygit TUI
gitinfo             # onefetch repo overview
diff                # git diff --color-words
difftool            # git difftool -y
mergetool           # git mergetool -y
log                 # git log --oneline --graph --decorate | bat
logall              # git log --graph --decorate | bat
logfull             # git log --full-history --graph | bat
logauthor <name>    # git log filtered by author | bat
```

### 🐙 GitHub CLI
```sh
copilot             # gh copilot
pull-list           # gh pr list
pull-view           # gh pr view
issue               # gh issue create -f
issue-list          # gh issue list
issue-search <q>    # gh issue list --search
issue-view <n>      # gh issue view
issue-comment <n>   # gh issue comment
issue-assign <n>    # gh issue assign
issue-close <n>     # gh issue close
```

### 📦 Package Management
```sh
install <pkg>       # sudo pacman -S
remove <pkg>        # sudo pacman -R
search <pkg>        # pacman -Ss
update              # sudo pacman -Syu
clean               # sudo pacman -Sc (clean cache)
autoremove          # Remove orphaned packages (safe)

# Mirror optimisation
mirror              # Best 10 mirrors by speed
mirrord             # Sort by delay
mirrors             # Sort by score
mirrora             # Sort by age
```

### 🖥️ Kitty Terminal
```sh
kitty-theme         # Change Kitty colour theme
kitty-fonts         # Change Kitty font
kitty-ssh           # SSH via Kitty
compare <f1> <f2>   # Side-by-side diff in Kitty
image <file>        # Display image inline in terminal
unicode             # Browse Unicode characters
ssh-download        # Download file via Kitty transfer
ssh-upload          # Upload file via Kitty transfer
ssh-rsync-download  # Rsync download (incremental)
ssh-rsync-upload    # Rsync upload (incremental)
```

### 🖥️ System
```sh
top                 # btop (beautiful system monitor)
sysremote           # glances (HTTP API system monitor)
sysinfo             # neofetch
cpu                 # lscpu (CPU info)
du                  # ncdu (visual disk usage)
df                  # pydf -h (disk free, colourised)
journal             # journalctl -xe (recent errors)
journalf            # journalctl -f (follow live)
kernel-errors       # dmesg errors/warnings
shutdown            # sudo shutdown -h now
reboot              # sudo reboot
sleep               # systemctl suspend
grub-install        # Install GRUB (EFI, x86_64)
grub-update         # Regenerate grub.cfg
```

### 🔧 Systemctl
```sh
sys-start   <svc>   # start immediately
sys-stop    <svc>   # stop
sys-restart <svc>   # restart
sys-enable  <svc>   # enable at boot
sys-disable <svc>   # disable at boot
sys-status  <svc>   # current status
```

### 🌐 Network
```sh
ping <host>         # ping -c 5 (5 packets)
httpie              # http --print=HhBb (headers + body)
network-speed       # speedtest-cli
```

### 📊 Processes
```sh
psa                 # ps auxf (all processes, tree)
psmem               # ps sorted by memory usage
pscpu               # ps sorted by CPU usage
psgrep <name>       # grep running processes
```

---

## 🎓 Origin

This project was created by **Borislav Aleksandrov Dostumski** as a practical application of skills learned during the **[Linux Shell Scripting course on Udemy](https://bdostumski.github.io/certificates/udemy-linux-shell-scripting.html)**.

What started as shell scripting practice — learning POSIX `sh`, scripting idioms, system automation and dotfile management — evolved into a complete, production-quality framework that demonstrates what is possible with solid fundamentals and deliberate practice.

> _"The best way to learn is to build something real."_

---

## 📄 License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

Free to use, modify and distribute. ❤️

---

<div align="center">

Made with ❤️ and lots of `zsh` by [bdostumski](https://github.com/bdostumski)

_"Let's try to master the chaos 🔥"_

</div>
