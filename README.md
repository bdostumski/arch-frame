<div align="center">

# 🏗️ arch-frame

### Automated Arch Linux Installation & Configuration Framework

[![Arch Linux](https://img.shields.io/badge/Arch_Linux-1793D1?style=for-the-badge&logo=arch-linux&logoColor=white)](https://archlinux.org)
[![Shell Script](https://img.shields.io/badge/Shell_Script-121011?style=for-the-badge&logo=gnu-bash&logoColor=white)](https://www.gnu.org/software/bash/)
[![Zsh](https://img.shields.io/badge/Zsh-F15A24?style=for-the-badge&logo=zsh&logoColor=white)](https://www.zsh.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)](LICENSE)

> **A professional-grade shell scripting framework that automatically installs and configures 100+ GB of software on Arch Linux — with a single command.**

[📖 About](#-about) • [🚀 Features](#-features) • [📦 What Gets Installed](#-what-gets-installed) • [🗂️ Project Structure](#️-project-structure) • [⚙️ Installation](#️-installation) • [🔧 Configuration](#-configuration) • [⌨️ Key Bindings](#️-key-bindings) • [📝 Aliases](#-aliases)

</div>

---

## 📖 About

This project was born from a passion for learning and automation. It started as a hands-on learning project while completing the **[Linux Shell Scripting course on Udemy](https://bdostumski.github.io/certificates/udemy-linux-shell-scripting.html)** — and evolved into a fully-fledged, production-quality framework.

**arch-frame** is a first-of-its-kind framework that:
- 🤖 **Fully automates** Arch Linux setup from a fresh install
- 📦 **Installs and configures 100+ GB** of curated software
- 🏢 **Enterprise-ready** — designed to help companies automate Linux workstation provisioning, software deployment and environment configuration for specific purposes
- 🔐 **Security-first** — includes antivirus, firewall, GPG encryption and credential management out of the box
- ⚡ **Shell-agnostic** — shared configuration works across both `zsh` and `bash`

---

## 🚀 Features

| Feature | Description |
|---------|-------------|
| 🤖 **One-command setup** | Full system from scratch with a single script |
| 🐚 **Dual shell support** | Shared config for both `zsh` and `bash` |
| 🎨 **Beautiful terminal** | Powerlevel10k Pure theme + Nerd Fonts + lsd + bat |
| 🔌 **Plugin management** | Zinit with autosuggestions, syntax highlighting, fzf-tab |
| 🛡️ **Security built-in** | ClamAV antivirus + UFW firewall + GPG encryption |
| 📧 **Email in terminal** | mu4e + isync + msmtp — full email workflow in Emacs |
| 🧠 **Doom Emacs** | Pre-configured with LSP, org-mode, mu4e, Magit and more |
| 🖥️ **Virtualisation** | QEMU/KVM + VirtualBox pre-configured |
| 🔧 **Dev environment** | Python, Node, Go, Rust, Java, Ruby, Clojure, Kotlin |
| 💾 **Safe dotfile backup** | All existing configs are backed up as `.bak` before replacing |

---

## 📦 What Gets Installed

### 🐧 Linux Kernel
| Package | Description |
|---------|-------------|
| `linux-zen` | Performance-optimised kernel |
| `linux-zen-headers` | Kernel headers for module compilation |

### 🔤 Fonts
DejaVu, Liberation, Roboto, Ubuntu, Noto (with emoji & CJK), Fira Code/Mono/Sans, JetBrains Mono, Hack, Inconsolata, **Nerd Fonts**, OpenSans, Terminus

### 🐚 Shell & Terminal
`kitty` · `zsh` · `git` · `github-cli` · `ranger`

### 🛠️ Development Tools
`base-devel` · `make` · `gcc` · `clang` · `cmake`

### ✏️ Editors
`vim` · `neovim` · `emacs` + Haskell/Lua/Python/SLIME modes

### 🔀 Version Control
`lazygit` · `git-delta` · `kdiff3`

### 🖥️ System Utilities
`tmux` · `fzf` · `fd` · `bat` · `btop` · `htop` · `lsd` · `ripgrep` · `ncdu` · `tldr` · `glances` · `trash-cli` · `neofetch` · `fastfetch` · `onefetch` · `vivid` · `httpie` · `curl` · `reflector` · `stow` · `pass` · `haveged`

### 🔐 Security
`ufw` · `clamav` · `gnupg` · `openssh`

### 📧 Mail & Communication
`isync` · `offlineimap` · `msmtp` · `mu` · `w3m` · `gnupg`

### 💾 Data & Linting
`sqlite` · `jq` · `direnv` · `shfmt` · `shellcheck` · `tidy` · `stylelint`

### 🌐 Programming Languages
`python` · `nodejs/npm/yarn` · `go` · `rust/cargo` · `ruby` · `kotlin` · `clojure` · `java (JDK 11/17/21)` · `maven` · `gradle`

### 🖼️ GUI Applications
`firefox` · `thunderbird` · `libreoffice` · `gimp` · `obs-studio` · `kdenlive` · `vlc` · `filezilla` · `dbeaver` · `gparted` · `steam` · `discord`

### 🖥️ Virtualisation
`qemu` · `virt-manager` · `virt-viewer` · `libvirt` · `virtualbox` · `dnsmasq` · `bridge-utils`

---

## 🗂️ Project Structure

```
arch-frame/
├── install.sh                          # 🚀 Main entry point
├── dependencies/
│   ├── packages/
│   │   ├── pkg-pacman.sh               # Pacman package list (100+ packages)
│   │   └── pkg-yay.sh                  # AUR package list
│   ├── configurations/
│   │   ├── config-env-variables.sh     # Generate ~/.env.sh with credentials
│   │   ├── config-gitconfig.sh         # Generate ~/.gitconfig
│   │   ├── config-ufw.sh               # UFW firewall rules
│   │   ├── config-clamav.sh            # ClamAV antivirus setup
│   │   └── config-vbox.sh              # VirtualBox drivers
│   └── utils/
│       └── install-utils.sh            # log(), backup_and_copy(), install_*()
└── dotfiles/
    ├── .zshrc                          # Zsh entry point
    ├── .bashrc                         # Bash entry point
    ├── .zshrc.d/                       # Zsh-specific modules
    │   ├── initialize.zsh              # Zinit bootstrap + log dirs
    │   ├── history.zsh                 # History options
    │   ├── plugins.zsh                 # Zinit plugins + Powerlevel10k
    │   ├── key-bindings.zsh            # Key bindings
    │   └── config.d/
    │       ├── themes/shell/           # Powerlevel10k theme configs
    │       └── themes/ls/              # lsd colour themes
    └── .shell.d/                       # Shared shell modules (zsh + bash)
        ├── environment.sh              # PATH + exports
        ├── aliases.sh                  # 100+ aliases
        ├── functions.sh                # Function loader
        ├── templates.sh                # File templates
        ├── functions.d/
        │   ├── archive-extraction.sh   # extract <file>
        │   ├── change-dir-up.sh        # up <n>
        │   ├── find-file.sh            # find-file <name>
        │   ├── find-dir.sh             # find-dir <name>
        │   ├── ranger-cd.sh            # ranger with cd on exit
        │   ├── editor-nvim-vim.sh      # nvim → vim fallback
        │   ├── editor-lvim-nvim-vim.sh # lvim → nvim → vim fallback
        │   └── git-logauthor.sh        # logauthor <author>
        └── config.d/
            ├── arch/                   # pacman.conf
            ├── kitty/                  # Kitty terminal config
            ├── tmux/                   # Tmux config
            ├── ranger/                 # Ranger file manager config
            ├── vim/                    # .vimrc
            ├── doom/                   # Doom Emacs config
            ├── clamav/                 # ClamAV config
            ├── cron/                   # Cron jobs (daily/weekly)
            ├── ufw/                    # UFW firewall rules
            └── themes/                 # lsd colour themes
```

---

## ⚙️ Installation

> ⚠️ **WARNING:** This script will modify system files, install 100+ GB of software and reconfigure your shell. **Test on a VM first.**

### Prerequisites
- Fresh Arch Linux installation
- Internet connection
- `git` installed (`sudo pacman -S git`)

### Steps

**1. Clone the repository**
```sh
git clone https://github.com/bdostumski/arch-frame.git
cd arch-frame
```

**2. Configure your credentials**

Edit the install script and fill in your personal details:
```sh
USER_NAME="your-username"
FIRST_NAME="Your"
LAST_NAME="Name"
GIT_USER="your-github-username"
GMAIL_EMAIL="your@gmail.com"
GMAIL_USER="your-gmail-user"
GMAIL_PASSWORD="your-gpg-encrypted-password"
```

**3. Run the installer**
```sh
chmod +x install.sh
./install.sh
```

**4. Restart your shell**
```sh
exec zsh
```

---

## 🔧 Configuration

### 🎨 Shell Theme — Powerlevel10k Pure

The terminal uses **Powerlevel10k** with the **Pure** style:

| Prompt Element | Description |
|----------------|-------------|
| Left: `dir` | Current directory in blue |
| Left: `vcs` | Git status in grey |
| Left: `prompt_char` | `❯` magenta (success) / red (error) |
| Right: `command_execution_time` | Duration if ≥ 5s, in yellow |
| Right: `context` | `user@host` — shown only when root or SSH |
| Right: `time` | Current time `HH:MM:SS` in blue |

To reconfigure the theme:
```sh
zsh-theme    # alias for: p10k configure
```

---

### 🧠 Doom Emacs

Doom Emacs is pre-configured as a full IDE with the following capabilities:

| Module | Description |
|--------|-------------|
| 📝 **org-mode** | Note-taking, task management, literate programming |
| 📧 **mu4e** | Full email client inside Emacs (Gmail via isync + msmtp) |
| 🔀 **Magit** | Best Git interface ever built |
| 🔍 **LSP** | Language Server Protocol for all major languages |
| 🐍 **Python** | python-mode, pyflakes, black, isort |
| ☕ **Java/Kotlin** | JVM language support |
| 🌐 **Web** | HTML/CSS/JS/PHP support |
| 🎨 **UI** | Nerd Fonts icons, modeline, dashboard |

**Useful Emacs aliases:**
```sh
emacs              # Open Emacs client (GUI)
emacs.             # Open Emacs in terminal (no window)
emacs-kill         # Kill and restart Emacs daemon
```

---

### 🛡️ Security — Antivirus & Firewall

#### ClamAV Antivirus

ClamAV is installed and configured with automated scanning via cron jobs:

```sh
clamscan-home             # Scan home directory
clamscan-full             # Full system scan
clamscan-home-quarantine  # Scan and quarantine threats in home
clamscan-root-quarantine  # Full scan and quarantine
clamscan-update           # Update virus definitions (freshclam)
clamscan-logs             # Follow ClamAV logs in real-time
```

Automated scans run via **cron.daily** and **cron.weekly** — no manual intervention required.

#### UFW Firewall

UFW is configured with sane defaults and email port rules:

```sh
ufw-on          # Enable firewall
ufw-off         # Disable firewall
ufw-status      # Show current rules (verbose)
ufw-list        # Show numbered rules
ufw-allow       # Allow a port/service
ufw-deny        # Deny a port/service
ufw-reload      # Reload rules
ufw-reset       # Reset all rules
ufw-logs        # View firewall logs

# Email ports (25, 465, 587, 143, 993, 110, 995)
ufw-mail-allow  # Allow all mail ports
ufw-mail-deny   # Deny all mail ports
ufw-mail-status # Check mail port status
```

---

## ⌨️ Key Bindings

### Navigation
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
| `Ctrl + R` | FZF history search |
| `Ctrl + E` | FZF file search |
| `Alt + [` | Search history backward |
| `Alt + ]` | Search history forward |
| `Alt + \` | GitHub Copilot suggest |
| `Alt + Shift + \` | GitHub Copilot explain |

---

## 📝 Aliases

### ✏️ Editors
```sh
v           # /bin/vim (raw vim, no config)
vi / vim    # nvim → vim fallback
emacs       # emacsclient GUI
emacs.      # emacsclient terminal
```

### 📁 Navigation
```sh
c.          # cd ..
c..         # cd ../..
c...        # cd ../../..
up 3        # Go up 3 directories
c <pattern> # z fuzzy jump to directory
rr          # Ranger (cd on exit)
```

### 📋 File Listing
```sh
ls    # lsd -al (full list, all files)
ll    # lsd -l  (long list)
lt    # lsd --tree (tree view)
la    # lsd -a  (all files)
l.    # list ../
l..   # list ../../
```

### 🔍 Search
```sh
find-text <pattern>   # ripgrep search in files
find-file <name>      # fd + fzf → open in nvim
find-dir  <name>      # fd + fzf → open in nvim
```

### 🔀 Git
```sh
g               # git
git.            # lazygit TUI
gitinfo         # onefetch repo info
log             # git log (oneline + graph + bat)
logall          # git log (full + bat)
logfull         # git log (full history + bat)
logauthor <name># git log filtered by author
diff            # git diff --color-words
difftool        # git difftool
mergetool       # git mergetool
```

### 🐙 GitHub CLI
```sh
copilot         # gh copilot
pull-list       # gh pr list
pull-view       # gh pr view
issue           # gh issue create
issue-list      # gh issue list
issue-search    # gh issue list --search
issue-close     # gh issue close
```

### 📦 Package Management
```sh
install <pkg>   # pacman -S
remove <pkg>    # pacman -R
search <pkg>    # pacman -Ss
update          # pacman -Syu
clean           # pacman -Sc (clean cache)
autoremove      # remove orphaned packages
```

### 🔒 SSH & GPG
```sh
ssh-ed          # Generate ed25519 SSH key
ssh-copy        # Copy SSH key to server
ssh-agent       # Start SSH agent
gpg-encrypt     # Encrypt a file
gpg-decrypt     # Decrypt a file
gpg-sign        # Sign a file
gpg-list        # List keys
```

### 🖥️ System
```sh
top             # btop  (system monitor)
sysinfo         # neofetch
sysremote       # glances (HTTP API monitor)
journal         # journalctl -xe
journalf        # journalctl -f (follow)
kernel-errors   # dmesg errors/warnings
shutdown        # sudo shutdown -h now
reboot          # sudo reboot
sleep           # systemctl suspend
```

### 🔧 Systemctl
```sh
sys-start   <service>   # start
sys-stop    <service>   # stop
sys-restart <service>   # restart
sys-enable  <service>   # enable at boot
sys-disable <service>   # disable at boot
sys-status  <service>   # status
```

### 🌐 Kitty Terminal
```sh
kitty-theme     # Change Kitty theme
kitty-fonts     # Change Kitty fonts
kitty-ssh       # SSH in Kitty
compare         # Kitty diff viewer
image <file>    # View image in terminal
ssh-download    # Download file via Kitty transfer
ssh-upload      # Upload file via Kitty transfer
```

---

## 🎓 Origin

This project was created as a practical application of skills learned during the **[Linux Shell Scripting course on Udemy](https://bdostumski.github.io/certificates/udemy-linux-shell-scripting.html)**.

What started as shell scripting practice evolved into a complete automation framework — demonstrating that solid fundamentals and hands-on practice can produce real, production-quality tools.

> _"The best way to learn is to build something real."_

---

## 📄 License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

Free to use, modify and distribute. ❤️

---

<div align="center">
Made with ❤️ and lots of <code>zsh</code> by <a href="https://github.com/bdostumski">bdostumski</a>
</div>
