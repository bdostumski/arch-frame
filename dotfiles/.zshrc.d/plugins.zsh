#!/usr/bin/env zsh
#
# PLUGINS
# Description: Install and configure shell plugins
#

# -----------------
# LOAD PLUGINS
# -----------------
zinit snippet OMZ::plugins/z                  # z jump around directories with ease
zinit light ael-code/zsh-colored-man-pages    # colored man pages
zinit light zsh-users/zsh-autosuggestions     # autosuggestions - use ctrl+f to accept suggestion
zinit light Aloxaf/fzf-tab                    # fzf - fuzzy finder completion tab
zinit light zsh-users/zsh-syntax-highlighting # syntax highlighting
# zinit light zdharma-continuum/fast-syntax-highlighting
zinit light zsh-users/zsh-completions # completions
zinit ice depth=1
zinit light romkatv/powerlevel10k # powerlevel10k prompt theme

# Install github copilot
# gh auth login
# gh extension install github/gh-copilot

# zinit light loiccoyle/zsh-github-copilot # github copilot

# -----------------
# COMPLETIONS
# -----------------
autoload -Uz compinit
compinit -d "${HOME}/.zshrc.d/.logs/.zcompdump"
