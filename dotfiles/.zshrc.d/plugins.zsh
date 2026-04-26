#!/usr/bin/env zsh
#
# PLUGINS
# Description: Install and configure shell plugins
#

# -----------------
# LOAD PLUGINS
# -----------------
# Load completions first when using zsh-completions (recommended), then compinit.
# Also ensure ~/.zcompdump ends up in our logs directory.

zinit snippet OMZ::plugins/z                  # z jump around directories with ease
zinit light ael-code/zsh-colored-man-pages    # colored man pages
zinit light zsh-users/zsh-autosuggestions     # autosuggestions - use ctrl+f to accept suggestion
zinit light Aloxaf/fzf-tab                    # fzf - fuzzy finder completion tab

# NOTE: zsh-syntax-highlighting must be loaded LAST among plugins.
zinit light zsh-users/zsh-completions         # extra completions

zinit ice depth=1
zinit light romkatv/powerlevel10k             # powerlevel10k prompt theme

zinit light zsh-users/zsh-syntax-highlighting # syntax highlighting (LAST)

# -----------------
# COMPLETIONS
# -----------------
autoload -Uz compinit
# -C: skip insecure dir checks prompt, and rely on compaudit handling in init
# We keep dump/logs within ~/.zshrc.d/.logs
compinit -d "${HOME}/.zshrc.d/.logs/.zcompdump"

# -----------------
# GITHUB COPILOT (optional)
# -----------------
# Install github copilot
#   gh auth login
#   gh extension install github/gh-copilot
#
# The key bindings in key-bindings.zsh assume functions exist. Only enable
# the plugin if you actually install it.
# zinit light loiccoyle/zsh-github-copilot
