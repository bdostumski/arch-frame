#!/usr/bin/env bash
#
# BASH PROFILE / THEMES
# Description: Bash prompt, colors, fzf and direnv integration
#

SHELLDIR="${HOME}/.bashrc.d"
THEMES="${SHELLDIR}/config.d/themes"  # shared themes dir (symlink or copy)

# LS colors
LS_COLOR_SCHEME="${THEMES}/ls/iceberg-dark"
[ -f "${LS_COLOR_SCHEME}" ] && export LS_COLORS="$(cat "${LS_COLOR_SCHEME}")"

# Prompt: use starship if available, else a clean PS1
if command -v starship > /dev/null 2>&1; then
    eval "$(starship init bash)"
else
    # Minimal colored PS1 fallback
    PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
fi

# fzf integration
if command -v fzf > /dev/null 2>&1; then
    eval "$(fzf --bash)"
fi

# direnv integration
if command -v direnv > /dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi

# Terraform autocomplete
if command -v terraform > /dev/null 2>&1; then
    complete -o nospace -C "$(which terraform)" terraform
fi
