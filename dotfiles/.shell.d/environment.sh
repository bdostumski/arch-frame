#!/usr/bin/env sh
#
# ENVIRONMENT (SHARED)
# Description: Export environment variables and PATH — compatible with all shells
#

export TERM="xterm-256color"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export EDITOR="nvim"
export VISUAL="nvim"
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export LESS="-S -M -R -X --shift 5"
export RANGER_LOAD_DEFAULT_RC=false
export PYENV_ROOT="${HOME}/.pyenv"

# PATH additions (guarded)
[ -d "${HOME}/.bin"               ] && PATH="${HOME}/.bin:${PATH}"
[ -d "${HOME}/.local/bin"         ] && PATH="${HOME}/.local/bin:${PATH}"
[ -d "${HOME}/.emacs.d/bin"       ] && PATH="${HOME}/.emacs.d/bin:${PATH}"
[ -d "${HOME}/.config/emacs/bin"  ] && PATH="${HOME}/.config/emacs/bin:${PATH}"
[ -d "${HOME}/Applications"       ] && PATH="${HOME}/Applications:${PATH}"
[ -d "${HOME}/.config/emacs/bin/doom" ] && PATH="${HOME}/.config/emacs/bin/doom:${PATH}"
[ -d "${HOME}/go/bin"             ] && PATH="${HOME}/go/bin:${PATH}"
[ -d "${PYENV_ROOT}/bin"          ] && PATH="${PYENV_ROOT}/bin:${PATH}"

if [ -d "/opt/anaconda" ]; then
    export CRYPTOGRAPHY_OPENSSL_NO_LEGACY=1
    PATH="/opt/anaconda/bin:${PATH}"
fi

if [ -d "/opt/cuda" ]; then
    PATH="/opt/cuda/bin:${PATH}"
    export LD_LIBRARY_PATH="/opt/cuda/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"
fi

export PATH

# pyenv init (works in both bash and zsh)
if command -v pyenv > /dev/null 2>&1; then
    eval "$(pyenv init -)"
fi
