#!/usr/bin/env zsh
#
# ENVIRONMENT
# Description: Export configuration paths and environment configuration variables
#

# -----------------
# ENVIRONMENT CONFIGURATION
# -----------------
export TERM="xterm-256color"                           # Set terminal to 256 colors
export LANG='en_US.UTF-8'                              # Set language to US English
export LC_ALL='en_US.UTF-8'                            # Set locale to US English
export EDITOR="lvim"                                   # Set editor to emacsclient
export VISUAL="lvim"                                   # Set visual to emacsclient
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s" # Set lessopen to src-hilite-lesspipe
export LESS='-S -M -R -X --shift 5'                    # Set less options
export RANGER_LOAD_DEFAULT_RC=false                    # Load default ranger configuration

# -----------------
# AUTOSUGGESTION CONFIGURATION
# -----------------
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)                 # set autosuggest strategy (history, completion, match_prev_cmd)
export ZSH_AUTOSUGGEST_HISTORY_IGNORE="(ls|pwd|exit|sudo reboot)"    # ignore these commands in autosuggest history (ls|pwd|exit|sudo reboot|cd|cd -|cd ..)
export ZSH_AUTOSUGGEST_COMPLETION_IGNORE="(ls|pwd|exit|sudo reboot)" # ignore these commands in autosuggest completion (ls|pwd|exit|sudo reboot|cd|cd -|cd ..)
export ZSH_IGNORE_ALL_DUPS=1                                         # ignore all duplicates in history
# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline" # set autosuggest highlight style
# export ZSH_AUTOSUGGEST_ACCEPT_WIDGET="^F" # accept autosuggest widget
# export ZSH_AUTOSUGGEST_EXECUTE_WIDGET="^D" # execute autosuggest widget
# export ZSH_AUTOSUGGEST_CLEAR_WIDGET="^c" # clear autosuggest widget
# export ZSH_AUTOSUGGEST_IGNORE_WIDGET="^i" # ignore autosuggest widget

# -----------------
# ZINIT CONFIGURATION
# -----------------
export ZINIT[PLUGINS_DIR]="${HOME}/.config/zinit/share/zinit/plugins"
export ZINIT[SNIPPETS_DIR]="${HOME}/.config/zinit/share/zinit/snippets"
export ZINIT_HOME="${HOME}/.config/zinit"
export ZINIT[COMPINIT_OPTS]=-C
export ZINIT[ZCOMET_NO_UPDATE]=1
export ZINIT_DEFAULT_PROTOCOL="SSH"
export PYENV_ROOT="${HOME}/.pyenv"

# -----------------
# PATH CONFIGURATION
# -----------------
# add bin to path
if [[ -d "${HOME}/.bin" ]]; then
	PATH="${HOME}/.bin:${PATH}"
fi
# ----------
# add local bin to path
if [[ -d "${HOME}/.local/bin" ]]; then
	PATH="${HOME}/.local/bin:${PATH}"
fi
# ----------
# add emacs path
if [[ -d "${HOME}/.emacs.d/bin" ]]; then
	PATH="${HOME}/.emacs.d/bin:${PATH}"
fi
# ----------
# add emacs config path
if [[ -d "${HOME}/.config/emacs/bin" ]]; then
	PATH="${HOME}/.config/emacs/bin:${PATH}"
fi
# ----------
# add Applications to path
if [[ -d "${HOME}/Applications" ]]; then
	PATH="${HOME}/Applications:${PATH}"
fi
# ----------
# add doom emacs to path
if [[ -d "${HOME}/.config/emacs/bin/doom" ]]; then
	PATH="${HOME}/.config/emacs/bin/doom:${PATH}"
fi
# ----------
# add go lang to path
if [[ -d "${HOME}/go" ]]; then
	PATH="${HOME}/go/bin:${PATH}"
fi
# ----------
# add python to path
PATH="${PYENV_ROOT}/bin:${PATH}"
eval "$(pyenv init -)"
# ----------
# Export path
export PATH="/opt/cuda/bin:${PATH}"
export LD_LIBRARY_PATH="/opt/cuda/lib64:${LD_LIBRARY_PATH}"
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
export PATH=/opt/cuda/bin:$PATH 
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH 
