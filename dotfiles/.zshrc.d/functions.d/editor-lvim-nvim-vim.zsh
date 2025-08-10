#!/usr/bin/env zsh
#
# EDITOR
# Description: Choose editor
# Example: if lvim exists then use lvim, else nvim, else vim
#

function editor_lvim_nvim_vim() {
    if command -v lvim >/dev/null 2>&1; then
        lvim "${@}"
    elif command -v nvim >/dev/null 2>&1; then
        nvim "${@}"
    elif command -v vim >/dev/null 2>&1; then
        vim "${@}"
    else
        echo "No suitable editor (lvim, nvim, or vim) found in PATH." >&2
        return 1
    fi

    return 0
}
