#!/usr/bin/env zsh

export backup_and_copy() {
    local src="$1"
    local dest="$2"
    local is_root="${3:-false}"

    if [[ $is_root == "true" ]]; then
        [[ -e "$dest" && ! -e "${dest}.bak" ]] && sudo mv "$dest" "${dest}.bak"
        sudo cp -r "$src" "$dest"
    else
        [[ -e "$dest" && ! -e "${dest}.bak" ]] && mv "$dest" "${dest}.bak"
        cp -r "$src" "$dest"
    fi
}
