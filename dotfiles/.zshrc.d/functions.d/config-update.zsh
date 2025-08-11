#!/usr/bin/env zsh
#
# Update Configuration Files
# Description: Override all applications coniguration files
#

# Check if it is user or not and copy directory
function copy_directory() {
    if [[ "${SUDO}" = 'true' ]]; then
        sudo cp -r "${COPY_FILE_DATA}" "${PASTE_FILE_PATH}"
    else
        cp -r "${COPY_FILE_DATA}" "${PASTE_FILE_PATH}"
    fi

    return 0
}

# Check if it is sudo or not and copy file
function copy_file() {
    if [[ "${SUDO}" = 'true' ]]; then
        sudo cp "${COPY_FILE_DATA}" "${PASTE_FILE_PATH}"
    else
        cp "${COPY_FILE_DATA}" "${PASTE_FILE_PATH}"
    fi

    return 0
}

# Backup target file and override it with new configuration
# Override target file if backup file exists
function config_backup() {

    local COPY_FILE_DATA="${1}"
    local PASTE_FILE_PATH="${2}"
    local SUDO="${3}"
    local FILE_NAME="$(echo "${COPY_FILE_DATA}" | awk -F '/' '{print $NF}')"

    if [[ -s "${COPY_FILE_DATA}" && ! -e "${PASTE_FILE_PATH}${FILE_NAME}.bak" ]]; then
        echo "Backup ${FILE_NAME} into ${PASTE_FILE_PATH} path." >&2
        cp -r "${COPY_FILE_DATA}" "${PASTE_FILE_PATH}${FILE_NAME}.bak"
    fi

    if [[ -d "${COPY_FILE_DATA}" && ! -z "${PASTE_FILE_PATH}" ]]; then
        echo "Copy directory ${FILE_NAME} into ${PASTE_FILE_PATH}" >&2
        copy_directory
    elif [[ -f "${COPY_FILE_DATA}" && ! -z "${PASTE_FILE_PATH}" ]]; then
        echo "Copy file ${FILE_NAME} into ${PASTE_FILE_PATH}" >&2
        copy_file
    elif [[ ! -s "$COPY_FILE_DATA" && ! -z "${PASTE_FILE_PATH}" ]]; then
        echo "Error: ${FILE_NAME} does not exists, or file path ${PASTE_FILE_PATH} is not correct." >&2
        return 1
    else
        return 0
    fi

}

# Main function
function config_update() {

    local LOCATION="$HOME/.zshrc.d/config.d"

    # Copy user-specific configs
    config_backup "${LOCATION}/gitconf/.gitconfig" "${HOME}/"
    config_backup "${LOCATION}/vim/.vimrc" "${HOME}/" false
    #config_backup "${LOCATION}/nvim" "${HOME}/.config/" false
    config_backup "${LOCATION}/tmux" "${HOME}/.config/" false
    config_backup "${LOCATION}/ranger" "$HOME/.config/" false
    config_backup "${LOCATION}/env/.env.zsh" "${HOME}/" false
    config_backup "${LOCATION}/kitty" "${HOME}/.config/" false
    config_backup "${LOCATION}/doom" "${HOME}/.config/" false

    # Copy system-wide configs with sudo
    config_backup "${LOCATION}/clamav" "/etc/" true
    config_backup "${LOCATION}/cron/cron.daily" "/etc/" true
    config_backup "${LOCATION}/cron/cron.weekly" "/etc/" true
    config_backup "${LOCATION}/arch/pacman.conf" "/etc/" true
    config_backup "${LOCATION}/ufw/before.rules" "/etc/ufw/" true

    return 0
}

# Call function
config_update
