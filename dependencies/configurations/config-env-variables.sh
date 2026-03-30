#!/usr/bin/env sh

create_env_variables_file() {

    # Validate required variables
    if [ -z "${USER_NAME}" ] || [ -z "${GMAIL_EMAIL}" ] || [ -z "${GIT_USER}" ]; then
        printf '❌ Error: USER_NAME, GMAIL_EMAIL and GIT_USER are required.\n' >&2
        return 1
    fi

    mkdir -p "${HOME}/.zshrc.d/config.d/env"

    cat <<EOF >"${HOME}/.zshrc.d/config.d/env/.env.sh"
#!/usr/bin/env sh
#
# ENVIRONMENT VARIABLES
# Description: Expose local environment variables
# Path: ~/.zsh.d/.env.sh
#
# WARNING: This file contains sensitive credentials in plaintext.
# Permissions are set to 600. Do not commit this file to version control.
# Consider migrating to a secrets manager (e.g., pass, gpg) in the future.

# -----------------
# USER DATA
# -----------------
export USER_NAME="${USER_NAME}"
export FIRST_NAME="${FIRST_NAME}"
export MIDDLE_NAME="${MIDDLE_NAME}"
export LAST_NAME="${LAST_NAME}"

# -----------------
# GIT DATA
# -----------------
export GIT_USER="${GIT_USER}"

# -----------------
# GMAIL DATA
# -----------------
export GMAIL_EMAIL="${GMAIL_EMAIL}"
export GMAIL_USER="${GMAIL_USER}"
export GMAIL_PASSWORD="${GMAIL_PASSWORD}"

# -----------------
# DATABASE DATA
# -----------------
export DB_NAME="${DB_NAME}"
export DB_USERNAME="${DB_USERNAME}"
export DB_PASSWORD="${DB_PASSWORD}"

EOF

    chmod 600 "${HOME}/.zshrc.d/config.d/env/.env.sh"
    printf '✅ Environment variables file created.\n'

    return 0
}
