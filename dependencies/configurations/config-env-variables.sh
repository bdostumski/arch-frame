#!/usr/bin/env sh

create_env_variables_file() {
    mkdir -p "${HOME}/.zshrc.d/config.d/env"
    cat <<EOF >"${HOME}/.zshrc.d/config.d/env/.env.sh"
#!/usr/bin/env sh
#
# ENVIRONMENT VARIABLES
# Description: Expose local environment variables
# Path: ~/.zsh.d/.env.sh
#

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
}
