#!/usr/bin/env zsh

config_update() {

    # Copy user-specific configs
    cp ~/.zshrc.d/config.d/env/.env.zsh ~/
    cp ~/.zshrc.d/config.d/gitconf/.gitconfig ~/
    cp ~/.zshrc.d/config.d/vim/.vimrc ~/
    cp -r ~/.zshrc.d/config.d/nvim ~/.config/
    cp -r ~/.zshrc.d/config.d/tmux ~/.config/
    cp -r ~/.zshrc.d/config.d/ranger ~/.config/
    cp -r ~/.zshrc.d/config.d/kitty ~/.config/
    cp -r ~/.zshrc.d/config.d/doom ~/.config/

    # Copy system-wide configs with sudo
    sudo cp -r ~/.zshrc.d/config.d/ranger /etc/
    sudo cp -r ~/.zshrc.d/config.d/clamav /etc/
    sudo cp -r ~/.zshrc.d/config.d/cron/cron.daily /etc/
    sudo cp -r ~/.zshrc.d/config.d/cron/cron.weekly /etc/
    sudo cp ~/.zshrc.d/config.d/arch/pacman.conf /etc/
    sudo cp ~/.zshrc.d/config.d/ufw/before.rules /etc/ufw/

}
