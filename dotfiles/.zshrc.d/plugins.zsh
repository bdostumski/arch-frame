#
# PLUGINS
# Description: Install and configure shell plugins
#

# -----------------
# LOAD THE PLUGIN MANAGER
# -----------------
zinit snippet OMZ::plugins/z # z jump around directories with ease
zinit light ael-code/zsh-colored-man-pages # colored man pages
zinit light zsh-users/zsh-autosuggestions # autosuggestions - use ctrl+f key to accept suggestion
zinit light Aloxaf/fzf-tab # fzf - fuzzy finder completion tab
zinit light zsh-users/zsh-syntax-highlighting # syntax highlighting - colorize commands and options
# zinit light zdharma-continuum/fast-syntax-highlighting # fast syntax highlighting
zinit light zsh-users/zsh-completions # completions - use tabkey to complete commands and options
zinit ice depth=1; zinit light romkatv/powerlevel10k # powerlevel10k - prompt theme

# Install github copilot
# gh auth login
# gh extension install github/gh-copilot

# Add a comment to stop github copilot for terminal
# zinit light loiccoyle/zsh-github-copilot # github copilot

# -----------------
# COMPLETIONS - USE TABKEY TO COMPLETE COMMANDS AND OPTIONS
# -----------------
autoload -Uz compinit # load the compinit module
zinit ice wait lucid # wait for the completions to load
rm -f ~/.zcompdump # remove the old completion dump file
compinit -d "$HOME/.logs.d/.zcompdump" # create a new completion dump file
