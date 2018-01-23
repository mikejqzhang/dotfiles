# External settings
source ~/.shell/external.sh

# Aliases
source ~/.shell/aliases.sh

# Lines configured by zsh-newuser-install
HISTFILE=$HOME/.zsh_history
HISTSIZE=1048576
SAVEHIST=$HISTSIZE
setopt appendhistory autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/michael/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
