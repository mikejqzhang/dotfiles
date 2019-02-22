# Initialize completion
autoload -Uz compinit && compinit -i
zstyle ':completion:*' menu select=4
zmodload zsh/complist

# Initialize editing command line (must be donne before bindkey stuff)
autoload -U edit-command-line && zle -N edit-command-line

# Set automatic cd (typing directory name with no 'cd')
setopt autocd

# Enable interactive comments (# on the command line)
setopt interactivecomments

# Nicer history
HISTSIZE=1048576
HISTFILE="$HOME/.zsh_history"
SAVEHIST=$HISTSIZE
setopt appendhistory
setopt extendedhistory
setopt incappendhistory
# setopt sharehistory

# Time to wait for additional characters in a sequence
KEYTIMEOUT=10 # corresponds to 100ms

# Use vim as the editor
export EDITOR=vim

# Use vim style line editing in zsh
bindkey -v
# Use vim style navigation keys in menu completion
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
# fight me
bindkey jk vi-cmd-mode
bindkey kj vi-cmd-mode
# emacs things in insert mode
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
# Movement
bindkey -a 'gg' beginning-of-buffer-or-history
bindkey -a 'G' end-of-buffer-or-history
# Undo
bindkey -a 'u' undo
bindkey -a '^R' redo
# Edit line
bindkey -a '^V' edit-command-line
# Backspace
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char

# Use incremental search
bindkey "^R" history-incremental-search-backward

# Disable shell builtins
disable r
