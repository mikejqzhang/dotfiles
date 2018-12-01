# TESTING

# Nicer history
HISTSIZE=1048576
HISTFILE=$HOME/.zsh_history
setopt appendhistory
setopt incappendhistory
setopt extendedhistory

# Keybindings
bindkey -v
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode
bindkey '^P' up-history
bindkey '^N' down-history

# Movement
bindkey -a 'gg' beginning-of-buffer-or-history
bindkey -a 'G' end-of-buffer-or-history

# Use vim as the editor
export EDITOR=vim

# Use vim style line editing in zsh
bindkey -v
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
