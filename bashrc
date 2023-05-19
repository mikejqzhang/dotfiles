if [ -n "$CONDA_DEFAULT_ENV" ]; then                                                                                                                                         
    TMUX_CONDA_DEFAULT_ENV=$CONDA_DEFAULT_ENV
fi

# Allow local customizations in the ~/.shell_local_before file
if [ -f ~/.shell_local_before.sh ]; then
    source ~/.shell_local_before.sh
fi

# Source local config
if [ -f ~/.bashrc_local_before.sh ]; then
    source ~/.bashrc_local_before.sh
fi

export TERM=xterm-256color

# Settings
source ~/.bash/settings.bash

# External settings
source ~/.shell/external.sh

# Aliases
source ~/.shell/aliases.sh

# Custom prompt
source ~/.bash/prompt.bash


if [ -n "$CONDA_DEFAULT_ENV" ]; then                                                                                                                                         
    TMUX_CONDA_DEFAULT_ENV=$CONDA_DEFAULT_ENV
fi

# Allow local customizations in the ~/.shell_local_after file
if [ -f ~/.shell_local_after.sh ]; then
    source ~/.shell_local_after.sh
fi

if [ -n "$TMUX_CONDA_DEFAULT_ENV" ]; then                                                                                                                                         
    conda activate $TMUX_CONDA_DEFAULT_ENV
fi

# Source local config
if [ -f ~/.bashrc_local_after.sh ]; then
    source ~/.bashrc_local_after.sh
fi
