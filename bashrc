if [ -n "$CONDA_DEFAULT_ENV" ]; then                                                                                                                                         
    TMUX_CONDA_DEFAULT_ENV=$CONDA_DEFAULT_ENV
fi

# Allow local customizations in the ~/.shell_local_before file
if [ -f ~/.shell_local_before ]; then
    source ~/.shell_local_before
fi

# Source local config
if [ -f ~/.bashrc_local_before ]; then
    source ~/.bashrc_local_before
fi

# Source local config before
if [ -f ~/.bashrc_local_before ]; then
    source ~/.bashrc_local_before
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
if [ -f ~/.shell_local_after ]; then
    source ~/.shell_local_after
fi

if [ -n "$TMUX_CONDA_DEFAULT_ENV" ]; then                                                                                                                                         
    conda activate $TMUX_CONDA_DEFAULT_ENV
fi

# Source local config
if [ -f ~/.bashrc_local_after ]; then
    source ~/.bashrc_local_after
fi
