# Allow local customizations in the ~/.shell_local_before file
if [ -f ~/.shell_local_before ]; then
    source ~/.shell_local_before
fi

# Allow local customizations in the ~/.zshrc_local_before file
if [ -f ~/.zshrc_local_before ]; then
    source ~/.zshrc_local_before
fi
# 
# # External plugins (initialized before)
source ~/.zsh/plugins_before.zsh

# Settings
source ~/.zsh/settings.zsh
# 
# External settings
source ~/.shell/external.sh

# Aliases
source ~/.shell/aliases.sh

# Prompt
source ~/.zsh/prompt.zsh

# External plugins (initialized after)
source ~/.zsh/plugins_after.zsh

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

# Allow local customizations in the ~/.zshrc_local_after file
if [ -f ~/.zshrc_local_after ]; then
    source ~/.zshrc_local_after
fi
