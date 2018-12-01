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

# Settings
source ~/.bash/settings.bash

# External settings
source ~/.shell/external.sh

# Aliases
source ~/.shell/aliases.sh

# Custom prompt
source ~/.bash/prompt.bash

# Allow local customizations in the ~/.shell_local_after file
if [ -f ~/.shell_local_after ]; then
    source ~/.shell_local_after
fi

# Source local config
if [ -f ~/.bashrc_local_after ]; then
    source ~/.bashrc_local_after
fi
