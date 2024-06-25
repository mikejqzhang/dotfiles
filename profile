# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# if ! [ -f /bin/zsh ]; then
#   exec /bin/zsh -l
# elif [ -n "$BASH_VERSION" ]; then
#     # Include .bashrc if it exists
#     if [ -f "$HOME/.bashrc" ]; then
# 	    . "$HOME/.bashrc"
#     fi
# fi
#

case $- in
  *i*)
    # Interactive session. Try switching to bash.
    if [ -z "$ZSH" ]; then # do nothing if running under bash already
      zsh=$(command -v zsh)
      if [ -x "$zsh" ]; then
        export SHELL="$zsh"
        exec "$zsh"
      fi
    fi

    if [ -n "$BASH_VERSION" ]; then
        # Include .bashrc if it exists
        if [ -f "$HOME/.bashrc" ]; then
          . "$HOME/.bashrc"
        fi
    fi

esac


