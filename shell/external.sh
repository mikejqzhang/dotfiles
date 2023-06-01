if [[ "$OSTYPE" == "darwin"* ]]; then
  eval `gdircolors ~/.shell/plugins/dircolors-solarized/dircolors.256dark`
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
  eval `dircolors ~/.shell/plugins/dircolors-solarized/dircolors.256dark`
else
  echo "Unknown OSTYPE: $OSTYPE"
fi

# Fix for TRAMP
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
