if [[ "$OSTYPE" == "darwin"* ]]; then
  eval `gdircolors ~/.shell/plugins/dircolors-solarized/dircolors.256dark`
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
  eval `dircolors ~/.shell/plugins/dircolors-solarized/dircolors.256dark`
else
  echo "Unknown OSTYPE: $OSTYPE"
fi

# pip should only run if there is a virtualenv currently activated
# Now just here to make sure pip doesn't by default work as to not install globally
# My package installations are done almost entirely in conda environments now so this
# is more or less now a hack
# export PIP_REQUIRE_VIRTUALENV=false
# alias pip_on='export PIP_REQUIRE_VIRTUALENV=false'
# alias pip_off='export PIP_REQUIRE_VIRTUALENV=true'

# Cache pip-installed packages to avoid re-downloading
# export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache

# TODO: Change pip stuff to be only ok in conda envs with:
# if [ "$CONDA_PREFIX" = "" ]; then
#   is bad
# fi
