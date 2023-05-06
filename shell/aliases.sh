# ----------------
# OS dependent aliases
# ----------------

# Use colors in coreutils utilities output
if [[ "$OSTYPE" == "darwin"* ]]; then
  alias ls='ls -G'
else
  alias ls='ls --color=auto'
fi

# Open file in gui browser
if [[ "$OSTYPE" != "darwin"* ]]; then
  alias open='nautilus'
fi

# ----------------
# Personal aliases
# ----------------

alias grep='grep --color'

# ls aliases
alias ll='ls -lah'
alias la='ls -A'

# aliases to protect against overwriting
alias cp='cp -i'
alias mv='mv -i'

# clear screen
alias cl='clear'

# Go up [n] directories
up()
{
  local cdir="$(pwd)"
  if [[ "${1}" == "" ]]; then
    cdir="$(dirname "${cdir}")"
  elif ! [[ "${1}" =~ ^[0-9]+$ ]]; then
    echo "Error: argument must be a number"
  elif ! [[ "${1}" -gt "0" ]]; then
    echo "Error: argument must be positive"
  else
    for ((i=0; i<${1}; i++)); do
      local ncdir="$(dirname "${cdir}")"
      if [[ "${cdir}" == "${ncdir}" ]]; then
        break
      else
        cdir="${ncdir}"
      fi
    done
  fi
  cd "${cdir}"
}

# ----------------
# Conda aliases
# ----------------
alias csa='source activate'
alias cda='conda deactivate'
alias cls='conda env list'

# ----------------
# tmux aliases
# ----------------

alias tls='tmux ls'


tcs()
{
  csa "${1}" && tmux setenv TMUX_CONDA_DEFAULT_ENV "${1}" 
}

tat()
{
  if [[ "${1}" == "" ]]; then
    echo "Error: no target"
  else
    tmux a -t "${1}"
  fi
}

tns()
{
  if [[ "${1}" == "" ]]; then
    echo "Error: no session name"
  else
    tmux new-session -s "${1}"
  fi
}

tks()
{
  if [[ "${1}" == "" ]]; then
    echo "Error: no target"
  else
    tmux kill-session -t "${1}"
  fi
}

# ----------------
# other aliases
# ----------------
alias ipy='ipython'
alias smi='nvidia-smi'
alias sq='squeue'

jupyter_remote()
{
  if [[ "${1}" == "" ]]; then
    echo "Error: no port"
  else
    jupyter notebook --no-browser --port="${1}"
  fi
}
