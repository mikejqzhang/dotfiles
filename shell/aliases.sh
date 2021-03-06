# ----------------
# Personal aliases
# ----------------

# TODO: Automatically select which options depending on mac vs linux
# Use colors in coreutils utilities output
alias ls='ls --color=auto'
# alias ls='ls -G'

alias grep='grep --color'

# ls aliases
alias ll='ls -lah'
alias la='ls -A'

# aliases to protect against overwriting
alias cp='cp -i'
alias mv='mv -i'

# Personal
alias cl='clear'

# LaTeX aliases
alias pdflatexrm='rm *.aux *.log *.toc *.out *.bbl *.blg'

# Update dotfiles
dfu() {
  (
  cd ~/.dotfiles && git pull --ff-only && ./install -q
  )
}

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
# Application aliases
# ----------------
alias sublime='open -a Sublime\ Text .'

# ----------------
# Conda aliases
# ----------------
alias csa='source activate'
alias cda='conda deactivate'
alias cls='conda env list'

# ----------------
# ssh aliases
# ----------------

alias foch='ssh mjqzhang@foch.cs.washington.edu'

attu()
{
  if [[ "${1}" == "" ]]; then
    ssh mjqzhang@attu2.cs.washington.edu
  else
    ssh "mjqzhang@attu${1}.cs.washington.edu"
  fi
}

scp_attu()
{
  if [[ "${1}" == "" ]]; then
    echo "Error: no target"
  else
    if [[ "${2}" == "" ]]; then
      path="."
    else
      path="${2}"
    fi
    scp "mjqzhang@attu.cs.washington.edu:${1}" "$path"
  fi
}

scp_to_attu()
{
  if [[ "${1}" == "" ]]; then
    echo "Error: no target"
  else
    if [[ "${2}" == "" ]]; then
      path="/homes/iws/mjqzhang/scp_dump"
    else
      path="${2}"
    fi
    scp "${1}" "mjqzhang@attu.cs.washington.edu:$path"
  fi
}

# ----------------
# tmux aliases
# ----------------

alias tls='tmux ls'


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
    echo "error: no target"
  else
    tmux kill-session -t "${1}"
  fi
}

# ----------------
# python aliases
# ----------------

alias ipy='ipython'
alias nlp='allennlp'

pym()
{
  if [[ "${1}" == "" ]]; then
    echo "error: no target"
  else
    mod=`echo "${1}" | sed "s/\//./g" | sed "s/\.py//g"`
    # echo "${mod} ${@:2}"
    python -m ${mod} ${@:2}
  fi
}

# ----------------
# other aliases
# ----------------
alias smi='nvidia-smi'
alias sq='squeue'

