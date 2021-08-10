PS1='%(?..(%?%))%# '
setopt EXTENDED_GLOB EXTENDED_HISTORY INC_APPEND_HISTORY APPEND_HISTORY HIST_IGNORE_SPACE HIST_NO_STORE
setopt NO_NOMATCH

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

bindkey -e
alias odz='od -tx1z'
