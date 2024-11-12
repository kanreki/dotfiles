PS1='%(?..(%?%))%# '
setopt EXTENDED_GLOB EXTENDED_HISTORY INC_APPEND_HISTORY APPEND_HISTORY HIST_IGNORE_SPACE HIST_NO_STORE
setopt NO_NOMATCH
[[ -v INSIDE_EMACS ]] && setopt NO_ZLE NO_PROMPT_CR NO_PROMPT_SP

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

alias odz='od -tx1z'
bindkey -e

function make_restic_url() {
	read -s '?Password: ' my_password
	printf -v RESTIC_REPOSITORY rest:http://alan:%s@serenity.local:8088/ "$my_password"
	export RESTIC_REPOSITORY
}
