umask 077

# TODO: add --quit-if-one-screen once we get a recent enough version of `less` where it actually works
export LESS=--ignore-case

if [[ -v INSIDE_EMACS ]] ; then
    EDITOR=emacsclient
    export DPKG_PAGER=cat
else
    EDITOR=nvim
fi
export EDITOR
export GOBIN=$HOME/bin

SHELL_SESSIONS_DISABLE=1
