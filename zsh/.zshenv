path=( $HOME/bin $path /usr/local/go/bin /usr/lib/dart/bin)

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
