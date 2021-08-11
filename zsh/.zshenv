path=( $HOME/bin $path /usr/local/go/bin )
if (( ${+PREFIX} )) ; then
  # TODO: There's presumably an equivalent that we should set for the Linux case
  manpath=( '' $PREFIX/lib/node_modules/npm/man )
fi

# TODO: add --quit-if-one-screen once we get a recent enough version of `less` where it actually works
export LESS=--ignore-case

export EDITOR=nvim
