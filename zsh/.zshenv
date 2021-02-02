path=( $HOME/bin $path /usr/local/go/bin )
if (( ${+PREFIX} )) ; then
  # TODO: There's presumably an equivalent that we should set for the Linux case
  manpath=( '' $PREFIX/lib/node_modules/npm/man )
fi
export LESS='--ignore-case --quit-if-one-screen'
export EDITOR=nvim
