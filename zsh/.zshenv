if (( ${+PREFIX} )) ; then
  export MANPATH=:$PREFIX/lib/node_modules/npm/man
fi
export LESS='--ignore-case --quit-if-one-screen'
