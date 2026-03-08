case $OSTYPE in
    darwin*)
        PATH="/opt/local/bin:/opt/local/sbin:$PATH"
        ;;
    linux*)
        PATH="/usr/local/go/bin:$PATH"
        ;;
esac

PATH=$PATH:$HOME/bin:$HOME/scripts
