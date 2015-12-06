if [ ! -e "$exe" ]
then
    echo "Cannot run test. Did you build FOmega? Try:"
    echo "$ cabal build"
    exit 1
fi

if [ -z "$1" ]
then
    echo "Usage: $0 <filename>"
    exit 1
fi

"$exe" "$1"
