set -e

for command in "$@"
do
    case "${command}" in
    ghci)
        cabal exec ghci test/Spec.hs
    ;;
    stack)
        stack test
    ;;
    *)
        echo "Invalid command: '${command}'"
        exit 1
    esac
done