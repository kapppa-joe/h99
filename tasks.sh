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
    sensei)
        stack exec sensei test/Spec.hs
    ;;
    *)
        echo "Invalid command: '${command}'"
        exit 1
    esac
done