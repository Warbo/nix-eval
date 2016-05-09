#!/usr/bin/env bash

# This script will invoke $CMD, which will usually be "runhaskell" with a bunch
# of flags set.
CMD="$1"

function debugMsg {
    # Debug output, if requested
    [[ -z "$NIX_EVAL_DEBUG" ]] || echo -e "nix-eval: $1" 1>&2
}

if [[ -n "$2" ]]
then
    # If we're running in a nested nix-shell, the PATH may not be set up
    # correctly. To work around this, we accept the desired environment's name
    # as $2 (e.g. "ghc-env-with-text") and look for it in PATH. If found, we
    # push it to the front of PATH, so its binaries (`ghc`, etc.) will be used.

    NAME="$2"

    debugMsg "Looking for Haskell environment '$NAME'"

    DIR=$(echo "$PATH" | grep -o -- "[^:]*${NAME}/[^:]*") || {
        echo "Couldn't find $NAME in PATH: $PATH" 1>&2
        exit 1
    }

    debugMsg "Moving '$DIR' to the front of PATH" 1>&2
    export PATH=$DIR:$PATH
fi

# Now we can run the given command
debugMsg "Running command '$CMD'"

debugMsg "PATH is $PATH"

INPUT=$(cat)

# Use hindent if available, so error message line numbers are more specific
if command -v hindent > /dev/null
then
    debugMsg "Running hindent on given input:\n\n$INPUT"
    INPUT=$(echo "$INPUT" | hindent --style fundamental)
fi

debugMsg "Evaluating:\n\n$INPUT\n---\n"

OUTPUT=$(echo "$INPUT" | $CMD)
CODE="$?"

debugMsg "Output:\n\n$OUTPUT"

echo "$OUTPUT"

debugMsg "Finished; exit code was '$CODE'"

exit "$CODE"
