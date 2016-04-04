#!/usr/bin/env bash

# This script will invoke $CMD, which will usually be "runhaskell" with a bunch
# of flags set.
CMD="$1"

function debugMsg {
    # Debug output, if requested
    [[ -z "$NIX_EVAL_DEBUG" ]] || echo -e "nix-eval: $1" 1>&2
}

# The PATH may not be set up correctly, which we work around by finding the bin
# directory for the package with given NAME (e.g. "ghc-env-with-text") and
# putting it to the front of PATH

NAME="$2"

debugMsg "Looking for Haskell environment '$NAME'"

DIR=$(echo "$PATH" | grep -o -- "[^:]*${NAME}/[^:]*") || {
    echo "Couldn't find $NAME in PATH: $PATH" 1>&2
    exit 1
}

debugMsg "Moving '$DIR' to the front of PATH" 1>&2
export PATH=$DIR:$PATH

# Now we can run the given command
debugMsg "Running command '$CMD'"

INPUT=$(cat)
debugMsg "Evaluating:\n\n$INPUT\n---\n"

OUTPUT=$(echo "$INPUT" | $CMD)
CODE="$?"

debugMsg "Output:\n\n$OUTPUT"

echo "$OUTPUT"

debugMsg "Finished; exit code was '$CODE'"

exit "$CODE"
