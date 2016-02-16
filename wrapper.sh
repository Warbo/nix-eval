#!/usr/bin/env bash

# This script will invoke $CMD, which will usually be "runhaskell" with a bunch
# of flags set.
CMD="$1"

function debugMsg {
    # Debug output, if requested
    [[ -z "$NIX_EVAL_DEBUG" ]] || echo -e "nix-eval: $1" >> /dev/stderr
}

# The PATH may not be set up correctly, which we work around by finding the bin
# directory for the package with given NAME (e.g. "ghc-env-with-text") and
# putting it to the front of PATH

NAME="$2"

debugMsg "Looking for Haskell environment '$NAME'"

DIR=$(echo "$PATH" | grep -o -- "[^:]*${NAME}/[^:]*") || {
    echo "Couldn't find $NAME in PATH: $PATH" >> /dev/stderr
    exit 1
}

debugMsg "Moving '$DIR' to the front of PATH" >> /dev/stderr
export PATH=$DIR:$PATH

# Now we can run the given command
debugMsg "Running command '$CMD'"

if [[ -z "$NIX_EVAL_DEBUG" ]]
then
    $CMD
else
    INPUT=$(cat)
    debugMsg "Evaluating:\n\n$INPUT\n---\n"
    echo "$INPUT" | $CMD
fi
