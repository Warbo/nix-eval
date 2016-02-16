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

F=$(mktemp -t "nix-eval-XXXXX.hs")
F_EXE=$(echo "$F" | rev | cut -d . -f 2- | rev)

debugMsg "Writing input to '$F'"
INPUT=$(tee "$F")

debugMsg "Evaluating:\n\n$INPUT\n---\n"

debugMsg "Running '$CMD -o \"$F_EXE\" \"$F\"'"
GHC_OUTPUT=$($CMD -o "$F_EXE" "$F")
CODE="$?"

debugMsg "Compiler output:\n\n$GHC_OUTPUT\n\n"

[[ "$CODE" -eq 0 ]] || {
    debugMsg "Failed to compile"
    exit "$CODE"
}

[[ -f "$F_EXE" ]] || {
    debugMsg "No such file '$F_EXE'"
    exit 1
}

debugMsg "Running '$F_EXE'"
: | "$F_EXE"
CODE="$?"

debugMsg "Finished; exit code was '$CODE'"

debugMsg "Removing '$F'"
rm "$F"

debugMsg "Removing '$F_EXE'"
rm -f "$F_EXE"

debugMsg "Done"

exit "$CODE"
