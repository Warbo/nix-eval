#!/usr/bin/env bash

# This script will invoke $CMD, which will usually be "runhaskell" with a bunch
# of flags set.
CMD="$1"

function shouldDebug {
    [[ -n "$NIX_EVAL_DEBUG" ]]
}

function debugMsg {
    # Debug output, if requested
    if shouldDebug
    then
        echo -e "nix-eval: $1" 1>&2
    fi
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

ORIG_INPUT=$(cat)
INPUT="$ORIG_INPUT"

# If we're debugging, use hindent if available, so error message line numbers
# are more specific
if shouldDebug && command -v hindent > /dev/null
then
    debugMsg "Trying hindent on given input:\n\n$INPUT"
    if command -v timeout > /dev/null
    then
        INPUT=$(echo "$ORIG_INPUT" | timeout 20 hindent --style fundamental) || {
            echo "WARNING: hindent failed!"
            INPUT="$ORIG_INPUT"
        }
    else
        INPUT=$(echo "$ORIG_INPUT" | hindent --style fundamental) || {
            echo "WARNING: hindent failed!"
            INPUT="$ORIG_INPUT"
        }
    fi
fi

debugMsg "Evaluating:\n\n$INPUT\n---\n"

OUTPUT=$(echo "$INPUT" | $CMD)
CODE="$?"

debugMsg "Output:\n\n$OUTPUT"

echo "$OUTPUT"

debugMsg "Finished; exit code was '$CODE'"

exit "$CODE"
