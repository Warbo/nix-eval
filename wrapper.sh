#!/usr/bin/env bash

# This script will invoke $CMD, which will usually be "runhaskell" with a bunch
# of flags set.
CMD="$1"

# The PATH may not be set up correctly, which we work around by finding the bin
# directory for the package with given NAME (e.g. "ghc-env-with-text") and
# putting it to the front of PATH

NAME="$2"

DIR=$(echo "$PATH" | grep -o -- "[^:]*${NAME}/[^:]*") || {
    echo "Couldn't find $NAME in PATH: $PATH" >> /dev/stderr
    exit 1
}

echo "Moving '$DIR' to the front of PATH" >> /dev/stderr
export PATH=$DIR:$PATH

# Now we can run the given command
$CMD
