#!/usr/bin/env bash

function msg {
    echo -e "$*" 1>&2
}

function interpret {
    cabal repl -v0 --ghc-option=-XOverloadedStrings
}

function expression {
    EXPR='("head" $$ (("(:)" $$ ("show" $$ "True")) $$ "[]"))'
    PKGS="(withPkgs [\"pandoc\"] $EXPR)"
    if [[ PKG -eq 1 ]]
    then
        echo "eval $PKGS"
    else
        echo "eval $EXPR"
    fi
}

function run {
    expression | interpret
}

function getStdout {
    run
}

function getStdErr {
    # shellcheck disable=SC2069
    run 2>&1 1>/dev/null
}

# Tests

function testBuildable {
    MSG="Building nix-eval"
    if OUTPUT=$(cabal build 2>&1)
    then
        echo "ok - $MSG"
    else
        msg "$OUTPUT"
        echo "not ok - $MSG"
        return 1
    fi
}

function testSuite {
    MSG="Haskell test suite passes"
    if OUTPUT=$(cabal test 2>&1)
    then
        echo "ok - $MSG"
    else
        msg "$OUTPUT"
        echo "not ok - $MSG"
        return 1
    fi
}

function testHaveDataDir {
    MSG="Have data dir '$nix_eval_datadir'"
    if [[ -e "$nix_eval_datadir" ]]
    then
        echo "ok - $MSG"
    else
        echo "not ok - $MSG"
        return 1
    fi
}

function testEval {
    MSG="Checking we can evaluate expressions (PKG=$PKG)"
    OUTPUT=$(getStdout)
    if [[ "x$OUTPUT" = 'xJust "True"' ]]
    then
        echo "ok - $MSG"
    else
        msg "Unexpected output '$OUTPUT'"
        echo "not ok - $MSG"
        return 1
    fi
}

function testDebug {
    MSG="Checking nix-eval gives debug info (PKG=$PKG)"
    OUTPUT=$(getStdErr)
    if echo "$OUTPUT" | grep "^nix-eval: Evaluating:" > /dev/null
    then
        echo "ok - $MSG"
    else
        msg "No debug info on stderr: '$OUTPUT'"
        echo "not ok - $MSG"
        return 1
    fi
}

function testIndent {
    MSG="Checking expressions get indented"
    OUTPUT=$(getStdErr)
    if echo "$OUTPUT" | grep "Running hindent on given input" > /dev/null
    then
        echo "ok - $MSG"
    else
        msg "No indentation detected: '$OUTPUT'"
        echo "not ok - $MSG"
        return 1
    fi
}

function testHaskellOverride {
    MSG="Check we can override Haskell package set"
    echo "FIXME: make temp file with overrides, set it as env, then check it's used, e.g. by putting a 'trace' in the override"
    NIX_EVAL_HASKELL_PKGS=$(mktemp --tmpdir "nix-eval-test-overrides-XXXXX.nix")
    export NIX_EVAL_HASKELL_PKGS
    echo "Using overrides '$NIX_EVAL_HASKELL_PKGS'" 1>&2

    SENTINEL="sentinel-$RANDOM"
    NIX_EXPR="builtins.trace \"$SENTINEL\" (import <nixpkgs> {}).haskellPackages"
    echo "$NIX_EXPR" > "$NIX_EVAL_HASKELL_PKGS"

    OUTPUT=$(getStdErr)
    if echo "$OUTPUT" | grep "trace: $SENTINEL" > /dev/null
    then
        echo "ok - $MSG"
    else
        msg "Sentinel '$SENTINEL' from '$NIX_EVAL_HASKELL_PKGS' not spotted"
    fi

    rm -f "$NIX_EVAL_HASKELL_PKGS"
    unset NIX_EVAL_HASKELL_PKGS
}

# Invocation

function testPreconditions {
    testBuildable &&
    testSuite     &&
    testHaveDataDir
}

function runTests {
    testEval
    testDebug
    testIndent
    testHaskellOverride
}

export NIX_EVAL_DEBUG=1

nix_eval_datadir="$(dirname "$(readlink -f "$0")")"
export nix_eval_datadir

# If these fail, our tests are unrunnable so we bail out
testPreconditions || exit 1

msg "Testing expression without external dependencies"
export PKG=0
runTests

msg "Testing expression with external dependencies"
export PKG=1
runTests
