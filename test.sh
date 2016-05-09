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
    run 2>/dev/null
}

function getStderr {
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
    OUTPUT=$(getStderr)
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
    OUTPUT=$(getStderr)
    if echo "$OUTPUT" | grep "Running hindent on given input" > /dev/null
    then
        echo "ok - $MSG"
    else
        msg "No indentation detected: '$OUTPUT'"
        echo "not ok - $MSG"
        return 1
    fi
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
}

export NIX_EVAL_DEBUG=1
export nix_eval_datadir="$(dirname "$(readlink -f "$0")")"

# If these fail, our tests are unrunnable so we bail out
testPreconditions || exit 1

msg "Testing expression without external dependencies"
export PKG=0
runTests

msg "Testing expression with external dependencies"
export PKG=1
runTests
