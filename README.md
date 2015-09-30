# Dependency-Injecting Eval for Haskell

This Haskell package is a crude implementation of `eval`, as found in dynamic
languages like Lisp, Python, Javascript, etc. It lets us construct Haskell
code programatically (either at run time, or in Template Haskell), and attempt
to evaluate it.

What sets this package apart from other `eval` implementations, is the ability
to control which packages and modules are available during evaluation. This is
achieved by calling out to the [Nix package manager](http://nix.nixos.org).

# Implementation Details

`Expr` is the type of expressions, which contains a list of package names, a
list of modules to import and a `String` of Haskell code. All of these are just
`String`s internally, but the wrappers prevent accidentally using package as
modules, etc. A few combinators are provided for common manipulations, and the
`OverloadedStrings` extension allows packages, modules and expressions to be
written as literals. Note that literal expressions are given an empty context;
you will have to specify any required modules or packages separately.

When evaluated, the Haskell code is prefixed by an import of each module and
wrapped in `main = putStr (..)`, then piped into `runhaskell`. That process is
invoked via the `nix-shell` command, using Nix's standard
`haskellPackages.ghcWithPackages` function to ensure the GHC instance has all of
the given packages available.

If the process exits successfully, its stdout will be returned wrapped in
`Just`; otherwise `Nothing` is returned.

This implementation is a little rough; for example, you may prefer to use `Text`
rather than `String`; use a better representation like the syntax trees from
TemplateHaskell or `haskell-src-exts` instead; or accumulate packages and
modules monadically.

The intention of this library is to provide a simple, minimal base to support
such design choices, and `String` is the lowest common denominator. You're
welcome, and encouraged, to build more sophisticated APIs; as long as you can
pretty-print to a `String`, they should work out of the box.

This is also why we return the contents of stdout, rather than trying to parse
it into a more appropriate type: it's not our place to choose how the result
should be parsed, so we avoid the problem; by that point, our job is done.

# Limitations

 - Since evaluation takes place in a separate GHC process, there can be no
   sharing of data (unless you provide a separate mechanism like a FIFO)
 - Expressions are wrapped in `putStr`, so the expression must be a `String`.
   You may need to marshall your data into a form which is more amenable to
   serialising/deserialising via `String`.
 - Evaluation is **SLOW**! More specifically, `eval` has a very high latency, so
   it's *much* more efficient to `eval` one big collection of values than it is
   to `eval` each individually.
 - Evaluation time is highly variable, since the required packages may need to
   be compiled. Nix caches build products, so subsequent calls using the same
   packages will be quicker; however, my machine still takes about 2 seconds to
   instantiate a cached environment.
 - Output is captured from stdout, so if your expression triggers side-effects
   they'll appear in your result (this may be desirable, but keep it in mind).
 - Evaluation doesn't always compose, ie. just because `x` and `y` evaluate
   successfully doesn't mean that some combination of them will. Obviously an
   ill-typed combination will fail, but other reasons include:
    - Combining both import lists can make names ambiguous. For this reason you
      should always try to qualify your expressions.
    - Global properties may conflict between modules, like overlapping typeclass
      instances.
    - Combining both package lists can make modules ambiguous.
    - If the dependencies of two packages conflict, evaluation will fail.
 - Language extensions aren't handled yet. I'll add them as and when the need
   arises.
 - As with any kind of `eval`, there is absolutely no security. Do not pass
   potentially-malicious user input to this library.
