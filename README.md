# Eval for Haskell

This Haskell package is a crude implementation of `eval`, as found in dynamic
languages like Lisp, Python, Javascript, etc. It lets us construct Haskell
code programatically (either at run time, or in Template Haskell), and attempt
to evaluate it.

What sets this package apart from other `eval` implementations, is the ability
to control which packages and modules are available during evaluation. This is
achieved by calling out to the Nix package manager.

# Implementation Details

`Expr` is the type of expressions, which contains a list of package names, a
list of modules to import and a `String` of Haskell code. It's recommended to
use the combinators provided for creating and composing expressions, rather than
manually editing the `String`s.

To evaluate an `Expr`ession, the Haskell code is prefixed by import of each
module and `main = putStr . show $ `, then piped into `runhaskell`. That process
is invoked via the `nix-shell` command, using `haskellPackages.ghcWithPackages`
to provide GHC with a package database containing all of the given packages.

If the process exits successfully its stdout will be returned wrapped in `Just`,
otherwise `Nothing` is returned.

# Limitations

 - Since evaluation takes place in a separate GHC process, there can be no
   sharing of data (unless you provide a separate mechanism like a FIFO)
 - Expressions are wrapped in `print`, so the result must be an instance of
   `Show`. You may need to marshall your data into a form which is more amenable
   to serialising/deserialising via `String`.
 - Evaluation is **SLOW**! More specifically, `eval` has a very high latency
   (about 2 seconds on my machine), so it's *much* more efficient to `eval` one
   big collection of values than it is to `eval` each individually.
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
 - Language extensions are handled yet. I'll add them as and when the need
   arises.
