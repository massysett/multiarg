= Multiarg

This is multiarg, a library of combinators to parse command lines.

Since you are reading this you are examining the source code
which is kept in Git. This code is "pre-release," meaning that although
the "released" code has zero warranty, this code has negative
warranty :) However I try to keep the "master" branch in an always
working state.

For "released" code see Hackage:

http://hackage.haskell.org/package/multiarg

== Versioning

multiarg releases are numbered in accordance with the Haskell
Package Versioning Policy.

multiarg does not set its dependencies in accordance with the
Package Versioning Policy, as I do not set upper bounds.  multiarg
is guaranteed to build with the *minimum* versions specified in the
cabal file.  I also include a tested-dependencies file that
documents more recent dependencies that are also known to work.

If you find that multiarg does not build due to dependency problems:
1) please let me know at omari@smileystation.com; 2) feel free to
add appropriate upper bounds or patches to the package as
appropriate; and 3) feel free to add command-line contraints to your
cabal command to get it to build.
