# Multiarg

This is multiarg, a library of combinators to parse command lines.

For "released" code see Hackage:

http://hackage.haskell.org/package/multiarg

multiarg is on Github:

http://www.github.com/massysett/multiarg

## Versioning

multiarg releases are numbered in accordance with the Haskell
Package Versioning Policy.

multiarg does not set its dependencies in accordance with the
Package Versioning Policy, as I do not set upper bounds.  multiarg
is guaranteed to build with the *minimum* versions specified in the
cabal file.  I also include a current-versions.txt file that
documents more recent dependencies that are also known to work.

If you find that multiarg does not build due to dependency problems:
1) please let me know at omari@smileystation.com; 2) feel free to
add appropriate upper bounds or patches to the package as
appropriate; and 3) feel free to add command-line contraints to your
cabal command to get it to build.
