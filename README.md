# Multiarg

This is multiarg, a library of combinators to parse command lines.

For "released" code see Hackage:

http://hackage.haskell.org/package/multiarg

multiarg is on Github:

http://www.github.com/massysett/multiarg

## Building

If you obtained this code through Hackage, just build it using the
ordinary Cabal command:

cabal install

If you obtain this code on Github, you will first need to generate the
Cabal file and generate the tests.  This will require that you install
two libraries:

cabal install cartel quickpull

Then run this script to generate the Cabal file and the tests:

sh generate

## Versioning

multiarg releases are numbered in accordance with the Haskell
Package Versioning Policy.

Currently the multiarg library depends only on the "base" package, so
multiarg should have wide compatibility with different compilers and
sets of libraries.  The tests have some additional dependencies.

## Build history

If you're having trouble building multiarg, try looking at the
travis-ci build history at:

https://travis-ci.org/massysett/multiarg

It shows successful builds and the versions of any package
dependencies that were installed when that build succeeded, so it
might help you diagnose any dependency issues.

[![Build Status](https://travis-ci.org/massysett/multiarg.svg?branch=master)](https://travis-ci.org/massysett/multiarg)
