-- | A combinator library for building command-line parsers.
--
-- To say this library is inspired by Parsec would probably insult the
-- creators of Parsec, as this library could not possibly be as
-- elegant or throughly considered as Parsec is. Nevertheless this
-- library can be used in a similar style as Parsec, but is
-- specialized for parsing command lines.
--
-- This parser was built because I could not find anything that would
-- readily parse command lines where the options took more than one
-- argument. For example, for the @tail@ command on GNU systems, the
-- --lines option takes one argument to specify how many lines you
-- want to see. Well, what if you want to build a program with an
-- option that takes /two/ arguments, like @--foo bar baz@? I found no
-- such library so I built this one. Nevertheless, using this library
-- you can build parsers to parse a variety of command line
-- vocabularies, from simple to complex.
--
-- If your needs are simple to moderately complicated just look at the
-- "System.Console.MultiArg.SimpleParser" module, which uses the
-- underlying combinators to build a simple parser for you. That
-- module is already exported from this module for easy usage. For
-- maximum flexibility you will want to start with the
-- "System.Console.MultiArg.Prim" module.
--
-- Internally the combinators in "System.Console.MultiArg.Prim" use
-- strict "Data.Text" values rather than "String"s. That is because
-- this library was built for an application that sometimes parses an
-- enormous amount of command line data, and I thought that using
-- Data.Text would yield some memory savings while retaining Unicode
-- safety. Though I cannot remember whether this actually yielded any
-- space savings (it did not lead to more space usage, at least) it
-- also made the parser consistent with the rest of that program,
-- which also uses Data.Text. I have considered making this library
-- use either Data.Text or Strings but that makes it a lot more
-- complicated for little gain. The SimpleParser module, however,
-- wraps the "Data.Text" values up and exposes only Strings in the
-- interface, keeping things nice and simple. This does mean that
-- Strings have to be converted to Data.Text and back again, but the
-- performance hit will not be significant unless you are parsing an
-- obscene amount of data--and if you're doing that, you might want to
-- use Data.Text anyway :)
--
-- multiarg embraces \"The Tao of Option Parsing\" that Python's Optik
-- (<http://optik.sourceforge.net/>) follows. Read \"The Tao of Option
-- Parsing\" here:
--
-- <http://optik.sourceforge.net/doc/1.5/tao.html>
--
-- multiarg uses the same terminology and the same philosophy, which
-- means you won't be able to use multiarg to (easily) build a clone
-- to the UNIX @find(1)@ command. (You could do it, but multiarg won't
-- help you very much.)
module System.Console.MultiArg (
  module System.Console.MultiArg.SimpleParser ) where

import System.Console.MultiArg.SimpleParser
