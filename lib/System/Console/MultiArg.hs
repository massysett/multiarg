-- | A combinator library for building command-line parsers.

module System.Console.MultiArg (

  -- | To say this library is inspired by Parsec would probably insult the
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

  -- * Terminology

  -- | Some terms are used throughout multiarg:
  --
  -- [@word@] When you run your program from the Unix shell prompt,
  -- your shell is responsible for splitting the command line into
  -- words. Typically you separate words with spaces, although quoting
  -- can affect this. multiarg parses lists of words. Each word can
  -- consist of a single long option, a single long option and an
  -- accompanying option argument, a single short option, multiple
  -- short options, and even one or more multiple short options and an
  -- accompanying short option argument. Or, a word can be a
  -- positional argument or a stopper. All these are described below.
  --
  -- [@option@] Options allow a user to specify ways to tune the
  -- operation of a program. Typically options are indeed optional,
  -- although some programs do sport \"required options\" (a bit of an
  -- oxymoron). Options can be either short options or long
  -- options. Also, options can take arguments.
  --
  -- [@short option@] An option that is specified with a single hyphen
  -- and a single letter. For example, for the program @tail(1)@,
  -- possible short options include @n@ and @v@. With multiarg it is
  -- possible to easily parse short options that are specified in
  -- different words or in the same word. For example, if a user wants
  -- to run @tail@ with two options, he might type @tail -v -f@ or he
  -- might type @tail -vf@.
  --
  -- [@long option@] An option that is specified using two hyphens and
  -- what is usually a mnemonic word, though it could be as short as a
  -- single letter. For example, @tail(1)@ has long options including
  -- @follow@ and @verbose@. The user would specify these on the
  -- command line by typing @tail --follow --verbose@.
  --
  -- [@option argument@] Some options take additional arguments that
  -- are specific to the option and change what the option does. For
  -- instance, the @lines@ option to @tail(1)@ takes a single,
  -- optional argument, which is the number of lines to show. Option
  -- arguments can be optional or required, and a single option can
  -- take a mulitple, fixed number of arguments and it can take a
  -- variable number of arguments. Option arguments can be given in
  -- various ways. They can be specified in the same word as a long
  -- option by using an equals sign; they can also be specified in the
  -- same word as a short option simply by placing them in the same
  -- word, or they can be specified in the following word. For
  -- example, these different command lines all mean the same thing;
  -- @tail --verbose --lines=20@, @tail --verbose --lines 20@, @tail
  -- -vn 20@, @tail -v -n20@, @tail -vn20@, and @tail -v -n 20@, and
  -- numerous other combinations also have the same meaning.
  --
  -- [@GNU-style option argument@] A long option with an argument
  -- given with an equal sign, such as [@lines=20@].
  --
  -- [@positional argument@] A word on the command line that is not an
  -- option or an argument to an option. For instance, with @tail(1)@,
  -- you specify the files you want to see by using positional
  -- arguments. In the command @tail -n 10 myfile@, @myfile@ is a
  -- positional argument. For some programs, such as @git@ or @darcs@,
  -- a positional argument might be a \"command\" or a \"mode\", such
  -- as the @commit@ in @git commit@ or the @whatsnew@ in @darcs
  -- whatsnew@. multiarg has no primitive parsers that treat these
  -- positional arguments specially but it is trivial to build a
  -- parser for command lines such as this, too.
  --
  -- [@stopper@] A single word consisting solely of two hyphens,
  -- @--@. The user types this to indicate that all subsequent words
  -- on the command line are positional arguments, even if they begin
  -- with hyphens and therefore look like they might be options.
  --
  -- [@pending@] The user might specify more than one short option, or
  -- a short option and a short option argument, in a single word. For
  -- example, she might type @tail -vl20@. After parsing the @v@
  -- option, the Parser makes @l20@ into a \"pending\". The next
  -- parser can then treat @l20@ as an option argument to the @v@
  -- option (which is probably not what was wanted) or the next parser
  -- can parse @l@ as a short option. This would result in a
  -- \"pending\" of @20@. Then, the next parser can treat @20@ as an
  -- option argument. After that parse there will be no pendings.

  -- * Getting started

  -- |If your needs are simple to moderately complicated just look at the
  -- "System.Console.MultiArg.SimpleParser" module, which uses the
  -- underlying combinators to build a simple parser for you. That
  -- module is already exported from this module for easy usage.
  --
  -- "System.Console.MultiArg.SimpleParser" also has a parser that can
  -- handle multi-mode commands (examples include @git@, @darcs@, and
  -- @cvs@.)
  --
  -- For maximum flexibility you will want to start with the
  -- "System.Console.MultiArg.Prim" module. Using those parsers you
  -- can easily build parsers that are quite complicated. The parsers
  -- can check for errors along the way, simplifying the sometimes
  -- complex task of ensuring that data a user supplied on the command
  -- line is good. You can easily build parsers for programs that take
  -- no options, take dozens of options, require that options be given
  -- in a particular order, require that some options be given, or bar
  -- some combinations of options. You might also require particular
  -- positional arguments. Other helpful functions are in
  -- "System.Console.MultiArg.Combinator". You will also want to
  -- examine the source code for "System.Console.MultiArg.Combinator"
  -- and "System.Console.MultiArg.SimpleParser" as these show some
  -- ways to use the primitive parsers and combinators.

  -- * Non-features and shortcomings
  --
  -- | multiarg isn't perfect; no software is. multiarg does not
  -- automatically make online help for your command line
  -- parsers. Getting this right would be tricky given the nature of
  -- the code and I don't even want to bother trying, as I just write
  -- my own online help in a text editor.
  --
  -- multiarg partially embraces \"The Tao of Option Parsing\" that
  -- Python's Optik (<http://optik.sourceforge.net/>) follows. Read
  -- \"The Tao of Option Parsing\" here:
  --
  -- <http://optik.sourceforge.net/doc/1.5/tao.html>
  --
  -- multiarg's philosophy is similar to that of Optik, which
  -- means you won't be able to use multiarg to (easily) build a clone
  -- to the UNIX @find(1)@ command. (You could do it, but multiarg won't
  -- help you very much.)
  --
  -- multiarg can be complicated, although I'd like to believe this is
  -- because it addresses a complicated problem in a flexible way.

  -- * Projects usings multiarg

  -- | * Penny, an extensible double-entry accounting
  -- system. <http://hackage.haskell.org/package/penny-lib> The code
  -- using multiarg is woven throughout the system; for example, see
  -- the Penny.Liberty module.


    module System.Console.MultiArg.Combinator
  , module System.Console.MultiArg.CommandLine
  , module System.Console.MultiArg.GetArgs
  , module System.Console.MultiArg.Option
  , module System.Console.MultiArg.Prim
  , module Control.Monad.Exception.Synchronous
  ) where

import System.Console.MultiArg.Combinator
import System.Console.MultiArg.CommandLine
import System.Console.MultiArg.GetArgs
import System.Console.MultiArg.Option
import System.Console.MultiArg.Prim
import Control.Monad.Exception.Synchronous
