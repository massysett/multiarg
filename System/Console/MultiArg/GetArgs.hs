{-# LANGUAGE CPP #-}

-- | Get the arguments from the command line, ensuring they are
-- properly encoded into Unicode.
--
-- base 4.3.1.0 has a System.Environment.getArgs that does not return
-- a Unicode string. Instead, it simply puts each octet into a
-- different Char. Thus its getArgs is broken on UTF-8 and nearly any
-- non-ASCII encoding. As a workaround I use
-- System.Environment.UTF8. The downside of this is that it requires
-- that the command line be encoded in UTF8, regardless of what the
-- default system encoding is.
--
-- Unlike base 4.3.1.0, base 4.4.0.0 actually returns a proper Unicode
-- string when you call System.Environment.getArgs. (base 4.3.1.0
-- comes with ghc 7.0.4; base 4.4.0.0 comes with ghc 7.2.) The string
-- is encoded depending on the default system locale. The only problem
-- is that System.Environment.UTF8 apparently simply uses
-- System.Environment.getArgs and then assumes that the string it
-- returns has not been decoded. In other words,
-- System.Environment.UTF8 assumes that System.Environment.getArgs is
-- broken, and when System.Environment.getArgs was fixed in base
-- 4.4.0.0, it likely will break System.Environment.UTF8.
--
-- One obvious solution to this problem is to find some other way to
-- get the command line that will not break when base is updated. But
-- it was not easy to find such a thing. The other libraries I saw on
-- hackage (as of January 6, 2012) had problems, such as breakage on
-- ghc 7.2. There is a package that has a simple interface to the UNIX
-- setlocale(3) function, but I'm not sure that what it returns easily
-- and reliably maps to character encodings that you can use with,
-- say, iconv.
--
-- So by use of Cabal and preprocessor macors, the code uses
-- utf8-string if base is less than 4.4, and uses
-- System.Environment.getArgs if base is at least 4.4.
--
-- The GHC bug is here:
--
-- <http://hackage.haskell.org/trac/ghc/ticket/3309>

module System.Console.MultiArg.GetArgs ( getArgs, getProgName ) where

#if MIN_VERSION_base(4,4,0)
import qualified System.Environment as E ( getArgs, getProgName )
#else
import qualified System.Environment.UTF8 as E ( getArgs, getProgName )
#endif

getArgs :: IO [String]
getArgs = E.getArgs

getProgName :: IO String
getProgName = E.getProgName
