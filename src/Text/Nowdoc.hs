{- |

Module:		Text.Nowdoc
Description:	3 QuasiQuoter: nowdoc, txtfile, binfile
Copyright:	(c) Yoshikuni Jujo, 2018
License:	BSD-3-Clause
Maintainer:	PAF01143@nifty.ne.jp

With QuasiQuotes language extension.

-}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Text.Nowdoc (nowdoc, txtfile, binfile) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Language.Haskell.TH (ExpQ, stringE, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Data.ByteString.Char8 as BSC

{- |
Simplest here document. Only two transformation.

* remove head newline if exist
* remove one space from '|', space, space, ..., ']'

@
main :: IO ()
main = putStrLn [nowdoc|hello|]
@

@
main :: IO ()
main = putStr [nowdoc|
Hello, world!
|]
@

@
main :: IO ()
main = putStr [nowdoc|
main :: IO ()
main = putStr [nowdoc|
Hello, world!
| ]
|]
@
-}

nowdoc :: QuasiQuoter
nowdoc = QuasiQuoter {
	quoteExp = stringE . unescape . \case '\n' : cs -> cs; cs -> cs,
	quotePat = const $ err "pattern",
	quoteType = const $ err "type",
	quoteDec = const $ err "declaration" }

unescape :: String -> String
unescape ('|' : cs) = case span (== ' ') cs of
	(_ : ss, ']' : cs') -> '|' : ss ++ "]" ++ unescape cs'
	(ss, cs') -> '|' : ss ++ unescape cs'
unescape (c : cs) = c : unescape cs
unescape "" = ""

err :: String -> a
err ctx = error $
	"You have used the `nowdoc' QuasiQoter in a " ++ ctx ++
	" context; you must only use it in an expression context"

{- |
QuasiQuoter txtfile incerts file contents as string without transformation.
It read file as text file (with default encoding on your system).

@
main :: IO ()
main = putStr [txtfile|foo.txt|]
@
-}

txtfile :: QuasiQuoter
txtfile = QuasiQuoter {
	quoteExp = readFileQ,
	quotePat = const $ err "pattern",
	quoteType = const $ err "type",
	quoteDec = const $ err "declaration" }

readFileQ :: FilePath -> ExpQ
readFileQ fp_ = do
	cnt <- runIO $ readFile fp
	addDependentFile fp
	stringE cnt
	where fp = dropWhile isSpace $ dropWhileEnd isSpace fp_

{- |
QuasiQuoter binfile incerts file contents as string without transformation.
It read file as binary file.

@
main :: IO ()
main = putStr [binfile|foo.dat|]
@
-}

binfile :: QuasiQuoter
binfile = QuasiQuoter {
	quoteExp = readBinFileQ,
	quotePat = const $ err "pattern",
	quoteType = const $ err "type",
	quoteDec = const $ err "declaration" }

readBinFileQ :: FilePath -> ExpQ
readBinFileQ fp_ = do
	cnt <- runIO $ BSC.unpack <$> BSC.readFile fp
	addDependentFile fp
	stringE cnt
	where fp = dropWhile isSpace $ dropWhileEnd isSpace fp_
