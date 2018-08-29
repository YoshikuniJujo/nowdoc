{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Text.Nowdoc (nowdoc) where

import Language.Haskell.TH (litE, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

nowdoc :: QuasiQuoter
nowdoc = QuasiQuoter {
	quoteExp = litE . stringL . unescape . dropHeadNewline,
	quotePat = const . error $ errorString "a pattern",
	quoteType = const . error $ errorString "a type",
	quoteDec = const . error $ errorString "a declaration"
	}

errorString :: String -> String
errorString ctx =
	"You have used the `nowdoc' QuasiQoter in " ++ ctx ++
	" context; you must only use it in an expression context"

dropHeadNewline :: String -> String
dropHeadNewline ('\n' : cs) = cs
dropHeadNewline cs = cs

unescape :: String -> String
unescape ('|' : cs) = case span (== ' ') cs of
	(_ : ss, ']' : cs') -> '|' : ss ++ "]" ++ unescape cs'
	(ss, cs') -> '|' : ss ++ unescape cs'
unescape (c : cs) = c : unescape cs
unescape "" = ""
