{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Text.Nowdoc (nowdoc) where

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

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
