module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)

import DOM (DOM)

import Flare (UI, string)
import Flare.Smolder (runFlareHTML)

import Signal.Channel (CHANNEL)

import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M

type Markup = M.Markup Unit

data Resolution = Match | NoMatch

toBool ∷ Resolution → Boolean
toBool Match   = true
toBool NoMatch = false

data Action = Rock | Paper | Scissors | Lizard | Spock

name ∷ Action → String
name Rock     = "Rock"
name Paper    = "Paper"
name Scissors = "Scissors"
name Lizard   = "Lizard"
name Spock    = "Spock"

tdStyle ∷ String → String → Markup
tdStyle cn str = H.td ! A.className cn $ M.text str

err ∷ String → Markup
err = tdStyle "err"

okay ∷ String → Markup
okay = tdStyle "okay"

renderTable ∷ String → String → String → Maybe String → Maybe String → Markup
renderTable pRock pPaper pScissors pLizard pSpock =
  H.table $ do
    H.tr $ do
      H.th (M.text "Pattern")
      H.th (M.text "Valid regex")
      H.th (M.text "Matches")
      when (not simple) $ H.th (M.text "Matches")
      H.th (M.text "Doesn't match")
      when (not simple) $ H.th (M.text "Doesn't match")

    if simple
      then do
        row Rock     Scissors Paper
        row Paper    Rock     Scissors
        row Scissors Paper    Rock
      else do
        rowA Rock     Lizard   Scissors Spock    Paper
        rowA Paper    Rock     Spock    Lizard   Scissors
        rowA Scissors Paper    Lizard   Rock     Spock
        rowA Lizard   Spock    Paper    Scissors Rock
        rowA Spock    Scissors Rock     Paper    Lizard

  where
    simple = isNothing pLizard

    toPattern ∷ Action → String
    toPattern Rock     = pRock
    toPattern Paper    = pPaper
    toPattern Scissors = pScissors
    toPattern Lizard   = fromMaybe "" pLizard
    toPattern Spock    = fromMaybe "" pSpock

    toRegex ∷ Action → Either String Regex
    toRegex a = regex (toPattern a) noFlags

    validRegex pat =
      case toRegex pat of
        Right _     → okay "Okay"
        Left errMsg → err  "Error" ! A.title errMsg

    entry res pat m =
      case toRegex pat of
        Right r →
          if test r (toPattern m) == toBool res
            then okay (name m)
            else err  (name m)
        _ → err (name m)

    row pattern match noMatch =
      H.tr $ do
        H.td $ M.text (name pattern)
        validRegex pattern
        entry Match pattern match
        entry NoMatch pattern noMatch

    rowA pattern m1 m2 n1 n2 =
      H.tr $ do
        H.td $ M.text (name pattern)
        validRegex pattern
        entry Match pattern m1
        entry Match pattern m2
        entry NoMatch pattern n1
        entry NoMatch pattern n2

rps ∷ ∀ eff. UI eff Markup
rps =
  renderTable <$> string "Pattern Rock:" ""
              <*> string "Pattern Paper:" ""
              <*> string "Pattern Scissors:" ""
              <*> pure Nothing
              <*> pure Nothing

rpslv ∷ ∀ eff. UI eff Markup
rpslv =
  renderTable <$> string "Pattern Rock:" ""
              <*> string "Pattern Paper:" ""
              <*> string "Pattern Scissors:" ""
              <*> (Just <$> string "Pattern Lizard:" "")
              <*> (Just <$> string "Pattern Spock" "")

main ∷ Eff (dom :: DOM, channel :: CHANNEL) Unit
main = do
  runFlareHTML "controls1" "output1" rps
  runFlareHTML "controls2" "output2" rpslv
