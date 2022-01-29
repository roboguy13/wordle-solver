{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ppr
  (Color(..)
  ,ColorfulString

  ,defaultColor
  ,greenColor
  ,yellowColor

  ,renderColorfulString

  ,Ppr(..)
  )
  where

data Color = DefaultColor | Green | Yellow

newtype ColorfulString = ColorfulString [(String, Color)]
  deriving (Semigroup, Monoid)

withColor :: String -> Color -> ColorfulString
withColor str color = ColorfulString [(str, color)]

defaultColor :: String -> ColorfulString
defaultColor = (`withColor` DefaultColor)

greenColor :: String -> ColorfulString
greenColor = (`withColor` Green)

yellowColor :: String -> ColorfulString
yellowColor = (`withColor` Yellow)

resetColor :: String
resetColor = "\ESC[0m"

setBgColor :: Color -> String
setBgColor DefaultColor = resetColor
setBgColor Green = "\ESC[42m"
setBgColor Yellow = "\ESC[43m"

setBlackText :: String
setBlackText = "\ESC[30m"

renderWithColor :: String -> Color -> String
renderWithColor str DefaultColor = str
renderWithColor str color = setBgColor color <> setBlackText <> str <> resetColor

renderColorfulString :: ColorfulString -> String
renderColorfulString (ColorfulString chunks) = concatMap (uncurry renderWithColor) chunks

class Ppr a where
  ppr :: a -> String

