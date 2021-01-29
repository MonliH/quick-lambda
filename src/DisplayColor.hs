module DisplayColor where
import           Data.Maybe                     ( fromJust )

data Font = Bold | Reset | Red | Green | Blue deriving Eq
table =
  [ (Bold , "\ESC[1m")
  , (Reset, "\ESC[0m")
  , (Blue , "\ESC[94m")
  , (Red  , "\ESC[91m")
  , (Green, "\ESC[92m")
  ]

getEscape font = fromJust $ lookup font table

displayColor :: [Font] -> String -> String
displayColor font text =
  foldl1 (++) (map getEscape font) ++ text ++ getEscape Reset
