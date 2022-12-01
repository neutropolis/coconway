import Data.Functor
import Data.Matrix
import Data.Maybe

import Brick
import Brick.AttrMap
import Brick.Main
import Brick.Util
import Brick.Widgets.Core
import Graphics.Vty.Attributes (defAttr)

import Conway

rle = (fromMaybe [] $ parseRle $ 
        "24bo11b$22bobo11b$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o14b$" ++
        "2o8bo3bob2o4bobo11b$10bo5bo7bo11b$11bo3bo20b$12b2o!")

app :: App (Store Cell) e ()
app = App { appDraw         = (\_ -> [(str "Hello," <=> str "World!")])
          , appChooseCursor = (\_ _ -> Nothing)
          , appHandleEvent  = (\_ -> pure ())
          , appStartEvent   = pure ()
          , appAttrMap      = const $ attrMap defAttr [] }

main :: IO ()
main = do
  let initialState = Store (1, 1) (matrix 61 60 $ lookups rle)
  finalState <- defaultMain app initialState
  putStrLn "Bye, bye!"

-- main :: IO ()
-- main = void $ traverse print $ fmap peek $ take 200 $ iterate step board where
--   rle = fromMaybe [] $ parseRle $ 
--     "24bo11b$22bobo11b$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o14b$" ++
--       "2o8bo3bob2o4bobo11b$10bo5bo7bo11b$11bo3bo20b$12b2o!"
--   board = Store (1, 1) (matrix 61 60 $ lookups rle)

