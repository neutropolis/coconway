{-# LANGUAGE TemplateHaskell #-}

import Data.Functor
import Data.List
import Data.Matrix
import Data.Maybe

import Control.Lens hiding ( zoom )

import Brick
import Brick.AttrMap
import Brick.Main
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Table
import Graphics.Vty as V

import Conway

data ConwayState = ConwayState { _generation :: Int
                               , _board :: Store Cell }
makeLenses ''ConwayState

app :: App ConwayState () ()
app = App { appDraw         = draw
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent  = handleEvent
          , appStartEvent   = pure ()
          , appAttrMap      = const $ attrMap defAttr [(fgAttr, fg V.green)
                                                      ,(mFgAttr, fg V.red)] }

instance Show Cell where
  show Live = "▮"
  show Dead = "·"

draw (ConwayState g (Store _ m)) = [center $ help <+> str " " <+> t2 <+> str " " <+> meta] where
  t = str . intercalate "\n" $ fmap (head . show) <$> toLists m
  t2 = joinBorders $
       withBorderStyle unicode $
       withAttr fgAttr $
       borderWithLabel (str "Conway's Game of Life") t
  meta = joinBorders $
         withBorderStyle unicode $
         withAttr mFgAttr $
         borderWithLabel (str "Meta") $ str $ "generation: " ++ show g
  help = joinBorders $
         withBorderStyle unicode $
         borderWithLabel (str "Help") $ str "Press <SPACE> to evolve\nPress <ESC>   to exit"

handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  do modify (over generation (+1))
     modify (over board step)
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt 
handleEvent _ = pure ()

fgAttr = attrName "fgAttr"
mFgAttr = attrName "mFgAttr"

rle = fromMaybe [] $ parseRle "27b2o$27bobo$29bo4b2o$25b4ob2o2bo2bo$25bo2bo3bobob2o$28bobobobo$29b2obobo$33b2o$19b2o$20bo8bo$20bobo5b2o$21b2o$35bo$36bo$34b3o2b$25bo$25b2o$24bobo4b2o22bo$31bo21b3o$32b3o17bo$34bo17b2o2b$45bo$46b2o12b2o$45b2o14bo$3b2o56bob2o$4bo9b2o37bo5b3o2bo$2bo10bobo37b2o3bo3b2o$2b5o8bo5b2o35b2obo$7bo13bo22b2o15bo$4b3o12bobo21bobo12b3o$3bo15b2o22bo13bo$3bob2o35b2o5bo8b5o$b2o3bo3b2o37bobo10bo$o2b3o5bo37b2o9bo$2obo56b2o$3bo14b2o$3b2o12b2o$19bo2b$11b2o17bo$12bo17b3o$9b3o21bo$9bo22b2o4bobo$38b2o$39bo2b$28b3o$28bo$29bo$42b2o$35b2o5bobo$35bo8bo$44b2o2b$31bo$30bobob2o$30bobobobo$27b2obobo3bo2bo$27bo2bo2b2ob4o$29b2o4bo$35bobo$36b2o!"

main :: IO ()
main = do
  let initialState = ConwayState 0 $ Store (1, 1) (matrix 80 100 $ lookups rle)
  finalState <- defaultMain app initialState
  putStrLn "Bye, bye!"

