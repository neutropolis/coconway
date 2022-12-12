{-# LANGUAGE TemplateHaskell, LambdaCase #-}

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
import Parser
import Rle

data ConwayState = ConwayState { _generation :: Int
                               , _board :: Store Cell
                               , _rle :: Rle }
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

draw (ConwayState g (Store _ m) rle) = [center $ help <+> str " " <+> t2 <+> str " " <+> meta] where
  t = str . intercalate "\n" $ fmap (head . show) <$> toLists m
  t2 = joinBorders $
       withBorderStyle unicode $
       withAttr fgAttr $
       borderWithLabel (str "Conway's Game of Life") t
  meta = joinBorders $
         withBorderStyle unicode $
         withAttr mFgAttr $
         borderWithLabel (str "Meta") $
         str $ "generation: " ++ show g ++ "\n" ++
               "comment:    " ++ show (fromMaybe "" $ rleComment rle) ++ "\n" ++
               "name:       " ++ show (fromMaybe "" $ rleName rle)
  help = joinBorders $
         withBorderStyle unicode $
         borderWithLabel (str "Help") $
         str "Press <SPACE> to evolve\nPress <ESC>   to exit"

handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  do modify (over generation (+1))
     modify (over board step)
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt 
handleEvent _ = pure ()

fgAttr = attrName "fgAttr"
mFgAttr = attrName "mFgAttr"

instance Read Cell where
  readsPrec _ "o" = [(Live, "")]
  readsPrec _  _  = [(Dead, "")]

main :: IO ()
main = do
  let file = "pattern/gosper-glider-gun.rle"
  txt <- readFile file 
  let rle = parseRle txt
      (x, y) = fromMaybe (0, 0) $ rleOffset rle
      items = fmap (read . return) <$> rleUncompress rle
      m = fromLists items
      matrix = extendTo Dead (nrows m + x) (ncols m + y) $ fromLists items
      initialState = ConwayState 0 (Store (1, 1) matrix) rle
  finalState <- defaultMain app initialState
  putStrLn "Bye, bye!"

