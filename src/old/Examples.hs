{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Examples where

import A
import Control.Monad
import Data.Function ((&))
import Text.Blaze
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html4.FrameSet.Attributes
  ( style,
  )
import qualified Text.Blaze.Html5 as Html

-- import V

appSize = 56

app ::
  (HasMiddle h, HasMiddle v) => Box ((h, v) -> (Constrained, Constrained))
app =
  relative $
    box
      & width appSize
      & height appSize
      & centerX (getCenterX container)
      & centerY (getCenterY container)

-- appRow :: Box ((Constrained, Constrained) -> [(Constrained, Constrained)])
-- appRow = spaceBetweenH (replicate 4 app)

iphone =
  let appContainer =
        relative $
          box
            & width (getWidth container - appSize)
            & centerX (getCenterX container)
            & top (appSize / 1.618 * 2)
            & height (appSize * 6 * 1.618)
   in appContainer

screen = box & left 0 & top 0 & width 375 & height 812

--       bottomContainer =
--         box
--           & height appSize
--           & width (getWidth screen - appSize)
--           & centerX (getCenterX screen)
--           & bottom (getHeight screen - (appSize / 1.618))
--       b = appRow bottomContainer
--       -- apps = do
--       --   row <- appRow appContainer
--       --   a <- spaceBetweenV (replicate 6 row) appContainer
--       --   pure a
--    in screen : appContainer : bottomContainer

-- instance ToMarkup (Vector Constrained, Vector Constrained) where
--   toMarkup (a, b) = Html.div ! style ("position: absolute; background: rgba(0, 0, 0, 0.1); width: " <> w <> "; height: " <> h <> "; transform: translate(" <> p <> ");") $ mempty
--     where
--       w = stringValue $ show (_size a) <> "px"
--       h = stringValue $ show (_size b) <> "px"
--       l = show (_start a) <> "px"
--       t = show (_start b) <> "px"
--       p = stringValue $ l <> ", " <> t

-- instance ToMarkup [(Vector Constrained, Vector Constrained)] where
--   toMarkup b = Html.div ! style "position: absolute; top: 0; left: 0; bottom: 0; right: 0;" $ Html.toHtml $ toMarkup <$> b

-- render :: IO ()
-- render = writeFile "rendered.html" $ renderHtml (toMarkup (eval <$> (iphone (box & left 0 & top 0 & width 375 & height 812))))

-- vw :: Fractional a => a -> a
-- vw px = (100 * px) / 1864
