module Qafny.Syntax.Render(renderLazy, renderIO, putDoc, hPutDoc) where

import qualified Data.Text                       as TS
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Builder          as TLB
import           Prettyprinter
import           Prettyprinter.Internal
import qualified Prettyprinter.Render.Text       as P
import           Prettyprinter.Render.Util.Panic
    (panicUncaughtFail)
import           System.IO

renderLazy :: Bool -> SimpleDocStream TS.Text -> TL.Text
renderLazy debug =
  if debug then P.renderLazy else renderLazyRelease

-- | Render the DocStream and prefer the debugging warning annotations.
renderLazyRelease :: SimpleDocStream TS.Text -> TL.Text
renderLazyRelease = TLB.toLazyText . go
  where
    go x = case x of
        SFail              -> panicUncaughtFail
        SEmpty             -> mempty
        SChar c rest       -> TLB.singleton c <> go rest
        SText _l t rest    -> TLB.fromText t <> go rest
        SLine i rest       -> TLB.singleton '\n' <> (TLB.fromText (textSpaces i) <> go rest)
        SAnnPush ann _     -> TLB.fromText ann
        SAnnPop rest       -> go rest


renderIO :: Bool -> Handle -> SimpleDocStream TS.Text -> IO ()
renderIO debug h = go
  where
    go :: SimpleDocStream TS.Text -> IO ()
    go = \sds -> case sds of
        SFail              -> panicUncaughtFail
        SEmpty             -> pure ()
        SChar c rest       -> do hPutChar h c
                                 go rest
        SText _ t rest     -> do T.hPutStr h t
                                 go rest
        SLine n rest       -> do hPutChar h '\n'
                                 T.hPutStr h (textSpaces n)
                                 go rest
        SAnnPush ann rest -> if debug then go rest else T.hPutStr h ann
        SAnnPop rest       -> go rest

putDoc :: Bool -> Doc TS.Text -> IO ()
putDoc debug = hPutDoc debug stdout

hPutDoc :: Bool -> Handle -> Doc TS.Text -> IO ()
hPutDoc debug h doc = renderIO debug h (layoutPretty defaultLayoutOptions doc)
