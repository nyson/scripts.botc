module Main
  ( main
  )
  where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Console (log)
import Pages.Head (startingPage)
import Pages.Head as Head

main ∷ Effect Unit
main = do
    log "starting..."
    runWidgetInDom "main"
        $ Head.page startingPage
