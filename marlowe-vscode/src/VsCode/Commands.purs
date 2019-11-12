module VsCode.Commands where

import Prelude
import Effect (Effect)
import VsCode.Disposable (Disposable)

foreign import registerCommand :: String -> Effect Unit -> Effect Disposable
