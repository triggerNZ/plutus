module VsCode.ExtensionContext.Subscriptions where

import Effect (Effect)
import VsCode.Disposable (Disposable)

foreign import data Subscriptions :: Type

foreign import push :: Subscriptions -> Disposable -> Effect Subscriptions
