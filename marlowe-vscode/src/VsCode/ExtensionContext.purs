module VsCode.ExtensionContext where

import Effect (Effect)
import VsCode.ExtensionContext.Subscriptions (Subscriptions)

foreign import data ExtensionContext :: Type

foreign import subscriptions :: ExtensionContext -> Effect Subscriptions
