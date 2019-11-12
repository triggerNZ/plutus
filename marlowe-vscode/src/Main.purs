module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import VsCode.Commands (registerCommand)
import VsCode.ExtensionContext (ExtensionContext, subscriptions)
import VsCode.ExtensionContext.Subscriptions (push)
import VsCode.Window (showInformationMessage)

activateEff :: ExtensionContext -> Effect Unit
activateEff ctx =
  void do
    log "PureScript extension activated"
    disposable <-
      registerCommand "extension.helloWorld"
        $ showInformationMessage "Hello World!!!"
    ss <- subscriptions ctx
    push ss disposable

activate :: ExtensionContext -> Unit
activate ctx = unsafePerformEffect $ activateEff ctx

deactivate :: Effect Unit
deactivate = do
  log "PureScript extension deactivated"
