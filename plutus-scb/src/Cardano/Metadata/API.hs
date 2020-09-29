{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Metadata.API
    ( API
    ) where

import           Cardano.Metadata.Types (PropertyDescription, PropertyKey, Subject, SubjectProperties)
import           Servant.API            ((:<|>), (:>), Capture, Get, JSON)

type API
     = "metadata" :> Capture "subject" Subject :> ("properties" :> Get '[ JSON] SubjectProperties
                                                   :<|> "property" :> Capture "property" PropertyKey :> Get '[ JSON] PropertyDescription)
