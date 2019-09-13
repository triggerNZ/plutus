{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds                  #-}
module Types.API where

import           Servant.API                    ( (:<|>)
                                                , (:>)
                                                , Get
                                                , JSON
                                                , Post
                                                , ReqBody
                                                )
import           Types.Response                 ( Response )

type API = "notify" :> ReqBody '[JSON] Response :> Post '[JSON] ()
