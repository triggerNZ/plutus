{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds                  #-}
module Marlowe.Symbolic.Types.API where

import           Servant.API                    ( (:<|>)
                                                , (:>)
                                                , Get
                                                , JSON
                                                , Post
                                                , ReqBody
                                                , NoContent
                                                )
import           Marlowe.Symbolic.Types.Response                 ( Response )

type API = "notify" :> ReqBody '[JSON] Response :> Post '[JSON] NoContent
