{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Metadata.Client
    ( handleMetadataClient
    ) where

import           Cardano.Metadata.API      (API)
import           Cardano.Metadata.Types    (MetadataEffect (GetProperties, GetProperty),
                                            MetadataError (MetadataClientError, PropertyNotFound, SubjectNotFound))
import           Control.Monad.Freer       (Eff, LastMember, Member, type (~>), interpret, sendM)
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Proxy                (Proxy (Proxy))
import           Network.HTTP.Types        (status404)
import           Servant.Client            (ClientEnv, ClientError (FailureResponse), ClientM, client,
                                            responseStatusCode, runClientM)
import           Servant.Extra             (left, right)

handleMetadataClient ::
       forall m effs.
       (LastMember m effs, MonadIO m, Member (Error MetadataError) effs)
    => ClientEnv
    -> Eff (MetadataEffect ': effs) ~> Eff effs
handleMetadataClient clientEnv =
    let (getProperties, getProperty) = (_getProperties, _getProperty)
          where
            _getProperties = left . api
            _getProperty = right . api
            api = client (Proxy @API)

        handleError :: MetadataError -> ClientError -> MetadataError
        handleError onNotFound (FailureResponse _ response)
            | responseStatusCode response == status404 = onNotFound
        handleError _ err = (MetadataClientError err)

        runClient :: forall a. MetadataError -> ClientM a -> Eff effs a
        runClient onNotFound a =
            (sendM $ liftIO $ runClientM a clientEnv) >>=
            either (throwError . handleError onNotFound) pure
     in interpret $ \case
            GetProperties subject ->
                runClient (SubjectNotFound subject) (getProperties subject)
            GetProperty subject propertyKey ->
                runClient
                    (PropertyNotFound subject propertyKey)
                    (getProperty subject propertyKey)
