{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module SharadFrontend.Crud where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Either (EitherT)

import Model
import SharadFrontend.Utils

import Miso.String (ms)

import JavaScript.Web.XMLHttpRequest (xhrByteString, Request(..), Response(..), Method(..), RequestData(..))

class Content contentType => CrudType token contentType | token -> contentType where
  getRequest :: token -> Request
  getRequest token = Request { reqMethod = GET
                          , reqURI = (ms . getEndpoint) token
                          , reqLogin = Nothing
                          , reqHeaders = []
                          , reqWithCredentials = False
                          , reqData = NoData
                          }

  deleteRequest :: token -> String -> Request
  deleteRequest token idToDelete = Request { reqMethod = DELETE
                                       , reqURI = ms $ getEndpoint token ++ "/" ++ idToDelete
                                       , reqLogin = Nothing
                                       , reqHeaders = []
                                       , reqWithCredentials = False
                                       , reqData = NoData
                                       }

  postRequest :: token -> contentType -> Request
  postRequest token content = Request { reqMethod = POST
                                  , reqURI = (ms . getEndpoint) token
                                  , reqLogin = Nothing
                                  , reqHeaders = []
                                  , reqWithCredentials = False
                                  , reqData = asRequestBody content
                                  }

  putRequest :: token -> Identifiable contentType -> Request
  putRequest token update = Request { reqMethod = PUT
                                , reqURI = (ms . getEndpoint) token
                                , reqLogin = Nothing
                                , reqHeaders = []
                                , reqWithCredentials = False
                                , reqData = asRequestBody update
                                }

  getEndpoint :: token -> String

  callGet :: token -> EitherT CallError IO [Identifiable contentType]
  callGet token = callAndRetrieveBody (getRequest token)

  callPost :: token -> contentType -> EitherT CallError IO StorageId
  callPost token newContent = callAndRetrieveBody (postRequest token newContent)

  callPut :: token -> Identifiable contentType -> EitherT CallError IO StorageId
  callPut token update = callAndRetrieveBody (putRequest token update)

  callDelete :: token -> String -> MaybeT IO CallError
  callDelete token idToDelete = callWithoutBody (deleteRequest token idToDelete)

