{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Utils (CallError(..), callAndRetrieveBody, callWithoutBody, mapWithIndexMaybe, mapWithIndex, asRequestBody, onBlur, onEnterKeyHit, deleteIdentifiableFromId, toSystemError, onAnimationEnd) where

import Prelude hiding (id)

import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Vector ((!))
import Data.Either.Combinators (mapLeft)
import Control.Exception (SomeException)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Either (EitherT, runEitherT, left, right, newEitherT, handleEitherT, hoistMaybe)

import qualified Data.Aeson as Aeson (FromJSON, ToJSON, withObject, withArray, eitherDecode', encode)
import Data.Aeson ((.:))

import Model
import SharadFrontend.System

import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (Attribute, on)
import Miso.Event.Decoder (Decoder(..), DecodeTarget(..), emptyDecoder, valueDecoder, keycodeDecoder)
import Miso.Event.Types (KeyCode(..))

import JavaScript.Web.XMLHttpRequest (xhrByteString, Request(..), RequestData(..), Response(..), contents)

toSystemError :: CallError -> SystemEvent
toSystemError (NetworkError e) = ErrorHappened (show e)
toSystemError EmptyBody = ErrorHappened "Unexpected empty body in server's response"
toSystemError BadResponseCode = ErrorHappened "Unexpected error code in server's response"
toSystemError (BodyDecodingError s) = ErrorHappened ("Error while deserializing the body's response: " ++ s)

callAndRetrieveBody :: Aeson.FromJSON a => Request -> EitherT CallError IO a
callAndRetrieveBody req = do 
  resp <- call req
  body <- hoistMaybe EmptyBody (contents resp)
  newEitherT $ return $ mapLeft BodyDecodingError (eitherDecode' body)

callWithoutBody :: Request -> MaybeT IO CallError
callWithoutBody req = leftToMaybeT (call req)

leftToMaybeT :: EitherT e IO a -> MaybeT IO e
leftToMaybeT e = MaybeT (do
  res <- runEitherT e
  return $ either Just (const Nothing) res)

call :: Request -> EitherT CallError IO (Response ByteString)
call req = do
  resp <- handleEitherT NetworkError (xhrByteString req)
  if status resp /= 200 then left BadResponseCode else right resp

data CallError = NetworkError SomeException
               | EmptyBody
               | BadResponseCode
               | BodyDecodingError String

eitherDecode' :: Aeson.FromJSON fromJSON => ByteString -> Either String fromJSON
eitherDecode' = Aeson.eitherDecode' . fromStrict

mapWithIndexMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
mapWithIndexMaybe f l = fst fold_
    where
        fold_ = foldr accumulatingFunction ([], length l - 1) l 
        accumulatingFunction currentItem (accumulatingList, currentIdx) = (newAccumulatingList, currentIdx - 1)
            where newAccumulatingList = let newRes = f currentIdx currentItem in
                      case newRes of
                          Just res -> res : accumulatingList
                          Nothing -> accumulatingList

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f l = mapWithIndexMaybe (\i a -> Just (f i a)) l
 
asRequestBody :: Aeson.ToJSON requestObj => requestObj -> RequestData
asRequestBody = StringData . ms . toString . toStrict . Aeson.encode

onBlur :: (MisoString -> action) -> Attribute action
onBlur = on "blur" valueDecoder

onEnterKeyHit :: (String -> a) -> Attribute (Either SystemEvent a)
onEnterKeyHit toAction = on "keyup" onEnterInputDecoder (\maybeMisoStr -> maybe (Left NoEffect) (Right . toAction . fromMisoString) maybeMisoStr)

onEnterInputDecoder :: Decoder (Maybe MisoString)
onEnterInputDecoder = Decoder { decodeAt = DecodeTargets [["target"], []]
                              , decoder = Aeson.withArray "event" (\array -> do
                                   value <- Aeson.withObject "target" (\o -> o .: "value") (array ! 0)
                                   KeyCode keyCode <- decoder keycodeDecoder (array ! 1)
                                   if keyCode == enterKeyCode then return $ Just value else return Nothing)
                               }
                               where enterKeyCode = 13 

deleteIdentifiableFromId :: Content a => [Identifiable a] -> String -> [Identifiable a]
deleteIdentifiableFromId [] noteId = [] -- TODO propagate error not finding item to delete
deleteIdentifiableFromId (currentItem@Identifiable { storageId = StorageId { id = currentId }}:rest) itemId =
  if currentId == itemId
    then rest
    else currentItem : deleteIdentifiableFromId rest itemId

onAnimationEnd :: action -> Attribute action
onAnimationEnd action = on "animationend" emptyDecoder (\() -> action)

