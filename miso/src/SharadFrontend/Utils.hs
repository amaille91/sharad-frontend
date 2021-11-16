{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Utils (callAndRetrieveBody, mapWithIndexMaybe, mapWithIndex, asRequestBody, onBlur, onEnterKeyHit) where

import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Vector ((!))

import qualified Data.Aeson as Aeson (FromJSON, ToJSON, withObject, withArray, decode, encode)
import Data.Aeson ((.:))

import SharadFrontend.System

import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (Attribute, on)
import Miso.Event.Decoder (Decoder(..), DecodeTarget(..), valueDecoder, keycodeDecoder)
import Miso.Event.Types (KeyCode(..))

import JavaScript.Web.XMLHttpRequest (xhrByteString, Request(..), RequestData(..), contents)

callAndRetrieveBody :: Aeson.FromJSON a => Request -> IO (Maybe a)
callAndRetrieveBody req = do 
  maybeBS <- fmap contents $ xhrByteString req
  case maybeBS of
    Nothing -> return Nothing
    Just bs -> return $ decode bs

decode :: Aeson.FromJSON fromJson => ByteString -> Maybe fromJson
decode = Aeson.decode . fromStrict

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

