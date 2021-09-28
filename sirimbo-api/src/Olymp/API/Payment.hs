{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Payment
  ( QrPaymentAPI
  , qrPaymentAPI
  , attr
  , toIban
  , mkImage
  , mkSpayd
  , checksum
  ) where

import Codec.Picture (Image, Pixel8)
import Codec.QRCode
import Codec.QRCode.JuicyPixels (toImage)
import Control.Applicative ((<|>), liftA2)
import Control.Effect (Embed, embed, Eff)
import Data.Bifunctor (Bifunctor(first))
import Data.Binary.Builder (Builder, fromLazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (elemIndex, intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits (symbolVal, KnownSymbol)
import Servant
import Servant.JuicyPixels (PNG)
import Control.Effect.Error (Throw, throw)


type QrPaymentAPI
  = "qr-payment.png"
  :> QueryParam "acc" (Tagged (Proxy "ACC") Text)
  :> QueryParam "am" (Tagged (Proxy "AM") Text)
  :> QueryParam "msg" (Tagged (Proxy "MSG") Text)
  :> QueryParam "ss" (Tagged (Proxy "X-SS") Text)
  :> QueryParam "vs" (Tagged (Proxy "X-VS") Text)
  :> QueryParam "ks" (Tagged (Proxy "X-KS") Text)
  :> Get '[PNG] (Image Pixel8)

qrPaymentAPI :: (Eff (Throw ServerError) m, Eff (Embed IO) m) => ServerT QrPaymentAPI m
qrPaymentAPI (attr . toIban -> acc) (attr -> am) (attr -> msg) (attr -> ss) (attr -> vs) (attr -> ks) = do
  let spayd = mkSpayd [acc, am, msg, ss, vs, ks]
  embed $ print spayd
  maybe (throw err404 { errBody = "Invalid QR payment" }) pure (mkImage spayd)

mkSpayd :: [Builder] -> Text
mkSpayd attrs = T.decodeUtf8 . BC.toStrict . toLazyByteString . mconcat $
  intersperse "*" (["SPD", "1.0", "CC:CZK"] ++ attrs)

mkImage :: Text -> Maybe (Image Pixel8)
mkImage spayd =
  toImage 4 6 <$> encodeText (defaultQRCodeOptions M) Iso8859_1OrUtf8WithoutECI spayd

attr :: forall s. KnownSymbol s => Maybe (Tagged (Proxy s) Text) -> Builder
attr Nothing = ""
attr (Just (Tagged s)) = fromLazyByteString (BC.pack (symbolVal (Proxy @s))) <> ":" <> toEncodedUrlPiece s

toIban :: Maybe (Tagged (Proxy "ACC") Text) -> Maybe (Tagged (Proxy "ACC") Text)
toIban Nothing = Nothing
toIban (Just (Tagged t)) =
  let
    (x, drop 1 . T.unpack -> bank) = T.breakOn "/" t
    (reverse . T.unpack -> acc, reverse . drop 1 . T.unpack -> pref) = T.breakOn "-" (T.reverse x)
    bban = mconcat
      [ replicate (4 - length bank) '0' <> bank
      , replicate (6 - length pref) '0' <> pref
      , replicate (10 - length acc) '0' <> acc
      ]
  in case checksum "CZ" bban of
    Left _ -> Nothing
    Right (show -> ch) ->
      Just . Tagged . T.pack $ mconcat
        [ "CZ"
        , replicate (2 - length ch) '0' <> ch
        , bban
        ]

checksum :: String -> String -> Either String Int
checksum country bban =
   (98-) . divide 97 <$>
      mapM intFromAlphaNum bban +++
      mapM intFromAlpha country +++ pure [(100, 0)]

(+++) :: (Applicative f) => f [a] -> f [a] -> f [a]
(+++) = liftA2 (++)

divide :: Int -> [(Int,Int)] -> Int
divide divisor = foldl (\r (base,x) -> mod (base*r+x) divisor) 0

intFromDigit :: Char -> Either String (Int,Int)
intFromDigit c =
  case elemIndex c ['0'..'9'] of
    Just ix -> Right (10, ix)
    Nothing -> Left ("not a digit: " <> [c])

intFromAlpha :: Char -> Either String (Int,Int)
intFromAlpha c =
  case elemIndex c ['a'..'z'] <|> elemIndex c ['A'..'Z'] of
    Just ix -> Right (100, ix + 10)
    Nothing -> Left ("not a letter: " <> [c])

intFromAlphaNum :: Char -> Either String (Int,Int)
intFromAlphaNum c =
  first (const $ "invalid alphanumeric character: " ++ [c]) $
  intFromDigit c <|> intFromAlpha c
