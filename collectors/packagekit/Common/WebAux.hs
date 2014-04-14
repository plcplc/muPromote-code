-- | this module provides auxiliary functions for web data exchange
module MuPromote.Common.WebAux where

import Data.Aeson
import Data.Aeson.Encode(fromValue)
import Data.Text.Lazy.Builder as TL (toLazyText)
import Data.Text.Lazy as TL (toStrict)

encodeText :: ToJSON a => a -> Text
encodeText = TL.toStrict . TL.toLazyText . fromValue . toJSON

