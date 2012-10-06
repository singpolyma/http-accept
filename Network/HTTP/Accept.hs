module Network.HTTP.Accept (selectAcceptType) where

import Data.Char (isAscii)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Arrow (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (singleton, split)
import qualified Data.ByteString.Char8 as Char8

data Pattern a = PatternAny | PatternExactly a

instance (Eq a) => Eq (Pattern a) where
	PatternAny == _ = True
	_ == PatternAny = True
	(PatternExactly a) == (PatternExactly b) = a == b

-- | Select which Accept type to use
selectAcceptType ::
	[String]        -- ^ List of supported MIME types, in preferred order
	-> [ByteString] -- ^ List of types from Accept, pre-sorted with no q
	-> Maybe String -- ^ Just the selected supported type, or else Nothing
selectAcceptType supported accept =
	listToMaybe $ mapMaybe (`lookup` supported') accept'
	where
	accept' = map (Just . parseAccept) accept
	supported' = map (first $ fmap parseAccept . stringAscii)
		(zip supported supported)
	parseAccept s = let (t:sub:_) = BS.split 0x2f s in
		(parsePattern t, parsePattern sub)

parsePattern :: ByteString -> Pattern ByteString
parsePattern s
	| s == BS.singleton 0x2a = PatternAny
	| otherwise = PatternExactly s

-- | Safely convert an ASCII 'String' to 'ByteString'
stringAscii :: String -> Maybe ByteString
stringAscii s
	| all isAscii s = Just $ Char8.pack s
	| otherwise     = Nothing
