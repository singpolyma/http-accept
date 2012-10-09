module Network.HTTP.Accept (selectAcceptType) where

import Data.Char (isAscii)
import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad (liftM2)
import Control.Arrow (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (singleton, split)
import qualified Data.ByteString.Char8 as Char8

data Pattern a = PatternAny | PatternExactly a

class Match a where
	match :: a -> a -> Maybe Int

instance (Eq a) => Match (Pattern a) where
	match PatternAny _ = Just 0
	match _ PatternAny = Just 0
	match (PatternExactly a) (PatternExactly b)
		| a == b = Just 1
		| otherwise = Nothing

instance (Match a, Match b) => Match (a, b) where
	match (a1, a2) (b1, b2) = liftM2 (+) (match a1 b1) (match a2 b2)

instance (Match a) => Match (Maybe a) where
	match Nothing Nothing   = Just 0
	match Nothing _         = Nothing
	match _       Nothing   = Nothing
	match (Just a) (Just b) = fmap (+1) (match a b)

maxMatchIndex :: (Match a) => a -> [a] -> Maybe Int
maxMatchIndex k xs = fmap fst $ maybeMax (comparing snd) $ catMaybes $
	zipWith (\i x -> fmap ((,)i) (match k x)) [0..] xs

maybeMax :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMax _ [] = Nothing
maybeMax cmp xs = Just (maximumBy cmp xs)

maybeMin :: (a -> a -> Ordering) -> [a] -> Maybe a
maybeMin _ [] = Nothing
maybeMin cmp xs = Just (minimumBy cmp xs)

-- | Select which Accept type to use
selectAcceptType ::
	[String]        -- ^ List of supported MIME types, in preferred order
	-> [ByteString] -- ^ List of types from Accept, pre-sorted with no q
	-> Maybe String -- ^ Just the selected supported type, or else Nothing
selectAcceptType supported accept = fmap fst $ maybeMin (comparing snd) $
	mapMaybe (\(p,s) -> fmap ((,)s) (maxMatchIndex p accept')) supported'
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
