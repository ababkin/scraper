{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative        ((<$>), (<*>))
import           Control.Lens               hiding ((.=))
import           Control.Monad              (forM, forM_)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
import           Data.List                  (intercalate)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Network.Wreq               (FormParam ((:=)), Response, asJSON,
                                             cookieValue, defaults, get, param,
                                             responseBody, responseCookie)
import qualified Network.Wreq.Session       as S
import           System.Environment         (getArgs)
import           Text.HTML.TagSoup

type Username = BS.ByteString
type Url = BS.ByteString
type Tags = BL.ByteString
type Page = (Username, Tags)
type KeyVal = (BS.ByteString, BS.ByteString)

formData = [
    "username" :=  ("workleg" :: String)
  , "password" :=  ("izmail" :: String)
  , "redirect" :=  ("memberlist.php?mode=viewprofile&u=36677" :: String)
  , "sid"      :=  ("e7447b401e15302414334a75d08cc548" :: String)
  , "login"    :=  ("Login" :: String)
  ]


main :: IO ()
main = S.withSession $ \sess -> do

  let opts = defaults & param "mode" .~ ["login"]
  pr <- S.postWith opts sess "http://www.top-law-schools.com/forums/ucp.php" formData

  [urlsFile] <- getArgs

  content <- BL.readFile urlsFile
  case decode NoHeader content :: Either String (Vector (Vector BS.ByteString)) of
    Right rows -> do
      pages <- forM (V.toList . V.map V.toList $ rows) $ \(url:username:_) -> do
        r <- S.get sess $ BS.unpack url
        return (username, r ^. responseBody)

      BL.writeFile "out.csv" $ encodeByName (V.fromList $ allHeaders pages) $ map ((flip Map.union $ fillRecord pages) . sparseRecord) pages
    Left err ->
      putStrLn $ "could not extract rows: " ++ show err



sparseRecord :: Page -> Map BS.ByteString BS.ByteString
sparseRecord (username, tags) = Map.insert "username" username . Map.fromList . extractFromPage $ tags

fillRecord :: [Page] -> Map BS.ByteString BS.ByteString
fillRecord = Map.fromList . map (,"") . allHeaders


allHeaders :: [Page] -> [BS.ByteString]
allHeaders = ("username":) . Map.keys . Map.fromList . concat . map (extractFromPage . snd)

extractFromPage :: Tags -> [KeyVal]
extractFromPage =
  map extractKV .
  sections (~== ("<tr>" :: String)) .
  takeWhile (~/= ("</table>" :: String)) .
  dropWhile (~/= ("<tr valign=top>" :: String)) .
  dropWhile (~/= ("<table>" :: String)) .
  dropWhile (~/= ("<table>" :: String)) .
  parseTags

extractKV :: [Tag BL.ByteString] -> KeyVal
extractKV s = (key, value)
  where
    key   = extract . V.dropWhile (~/= ("<b>"     :: String)) . V.fromList $ s
    value = extract . V.dropWhile (~/= ("<span>"  :: String)) . V.fromList $ s
    extract =  BL.toStrict . V.head . V.map fromTagText . V.filter isTagText . V.takeWhile (~/= ("</td>" :: String))


