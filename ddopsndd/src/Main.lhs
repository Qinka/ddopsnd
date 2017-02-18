\begin{code}
module Main where

import Data.Conduit
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Time
import Text.Parsec
import Yesod.Core

import qualified Data.HashMap       as HM
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
\end{code}


\begin{code}
main = warp 3000 Ddopsnd
\end{code}


for yesod
\begin{code}
data Ddopsnd = Ddopsnd

mkYesod "Ddopsnd" [parseRoutes|
  /record RecordR POST
  /ipaddr IpaddrR POST
  |]

instance Yesod Ddopsnd
\end{code}

to get record id
\begin{code}
postRecordR :: Handler T.Text
postRecordR = do -- Handler
  sub  <- lookupPostParam "sub-domain"
  json <- getFile "context"
  ver  <- toType <$> lookupPostParam "version"
  return $ case getRecord sub ver json of
    Nothing -> "notfound"
    Just x  -> T.unwords x
  where toType (Just "4") = Just "A"
        toType (Just "6") = Just "AAAA"
        toType _          = Nothing
\end{code}


get the record id
\begin{code}
type SubDomain  = T.Text
type RecordId   = T.Text
type RecordType = T.Text
data Record = Record
              { sub_domain  :: SubDomain
              , record_id   :: RecordId
              , record_type :: RecordType
              , item_value  :: T.Text
              }
            deriving (Eq,Show)
newtype Records = Records [Record]
                deriving (Eq,Show)
instance FromJSON Record where
  parseJSON (Object v) = Record
    <$> v .: "name"
    <*> v .: "id"
    <*> v .: "type"
    <*> v .: "value"
instance FromJSON Records where
  parseJSON (Object v) = Records
    <$> v .: "records"

getRecord :: Maybe T.Text -> Maybe T.Text -> Maybe B.ByteString -> Maybe [T.Text]
getRecord (Just sd) (Just typ) b = do -- Maybe
  Records rs <- decodeStrict =<< b
  let rc = filter (\r -> sub_domain r == sd && record_type r == typ) rs
      gets Record{..} = [record_id,item_value]
  gets <$> listToMaybe rc
getRecord _ _ _ = Nothing
\end{code}



to get ip addr
\begin{code}
postIpaddrR :: Handler String
postIpaddrR = do -- Handler
  txt <- getFile "context"
  ver <- lookupPostParam "version"
  return $ getIP txt ver
  where getIP (Just txt) (Just "4") =
          case ipv4s $ parsing txt of
            [] -> "notfound"
            rs -> unwords $ "4":rs
        getIP (Just txt) (Just "6") = 
          case ipv6s $ parsing txt of
            [] -> "notfound"
            rs -> unwords $ "6":rs
        getIP _ _ = "notfound"
        
  
  
\end{code}

\begin{code}
data IP = IPv4    String
        | IPv6    String
        | IPvNull String
        deriving (Eq,Show)
ipv4s, ipv6s :: [Either ParseError IP] -> [String]
ipv4s [] = []
ipv4s (Right (IPv4 x):xs) = x : ipv4s xs
ipv4s (_:xs) = ipv4s xs
ipv6s [] = []
ipv6s (Right (IPv6 x):xs) = x : ipv6s xs
ipv6s (_:xs) = ipv6s xs
type Parser = Parsec T.Text ()
parsingIpx :: String -> Parser Char -> Parser String
parsingIpx i select = do
  spaces
  string i
  skipMany space
  optional $ string "addr"
  spaces
  optional $ string ":"
  spaces
  rs <- many1 select
  skipMany (noneOf "\n\r")
  return rs
parsingIpv4, parsingIpv6, parsingIpvNull :: Parser IP
parsingIpv4 = let select = oneOf "0123456789."              in IPv4 <$> parsingIpx "inet"  select
parsingIpv6 = let select = oneOf "0123456789:aAbBcCdDeEfF"  in IPv6 <$> parsingIpx "inet6" select
parsingIpvNull = do
  IPvNull <$> many (noneOf "\n\r")
parsingIP :: Parser IP
parsingIP = try parsingIpv6 <|> try parsingIpv4 <|> parsingIpvNull
parsing :: B.ByteString -> [Either ParseError IP]
parsing bs = run <$> ts
  where ts = T.lines $ T.decodeUtf8 bs
        run = runParser parsingIP () "context"
\end{code}


\begin{code}
getFile :: T.Text -> Handler (Maybe B.ByteString)
getFile fn = do
  file <- lookupFile fn
  case file of
    Just fd -> Just . B.concat <$> sourceToList (fileSource fd)
    Nothing -> do
      field <- lookupPostParam fn
      case field of
        Just fd -> return $ Just $  T.encodeUtf8 fd
        Nothing -> return Nothing
\end{code}
