\begin{code}
module Main where


import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PSS
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Time


import           Network.HTTP    ()
import qualified Network.HTTP as HTTP
import           Yesod.Core hiding ()
import qualified Yesod.Core as YC


import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap           as HM
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as B
\end{code}

for yesod
\begin{code}
data Ddopsnd = Ddopsnd
               { loginToken :: String
               , publicDir  :: String
               }
             deriving (Eq,Show)

mkYesod "Ddopsnd" [parseRoutes|
  / UpdateR POST
  |]

instance Yesod Ddopsnd where
  isAuthorized _ _ = do
    pubKey     <- getPubKey
    checkHash  <- (B64.decode . T.encodeUtf8 .T.concat)         <$> lookupPostParams "sha-text"
    checkTime  <- T.concat                                      <$> lookupPostParams "time"
    checkDelta' <- (readD . T.concat) <$> lookupPostParams "delta"
    let checkDelta = fromRational $ toRational checkDelta'
    case (pubKey,checkHash,checkTime,checkDelta) of
      (Nothing,_,_,_) -> return $ Unauthorized "Who are you? My frend!"
      (_,Left _,_,_)  -> return $ Unauthorized "Who are you? My frien!"
      (Just pk,Right ch,ct,cd)  -> do
        let time = readT ct
        isTime <- checkTimeLim time cd
        if isTime
          then do
          isText <- checkText ct pk ch checkDelta'
          if isText
            then do
            $logDebugS "Auth" "Auth one"
            return Authorized
            else do
            liftIO $ threadDelay 60
            return $ Unauthorized "Who are you? The thing did have answer...."
          else do
          liftIO $ threadDelay 60
            return $ Unauthorized "Who are you? The thing did not answer...."
          
      where getPubKey = do
              fileDir <- publicDir <$> getYesod
              shaFilePath <- lookupPostParam "sha-file-name"
              case shaFilePath of
                Just sfp -> do
                  pKey <- read <$> (liftIO $ readFile (fileDir ++ T.unpack sfp))
                  return (Just pKey)
                Nothing -> return Nothing
            checkText :: T.Text -> PublicKey -> B.ByteString -> Double -> Handler Bool
            checkText text pk hash dl = do
              let test = T.encodeUtf8 text
              return $ verify sha512pss pk (B.concat [test,showBSUtf8 dl]) hash
            checkTimeLim :: UTCTime -> NominalDiffTime -> Handler Bool
            checkTimeLim time cd = do
              now <- liftIO getCurrentTime
              if abs (diffUTCTime now time) < cd
                then return True
                else return False
            readD = readT :: T.Text -> Double --} \_ -> 1000 :: Double
            sha512pss = defaultPSSParams SHA512 :: PSSParams SHA512 B.ByteString B.ByteString


\end{code}




\begin{code}
postUpdateR :: Handler String
postUpdateR = do
  sub <- lookupPostParam "sub-domain"
  dom <- lookupPostParam "domain"
  val <- lookupPostParam "value"
  case (sub,dom,val) of
    (Nothing,_,_) -> invalidArgs ["rgs"]
    (_,Nothing,_) -> invalidArgs ["ags"]
    (_,_,Nothing) -> invalidArgs ["ars"]
    (s,d,v) -> do
      ri <- getRecordId s d
      lT <- loginToken <$> getYesod
      let paramStr = mkParam
            [ "login_token" =: lT
            , "format"      =: "json"
            , "lang"        =: "cn"
            , "domain"      =: d
            , "record_id"   =: ri
            , "sub_domain"  =: s
            , "record_type" =: "A"
            , "record_line" =: "默认"
            , "value"       =: v
            ]
          request = HTTP.postRequestWithBody "https://dnsapi.cn/Record.Modify"
                                             "application/x-www-form-urlencoded"
                                             paramStr
      simpleHTTP request >>= getResponseBody
\end{code}

\begin{code}
--auth :: Route Ddopsnd -> Bool -> Handler Bool
auth _ _ = do
  pubKey     <- getPubKey
  checkHash  <- (B64.decode . T.encodeUtf8 .T.concat)         <$> lookupPostParams "sha-text"
  checkTime  <- T.concat                                      <$> lookupPostParams "time"
  checkDelta' <- (readD . T.concat) <$> lookupPostParams "delta"
  let checkDelta = fromRational $ toRational checkDelta'
  case (pubKey,checkHash,checkTime,checkDelta) of
    (Nothing,_,_,_) -> return $ Unauthorized "Who are you? My frend!"
    (_,Left _,_,_)  -> return $ Unauthorized "Who are you? My frien!"
    (Just pk,Right ch,ct,cd)  -> do
      let time = readT ct
      isTime <- checkTimeLim time cd
      if isTime
        then do
        isText <- checkText ct pk ch checkDelta'
        if isText
          then do
          $logDebugS "Auth" "Auth one"
          return Authorized
          else do
          liftIO $ threadDelay 60
          return $ Unauthorized "Who are you? The thing did have answer...."
        else do
          liftIO $ threadDelay 60
          return $ Unauthorized "Who are you? The thing did not answer...."
        
  where getPubKey = do
          fileDir <- publicDir <$> getYesod
          shaFilePath <- lookupPostParam "sha-file-name"
          case shaFilePath of
            Just sfp -> do
              pKey <- read <$> (liftIO $ readFile (fileDir ++ T.unpack sfp))
              return (Just pKey)
            Nothing -> return Nothing
        checkText :: T.Text -> PublicKey -> B.ByteString -> Double -> Handler Bool
        checkText text pk hash dl = do
          let test = T.encodeUtf8 text
          return $ verify sha512pss pk (B.concat [test,showBSUtf8 dl]) hash
        checkTimeLim :: UTCTime -> NominalDiffTime -> Handler Bool
        checkTimeLim time cd = do
          now <- liftIO getCurrentTime
          if abs (diffUTCTime now time) < cd
            then return True
            else return False
        readD = readT :: T.Text -> Double --} \_ -> 1000 :: Double
        sha512pss = defaultPSSParams SHA512 :: PSSParams SHA512 B.ByteString B.ByteString
\end{code}


get the record id
\begin{code}
getRecord :: String -> Object -> Maybe String
getRecord d o = do -- Maybe
  Array vs <-  HM.lookup "records" o
  let v = filter (\x -> lookup "name" x == Just (String $ T.pack d)) vs
  x <- lookup "id" $ head v
  case x of
    String str -> return $ T.unpack str
    _ -> Nothing
getRecordId :: String -> String -> Handler String
getRecordId subDomain domain = do --Handler
  lT <- loginToken <$> getYesod
  let paramStr = mkParam
        [ "login_token" =: lT
        , "format"      =: "json"
        , "lang"        =: "cn"
        , "domain"      =: domain
        ]
      request = HTTP.postRequestWithBody "https://dnsapi.cn/Record.List"
                                         "application/x-www-form-urlencoded"
                                         paramStr
  rp <- simpleHTTP request >>= getResponseBody
  case rp of
    Right rp' -> do -- Handler
      let id = do -- Maybe 
            o <- decodeStrict rp'
            getRecord subDomain o
      case id of
        Just i -> return i
        _ -> notFound
    Left e -> notFound
  where mkParam :: [String] -> String
        mkParam []     = ""
        mkParam [x]    = x
        mkParam (x:xs) = x ++ ('&': mkParamStep xs)
        (=:) :: String -> String -> String
        a =: b = a ++ ("=":b)


simpleHTTP = liftIO . HTTP.simpleHTTP
getResponseBody = liftIO . HTTP.getResponseBody
readT = read. T.unpack
showBSUtf8 = show . T.unpack . T.decodeUtf8
\end{code}
