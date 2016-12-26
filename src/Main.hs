{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
module Main where
import Control.Monad
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString
import Control.Monad
import System.Process
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.InterpolatedString.Perl6 (q, qc)

main :: IO ()
main = do
    run 8080 app

app :: Application
app request respond = do
    response <- case rawPathInfo request of
                    "/" -> return indexHtml 
                    "/wake" -> wakeMeUP request
                    "/stop" -> stop
                    _ -> return fourNotFour
    respond response

indexHtml :: Response
indexHtml = responseFile
    status200
    [("Content-Type","text/html")]
    "index.html"
    Nothing

wakeMeUP :: Request -> IO Response
wakeMeUP request = do 
    let query = queryString request
    let hour = join $ lookup "hour" query
    let min  = join $ lookup "min" query
    runCommand $ job hour min
    return $ responseLBS
        status200
        [("Content-Type","text/plain")]
        "Alarm set at...to be coded later"

stop :: IO Response
stop = do
    runCommand "mpc stop"
    return $ responseLBS
        status200
        [("Content-Type","text/plain")]
        "alarm stoped"

fourNotFour :: Response
fourNotFour = responseLBS
    status404
    [("Content-Type","text/plain")]
    "404 not found"

type Hour = Maybe ByteString
type Minute = Maybe ByteString

job :: Hour -> Minute -> String
job Nothing _ = [q| echo "didn't get hour "  |]
job (Just hr) Nothing =  [qc| echo "* {hr} * * * /usr/bin/mpc play" | crontab - |] 
job (Just hr) (Just min) = [qc| echo "{min} {hr} * * * /usr/bin/mpc play" | crontab - |]
