{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString
import Control.Monad
import System.Process
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    run 8080 app

app :: Application
app request respond = do
    response <- case rawPathInfo request of
                    "/" -> return indexHtml 
                    "/wake" -> wakeMeUP request
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
    runCommand "ls"
    return $ responseLBS
        status200
        [("Content-Type","text/plain")]
        "Alarm set at...to be coded later"

fourNotFour :: Response
fourNotFour = responseLBS
    status404
    [("Content-Type","text/plain")]
    "404 not found"
    
