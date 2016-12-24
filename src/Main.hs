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
app request respond = respond $ case rawPathInfo request of
    "/" -> indexHtml 
    "/wake" -> wakeMeUP request
    _ -> fourNotFour

indexHtml :: Response
indexHtml = responseFile
    status200
    [("Content-Type","text/html")]
    "index.html"
    Nothing

wakeMeUP :: Request -> Response
wakeMeUP request = 
    let query = queryString request
        hour = join $ lookup "hour" query
        min = join $ lookup "min" query
 
    in responseLBS
        status200
        [("Content-Type","text/plain")]
        "Alarm set at...to be coded later"

fourNotFour :: Response
fourNotFour = responseLBS
    status404
    [("Content-Type","text/plain")]
    "404 not found"
    
