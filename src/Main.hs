{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           System.FilePath
import           System.IO
--import           qualified Data.ByteString as B
import           qualified Data.ByteString.Char8 as B
--import           Data.Maybe
import           Utils

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
         ifTop (writeBS "hello world")
    <|>  route [ ("foo", writeBS "bar")
               , ("echo/:echoparam", echoHandler)
               , ("app/conifer/draw", method GET drawConiferGet)
               , ("app/conifer/draw", method POST drawConifer)
               ]
    <|> dir "app"  (serveDirectory "../conifer-ui/app")
    <|> dir "js"   (serveDirectory "../conifer-ui/app/js")
    <|> dir "css"  (serveDirectory "../conifer-ui/app/css")
    <|> dir "img"  (serveDirectory "../conifer-ui/app/img")
    <|> dir "bower_components"  (serveDirectory "../conifer-ui/bower_components")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

drawConiferGet :: Snap ()
drawConiferGet = do
    writeBS "received the GET request to conifer/draw"

drawConifer :: Snap ()
drawConifer = do
    userdata <- getPostParam "userdata"
    maybe err ok userdata
    where err   = liftIO writeError
          ok ud = do fname <- liftIO $ draw ud
                     writeBS (B.pack fname)
          draw ud =  do name <- drawConiferToFile dir "conifer" (B.unpack ud)
                        saveData (dir </> name ++ ".json") ud
                        return name
          dir = "../conifer-ui/app/img"

drawConifer_dumpParams :: Snap ()
drawConifer_dumpParams = do
    rq <- getRequest
    let ps = rqPostParams rq
    let ud = B.pack $ show ps
    liftIO $ saveData_old ud

saveData_old :: B.ByteString -> IO ()
saveData_old s = do
    let filename = "userdata.json"
    h <- openFile filename WriteMode
    B.hPutStr h s
    hClose h

saveData :: FilePath -> B.ByteString -> IO ()
saveData f s = do
    h <- openFile f WriteMode
    B.hPutStr h s
    hClose h

writeError :: IO ()
writeError = do
    let filename = "userdata.json"
    h <- openFile filename WriteMode
    B.hPutStr h "{\"error\": \"Could not get POST data\"}"
    hClose h
