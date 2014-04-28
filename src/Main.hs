{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as A
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B'
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           System.FilePath
import           System.IO
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
                     dims  <- liftIO $ extractDimsFromFile (dir </> fname)
                     case parseDims (B'.pack dims) of
                         Left e -> error e
                         Right (width, height) -> do
                             let d = mkData (B'.pack fname) width height
                             let bs = A.encode d
                             writeBS (B'.pack (B.unpack bs))
          draw ud =  do name <- uniqueName prefix
                        saveData (dir </> name ++ ".json") ud
                        drawConiferToFile dir name (B'.unpack ud)
                        return $ name ++ ".svg"
          dir = "../conifer-ui/app/img"
          prefix = "conifer"

mkData :: B'.ByteString -> Number -> Number -> A.Value
mkData name width height =
    A.Object $
        HM.fromList [
          ("name",   A.String $ T.pack (B'.unpack name))
        , ("width",  A.Number $ width)
        , ("height", A.Number $ height)
        ]

drawConifer_dumpParams :: Snap ()
drawConifer_dumpParams = do
    rq <- getRequest
    let ps = rqPostParams rq
    let ud = B'.pack $ show ps
    liftIO $ saveData_old ud

saveData_old :: B'.ByteString -> IO ()
saveData_old s = do
    let filename = "userdata.json"
    h <- openFile filename WriteMode
    B'.hPutStr h s
    hClose h

saveData :: FilePath -> B'.ByteString -> IO ()
saveData f s = do
    h <- openFile f WriteMode
    B'.hPutStr h s
    hClose h

writeError :: IO ()
writeError = do
    let filename = "userdata.json"
    h <- openFile filename WriteMode
    B'.hPutStr h "{\"error\": \"Could not get POST data\"}"
    hClose h
