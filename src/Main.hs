{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Main
Description : TicTacToe server using Snap
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Aeson (eitherDecode)
import Control.Monad (liftM)
import Control.Applicative ((<|>))
import Snap

import TicTacToe.Type (TTTAction(TTTAction, acAction, acBoard, acYou)
                      , TTTCoords
                      )
import TicTacToe.AI (aiPlay)

data App = App

-- | Initializes the web server
appInit :: SnapletInit App App
appInit = makeSnaplet "tictactoe" "TicTacToe bot" Nothing $ do
    addRoutes [ ("tictactoe", tictactoeHandler) ]
    wrapSite id
    return App

-- | Format coordinates when returning value to Bot’s Arena from Tinad
fmtCoords :: TTTCoords -> B.ByteString
fmtCoords (x, y) = B.pack [ (head . show) x, '-', (head . show) y ]

-- | TicTacToe handler
tictactoeHandler :: Handler App App ()
tictactoeHandler = method OPTIONS tictactoeHandlerOptions
               <|> method POST tictactoeHandlerPost

tictactoeHandlerOptions :: Handler App App ()
tictactoeHandlerOptions = do
    modifyResponse $ setHeader "Access-Control-Allow-Headers"
                               "Origin, X-Requested-With, Content-Type, Accept"
    modifyResponse $ setHeader "Access-Control-Allow-Methods" "OPTIONS, POST"
    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

    writeBS "Hello!"

tictactoeHandlerPost :: Handler App App ()
tictactoeHandlerPost = do
    eAction <- liftM eitherDecode (readRequestBody 2048)

    modifyResponse $ setHeader "Access-Control-Allow-Headers"
                               "Origin, X-Requested-With, Content-Type, Accept"
    modifyResponse $ setHeader "Access-Control-Allow-Methods" "OPTIONS, POST"
    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

    case eAction of
         Right (TTTAction { acAction = "init" }) -> do
             modifyResponse $ setContentType "application/json; charset=utf-8"
             writeBS "{\"name\":\"ZigTacToe\"}"

         Right action@(TTTAction { acAction = "play-turn" }) -> do
             modifyResponse $ setContentType "application/json; charset=utf-8"
             let fc = fmtCoords $ aiPlay (acBoard action) (acYou action)
             writeBS $ B.concat [ "{\"play\":\"", fc, "\"}" ]

         Right _ -> writeBS "Invalid action"

         Left err -> do
             modifyResponse $ setResponseCode 400
             writeBS . B.pack $ err

main :: IO ()
main = serveSnaplet defaultConfig appInit
