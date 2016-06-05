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
import qualified Data.ByteString.Lazy as LB
import Data.Map (toList)
import Data.Aeson (decode)
import Control.Monad (liftM)
import Control.Applicative ((<|>))
import Snap

import TicTacToe.Type (TTTAction(TTTAction, acAction, acBoard, acYou)
                      , TTTCoords
                      )
import TicTacToe.AI (aiPlay)

data App = App

-- | Returns POST content without any key
getDirectPostParam :: MonadSnap m => m B.ByteString
getDirectPostParam = liftM (extract . toList . rqPostParams) getRequest
    where extract [(k, _)] = k
          extract _        = ""

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
    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
    modifyResponse $ setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"

    writeBS "Hello!"

tictactoeHandlerPost :: Handler App App ()
tictactoeHandlerPost = do
    mAction <- liftM (decode . LB.fromStrict) getDirectPostParam

    modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
    modifyResponse $ setHeader "Access-Control-Allow-Methods" "GET, POST"

    case mAction of
         Just (TTTAction { acAction = "init" }) -> do
             modifyResponse $ setContentType "application/json; charset=utf-8"
             writeBS "{\"name\":\"ZigTacToe\"}"

         Just action@(TTTAction { acAction = "play-turn" }) -> do
             modifyResponse $ setContentType "application/json; charset=utf-8"
             let fc = fmtCoords $ aiPlay (acBoard action) (acYou action)
             writeBS $ B.concat [ "{\"play\":\"", fc, "\"}" ]

         _ -> do
             modifyResponse $ setResponseCode 400
             writeBS "Invalid action sent by the caller"

main :: IO ()
main = serveSnaplet defaultConfig appInit
