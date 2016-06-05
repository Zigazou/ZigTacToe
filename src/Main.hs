{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Map (toList)
import Data.Aeson (decode)
import Control.Monad (liftM)
import Snap

import TicTacToe.Type (TTTAction(TTTAction, acAction, acBoard, acYou)
                      , TTTCoords
                      )
import TicTacToe.AI (aiPlay)

data App = App

appInit :: SnapletInit App App
appInit = makeSnaplet "tictactoe" "TicTacToe bot" Nothing $ do
    addRoutes [ ("tictactoe", tictactoeHandler) ]
    wrapSite id
    return App

getDirectPostParam :: MonadSnap m => m B.ByteString
getDirectPostParam = liftM (extract . toList . rqPostParams) getRequest
    where extract [(k, _)] = k
          extract _        = ""

fmtCoords :: TTTCoords -> B.ByteString
fmtCoords (x, y) = B.pack [ (head . show) x, '-', (head . show) y ]

tictactoeHandler :: Handler App App ()
tictactoeHandler = method POST $ do
    mAction <- liftM (decode . LB.fromStrict) getDirectPostParam
    case mAction of
         Just action@(TTTAction { acAction = "play-turn" }) ->
             writeBS $ fmtCoords $ aiPlay (acBoard action) (acYou action)
         _ -> do
             modifyResponse $ setResponseCode 400
             writeBS "Invalid action sent by the caller"

main :: IO ()
main = serveSnaplet defaultConfig appInit
