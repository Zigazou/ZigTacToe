{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Type
Description : TicTacToe useful types
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module TicTacToe.Type
( TTTPlayer (PlayerO, PlayerX)
, nextPlayer
, TTTCell
, TTTBoard (TTTBoard, boCells)
, TTTCoords
, TTTAction ( TTTAction, acGameID, acAction, acGame, acBoard, acYou
            , acPlayerIndex
            )
)
where

import Data.Aeson (FromJSON, parseJSON, Value(Object, String), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Text as T

-- | A player
data TTTPlayer = PlayerO | PlayerX deriving (Eq, Show)

-- | Returns the next player given a player
nextPlayer :: TTTPlayer -> TTTPlayer
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

-- | A `TTTCell` may contain a `TTTPlayer` move or be free
type TTTCell = Maybe TTTPlayer

-- | A `TTTBoard` is composed of `TTTCell`
data TTTBoard = TTTBoard { boCells :: [TTTCell] } deriving (Eq, Show)

-- | `TTTCoords` contains coordinates in 2D
type TTTCoords = (Int, Int)

-- | A `TTTAction` represents an action according to Bot’s Arena from Tinad
data TTTAction = TTTAction
    { acGameID      :: T.Text
    , acAction      :: T.Text
    , acGame        :: T.Text
    , acBoard       :: TTTBoard
    , acYou         :: TTTPlayer
    , acPlayerIndex :: Int
    } deriving (Show)

instance FromJSON TTTAction where
    parseJSON (Object v) = TTTAction
                       <$> v .: "game-id"
                       <*> v .: "action"
                       <*> v .: "game"
                       <*> v .: "board"
                       <*> v .: "you"
                       <*> v .: "player-index"
    parseJSON invalid    = typeMismatch "TTTAction" invalid

instance FromJSON TTTPlayer where
    parseJSON (String "X") = return PlayerX
    parseJSON (String "O") = return PlayerO
    parseJSON invalid      = typeMismatch "TTTPlayer" invalid

instance FromJSON TTTCell where
    parseJSON (String "X") = return $ Just PlayerX
    parseJSON (String "O") = return $ Just PlayerO
    parseJSON (String _)   = return Nothing
    parseJSON invalid      = typeMismatch "TTTCell" invalid

instance FromJSON TTTBoard where
    parseJSON (Object v) = do
        let ks = [T.pack [x, '-', y] | y <- ['0'..'2'], x <- ['0'..'2']]
        cells <- sequence $ (v .:) <$> ks
        return $ TTTBoard cells
    parseJSON invalid = typeMismatch "TTTBoard" invalid
