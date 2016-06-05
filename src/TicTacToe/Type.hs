{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module TicTacToe.Type where

import Data.Aeson (FromJSON, parseJSON, Value(Object, String), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Text as T

data TTTPlayer = PlayerO | PlayerX deriving (Eq, Show)

nextPlayer :: TTTPlayer -> TTTPlayer
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

type TTTCell = Maybe TTTPlayer
data TTTBoard = TTTBoard { boCells :: [TTTCell] } deriving (Eq, Show)
type TTTCoords = (Int, Int)

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
