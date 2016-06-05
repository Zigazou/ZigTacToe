{- |
Module      : AI
Description : TicTacToe artificial intelligence
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module TicTacToe.AI (aiPlay) where

import Data.Maybe (mapMaybe, isNothing, isJust)
import Data.List (find)

import TicTacToe.Type

-- | Returns the first Just element of a list or Nothing
firstJust :: [Maybe a] -> Maybe a
firstJust (Nothing:ls) = firstJust ls
firstJust (Just a:_) = Just a
firstJust _ = Nothing

-- | Replace the nth element of a list
replaceNth :: Int -> a -> [a] -> [a]
replaceNth 0 newVal (_:xs) = newVal:xs
replaceNth n newVal (x:xs) = x:replaceNth (n - 1) newVal xs
replaceNth _ _ _ = []

-- | Tells the winner of the board, if any
winner :: TTTBoard -> Maybe TTTPlayer
winner (TTTBoard [a,b,c,d,e,f,g,h,i]) = firstJust $ winner' <$> s
    where s = [(a,b,c),(d,e,f),(g,h,i),(a,d,g),(b,e,h),(c,f,i),(a,e,i),(c,e,g)]
          winner' (Just PlayerX, Just PlayerX, Just PlayerX) = Just PlayerX
          winner' (Just PlayerO, Just PlayerO, Just PlayerO) = Just PlayerO
          winner' _ = Nothing
winner _ = Nothing

-- | Play a move at specified coordinates on a board
playAt :: TTTBoard -> TTTPlayer -> TTTCoords -> TTTBoard
playAt (TTTBoard cells) p (x, y) =
    TTTBoard $ replaceNth (x + y * 3) (Just p) cells

-- | Returns free cells list for a specific board
freeCells :: TTTBoard -> [TTTCoords]
freeCells = mapMaybe freeCell . zip coords . boCells
    where coords = [ (x, y) | y <- [0..2], x <- [0..2] ]
          freeCell (cd, Nothing) = Just cd
          freeCell _ = Nothing

{-| Given a board and a player, returns the list of updated board for each
    possible move.
-}
nextBoards :: TTTBoard -> TTTPlayer -> [(TTTCoords, TTTBoard)]
nextBoards b p = zip frees (playAt b p <$> frees)
    where frees = freeCells b

{-| Given a board and a player, returns coordinates the artificial
    intelligence would play.
-}
aiPlay :: TTTBoard -> TTTPlayer -> TTTCoords
aiPlay board player =
    case (winner1, depth2, freeCells board) of
         (Just (cx, _), _, _) -> cx
         (_, (cx, _):_, _)    -> cx
         (_, _, (cx:_))       -> cx
         _                    -> (0, 0)
    where
        depth1 = nextBoards board player
        depth2 = [ (move, boards)
                 | (move, board') <- depth1
                 , let boards = nextBoards board' (nextPlayer player)
                 , noWinner boards
                 ]
        winner1 = find (isJust . winner . snd) depth1
        noWinner = isNothing . find (isJust . winner . snd)
