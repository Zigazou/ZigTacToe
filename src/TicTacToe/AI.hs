module TicTacToe.AI where

import Data.Maybe (mapMaybe, isNothing, isJust)
import Data.List (find)

import TicTacToe.Type

firstJust :: [Maybe a] -> Maybe a
firstJust (Nothing:ls) = firstJust ls
firstJust (Just a:_) = Just a
firstJust _ = Nothing

replaceNth :: Int -> a -> [a] -> [a]
replaceNth 0 newVal (_:xs) = newVal:xs
replaceNth n newVal (x:xs) = x:replaceNth (n - 1) newVal xs
replaceNth _ _ _ = []

winner :: TTTBoard -> Maybe TTTPlayer
winner (TTTBoard [a,b,c,d,e,f,g,h,i]) = firstJust $ winner' <$> s
    where s = [(a,b,c),(d,e,f),(g,h,i),(a,d,g),(b,e,h),(c,f,i),(a,e,i),(c,e,g)]
          winner' (Just PlayerX, Just PlayerX, Just PlayerX) = Just PlayerX
          winner' (Just PlayerO, Just PlayerO, Just PlayerO) = Just PlayerO
          winner' _ = Nothing
winner _ = Nothing

playAt :: TTTBoard -> TTTPlayer -> TTTCoords -> TTTBoard
playAt (TTTBoard cells) p (x, y) =
    TTTBoard $ replaceNth (x + y * 3) (Just p) cells

freeCells :: TTTBoard -> [TTTCoords]
freeCells = mapMaybe freeCell . zip coords . boCells
    where coords = [ (x, y) | y <- [0..2], x <- [0..2] ]
          freeCell (_, Nothing) = Nothing
          freeCell (cd, _) = Just cd

nextBoards :: TTTBoard -> TTTPlayer -> [(TTTCoords, TTTBoard)]
nextBoards b p = zip frees (playAt b p <$> frees)
    where frees = freeCells b

aiPlay :: TTTBoard -> TTTPlayer -> TTTCoords
aiPlay board player =
    case (winner1, depth2) of
         (Just (cx, _), _) -> cx
         (_, (cx, _):_) -> cx
         _ -> (0, 0)
    where
        depth1 = nextBoards board player
        depth2 = [ (move, boards)
                 | (move, board') <- depth1
                 , let boards = nextBoards board' (nextPlayer player)
                 , noWinner boards
                 ]
        winner1 = find (isJust . winner . snd) depth1
        noWinner = isNothing . find (isJust . winner . snd)
