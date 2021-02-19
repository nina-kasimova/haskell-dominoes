module Domino where 
    import Data.Char
    import System.Random
    import Data.List
    import Debug.Trace


-- let d =Domino (5,4) 

    type Domino = (Int, Int)
    type Hand = [Domino]
    type Board = [Domino]
    data End = Left' | Right' deriving (Eq,Show)
    type DomsPlayer = Hand -> Board -> (Domino, End) 
    data Player = P1 | P2 deriving(Eq,Show)
  
    -- generates a list of all possile dominoes
    allDoms = [(x,y) | x <- [0..6], y<-[x..6]]


    board :: Board
    board = [(x,y) | x <- [0..6], y <- [0..6]]

    {-if board is empty, any domino is right. Left'
    parameter will match it to the very first pair in the list.
     Right' parameter will match it to the very last -}
    canPlay :: Domino -> End -> Board -> Bool
    canPlay d end [] = True
    canPlay (a,b) Left' ((d,_):_) 
        | a == d = True
        | b == d = True
        | otherwise = False
    canPlay (a,b) Right' board 
        |a == last_elem = True
        |b == last_elem = True
        | otherwise = False
            where last_elem = snd (last board) 

    -- проверяет если нет хода, reuse canPlay functions for both sides of the board
    blocked :: Hand -> Board -> Bool
    blocked hand board = null ([d | d<-hand, canPlay d Right' board || canPlay d Left' board ])

    -- check if this domino has already been played, find a match Domino in Board
    played :: Domino -> Board -> Bool
    played d board = not (null (filter (==d) board))

    -- return separately all dominoes that can be played on the left and on the right ends respectively
    possPlay :: Hand -> Board -> ([Domino],[Domino])
    possPlay hand board = (ls,rs)
        where
        ls =[h | h <- hand, canPlay h Left' board]
        rs = [h | h <- hand, canPlay h Right' board]

    {- if possible to play a certain domino at a said end, flips it so it match the number-}
    playDom :: Domino -> End-> Board -> Maybe Board
    playDom (x,y) Left' [] = Just [(x,y)]
    playDom (x,y) Right' [] = Just [(x,y)]
    playDom (x,y) Left' board
        | y == first_d = Just ((x,y):board)
        | x == first_d = Just ((y,x):board)
        | otherwise = Nothing
            where first_d = fst(head board)
    playDom (x,y) Right' board
        | x == last_d = Just (board ++[(x,y)])
        | y == last_d = Just (board ++ [(y,x)])
        | otherwise = Nothing
            where last_d = snd(last board)

    {- score counter хочу кофеееее-}
    scoreBoard :: Board -> Int
    scoreBoard [] = 0
    scoreBoard board 
        | s `mod` 3 == 0 && s `mod` 5 == 0 = s `div` 3 + s `div` 5
        | s `mod` 3 == 0 = s `div` 3
        | s `mod` 5 == 0 = s `div` 5
        | otherwise = 0
        where s = totalSum board
    totalSum board = edge_sum Left' + edge_sum Right'
        where
            left_edge = head board
            right_edge = last board
            edge_sum Left'
                | uncurry (==) left_edge = snd left_edge*2 -- if the pair is equal, add the values up
                | otherwise = fst left_edge 
            edge_sum Right'
                | uncurry (==) right_edge = snd right_edge*2
                | otherwise = snd right_edge
                

    scoreN :: Board -> Int -> [(Domino, End)]
    scoreN board n = score' board allDoms []
                    where 
                    score' board [] possible = possible
                    score' board (d : ds) possible 
                        | played d board = score' board ds possible
                        | otherwise = score' board ds newSet
                        where
                            leftB = playDom d Left' board
                            rightB = playDom d Right' board

                            goodLeft = leftB /= Nothing && scoreB leftB == n 
                            goodRight = rightB /= Nothing && scoreB rightB == n

                            scoreB (Just board) = scoreBoard board

                            newSet
                                | goodLeft && goodRight = (d, Left') :(d,Right'): possible
                                | goodLeft = (d,Left'):possible
                                | goodRight = (d,Right'):possible
                                | otherwise = possible
                

    simplePlayer :: DomsPlayer
    simplePlayer (d:rest) board xc
        | leftB /= Nothing = (d, Left')
        | rightB /=Nothing = (d, Right')
        | otherwise = simplePlayer rest board
        where
            leftB = playDom d Left' board
            rightB = playDom d Right' board

    shuffleDoms :: StdGen  -> [Domino]
    shuffleDoms g = [d | (d,n) <- sortBy cmp (zip allDoms (randoms g ::[Int]))]
        where cmp (_,y1) (_,y2) = compare y1 y2

    playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    playDomsRound player1 player2 seed = play' player1 player2 P1 (hand1,hand2, [], (0,0))
        where 
            shuffled = shuffleDoms (mkStdGen seed)
            hand1 = take 7 shuffled
            hand2 = take 7 (drop 7 shuffled)
            play' player1 player2 turn state@(hand1,hand2,board,(score1,score2))
             | p1_blocked && p2_blocked = (score1,score2)
             | turn == P1 && p1_blocked =  play' player1 player2 P2 state
             | turn == P2 && p2_blocked =  play' player1 player2 P1 state
             | turn == P2 = play' player1 player2 P1 newState
             | otherwise =  play' player1 player2 P2 newState
                 where
                p1_blocked = blocked hand1 board
                p2_blocked = blocked hand2 board
                (domino, end)
                  |turn == P1 = player1 hand1 board
                  |turn == P2 = player2 hand2 board 
                
                Just newBoard = playDom domino end board
                score = scoreBoard newBoard
                newState
                    |turn == P1 = (hand1\\[domino], hand2, newBoard, (score1 + score, score2))
                    | turn == P2 = (hand1, hand2\\[domino], newBoard, (score1, score2+ score))



    

    