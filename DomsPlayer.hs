{-

    module DomsPlayer: design for a player to use different strategies to improve performance

    Tactics takes four arguments descriving the current state of the game:
    The hand of the player
    The domino board
    The player number
    The current score
    and returns a domino to play and the side to play it at

    Section "Checks" consists of functions that test whether 
    a corresponding action can be applied.
    Section "Tactics" consists of functions that a return a domino to play
    according to certain strategy, used only after a check function returns True.
    Section "Players" contains functions representing different players
    
-}


module DomsPlayer where
    import DomsMatch
    import Data.Char
    import Data.List
    import System.Random


    type Tactics = Hand -> DominoBoard -> Player -> Scores -> (Domino, End)
    

    {-returns dominoes that might be in the opponent's hand: 
    not in the current player's hand or played-}
    opponentHand :: Hand -> DominoBoard -> Hand
    opponentHand [] _ = domSet
    opponentHand hand board = domSet \\((hand ++ map swap hand)++(history++map swap history))
        where 
            history = getHistory board
            swap (a,b) = (b,a)
            hand' = hand ++ map swap hand
            
            
    {-a list of played dominoes from the history of the board-}
    getHistory :: DominoBoard -> Hand
    getHistory InitBoard = []
    getHistory (Board d1 d2 hist) = [x | (x, _,_)<- hist]

    scoreB :: Maybe DominoBoard -> Int
    scoreB (Just board) = scoreBoard board


    {- CHECKS  
    -}

    {-if a player can win the game with a single domino-}
    canWin :: Hand -> DominoBoard -> Player -> Int -> Bool
    canWin _ InitBoard _ _ = False
    canWin [] board p points = False
    canWin (d:rest) board p points
        | (canPlay d L board ) && ((scoreB leftBoard)+ points) == 61 = True
        | (canPlay d R board ) && ((scoreB rightBoard)+ points) == 61 = True
        | otherwise = canWin rest board p points
        where 
        leftBoard = playDom p d board L
        rightBoard = playDom p d board R

    

    {-if there is a double domino that can be played-}
    canDouble :: Hand -> DominoBoard -> Bool
    canDouble [] _ = False
    canDouble (d:rest) b
        | (canPlay d L b || canPlay d R b) && isDouble d = True
        | otherwise = canDouble rest b
    
    isDouble :: Domino -> Bool
    isDouble (d1,d2) 
        | d1 == d2 = True
        | otherwise = False

    {-if opponent can be blocked-}
    canBlock :: Hand -> DominoBoard->Player -> Bool
    canBlock [] _ _  = False
    canBlock _ InitBoard _ = False
    canBlock h b p 
        | fst(possPlays h b)/=[] && canBlock' h oh b L p = True
        | snd(possPlays h b)/=[]  && canBlock' h oh b R p = True
        | otherwise = False
        where oh = opponentHand h b
    canBlock' :: Hand -> Hand -> DominoBoard ->End->Player-> Bool
    canBlock' [] _ _ _ _ = False
    canBlock' _ _ InitBoard _ _= False
    canBlock' hand@(d:rest) oh b e p
        | (canPlay d e b) && (blocked oh newBoard) = True
        | otherwise = canBlock' rest oh b e p
        where  Just newBoard = playDom p d b e
        
    {-used after canBlock returns false, increases the probability of blocking the opponent 
    in case there are dominoes sleeping and in the opponent's hand-}
    canTryBlock :: Hand -> DominoBoard -> Player -> Bool
    canTryBlock [] _ _  = False
    -- if the board is empty, can't block
    canTryBlock _ InitBoard _ = False
    canTryBlock h b p 
        | fst(possPlays h b)/=[] && canTryBlock' h oh b L p = True
        | snd(possPlays h b)/=[] && canTryBlock' h oh b R p = True
        | otherwise = False
        where oh = opponentHand h b
    canTryBlock' :: Hand -> Hand -> DominoBoard ->End->Player-> Bool
    canTryBlock' [] _ _ _ _ = False
    canTryBlock' _ _ InitBoard _ _ = False
    canTryBlock' hand@(d:rest) oh b e p
        | (canPlay d L b) && length(fst(possPlays oh newLBoard)) < 3 = True
        | (canPlay d R b)  && length(snd(possPlays oh newRBoard)) < 3 = True
        | otherwise = canTryBlock' rest oh b e p
        where 
            Just newLBoard = playDom p d b L
            Just newRBoard = playDom p d b R

    
    {- check if a domino will not cause the same value on both sides of the board
        if the player doesn't have that value -}
    isSafe :: (Domino,End)-> Hand -> DominoBoard -> Bool
    isSafe _ _ InitBoard = True
    isSafe ((ld,rd),e) hand (Board d1 d2 hist)
        | length hand == 1 = True
        | e == L && ld == rightSide && onlyPip ld hand = False
        | e == R && rd == leftSide && onlyPip rd hand = False
        | otherwise = True
            where 
                leftSide = fst(d1)
                rightSide = snd(d2)


    {-TACTICS
    -}

    {-choose the highest scoring domino-}
    playHighest :: Tactics
    playHighest hand board p scores 
        {- if one of the lists is empty (nothing can be played on one of the sides) take the first 
        item from the second list since they are sorted in descending order-}
        | sortedLeft == [] = (snd(head(sortedRight)),R)
        | sortedRight == [] = (snd(head(sortedLeft)),L)
        -- compare if the socre would be high on the left or right side
        | fst(head(sortedLeft)) > fst(head(sortedRight)) = (snd(head(sortedLeft)),L)
        | otherwise = (snd(head(sortedRight)),R)
     where 
         --a list of dominoes that can be playedo on the left and right sides of the current
         leftPlays  = fst(possPlays hand board) 
         rightPlays = snd(possPlays hand board) 
         -- a list of tuples (new score, domino) hypothetically played thus forming a new board state
         newLeftB = map(\d -> (scoreB (playDom p d board L),d)) leftPlays 
         newRightB = map(\d -> (scoreB (playDom p d board R),d)) rightPlays 
         -- the list of score and domino tuples sorted by the score in descending order
         sortedLeft = sortBy(\(s,d)(s1,d1) -> (flip compare) s s1 ) newLeftB
         sortedRight = sortBy(\(s,d)(s1,d1) -> (flip compare) s s1 ) newRightB
    
    {- play a domino that will score 61 (win the game) -}
    win61 :: Tactics
    win61 (d:rest) b p (s1,s2)
        | (canPlay d L b ) && ((scoreB leftBoard)+ score) == 61 = (d, L)
        | (canPlay d R b ) && ((scoreB rightBoard)+ score) == 61 = (d, R)
        | otherwise = win61 rest b p (s1,s2)
        where 
            score 
                | p == P1 = s1
                | otherwise = s2
            leftBoard = playDom p d b L
            rightBoard = playDom p d b R
                

    {- a missing case of empty hand or board because 
    it's applied only after canBlock returns true: if defiinite block is possible-}
    blockOpponent :: Tactics
    blockOpponent h b p s 
        | fst(possPlays h b)/=[] = block h oh b L p 
        | snd(possPlays h b)/=[] = block h oh b R p
        where oh = opponentHand h b
    block :: Hand -> Hand->DominoBoard -> End ->Player -> (Domino,End)
    block h@(d:rest) oh b e p
        | (canPlay d e b) && (blocked oh newBoard) = (d,e)
        | otherwise = block rest oh b e p
        where  Just newBoard = playDom p d b e
    

    {- applied only after canTryBlock returns true: if there is a possibility to block-}
    tryBlock :: Tactics
    tryBlock h b p s
        -- if there are possible plays on the left, try to block it on the left. Same for the right side
        | fst(possPlays h b)/=[] = tryBlock' h oh b L p 
        | snd(possPlays h b)/=[] = tryBlock' h oh b R p
        where oh = opponentHand h b
    tryBlock' :: Hand -> Hand->DominoBoard -> End ->Player -> (Domino,End)
    tryBlock' h@(d:rest) oh b e p
        {- 3 is a random number to allow a chance of a domino not being in the hand 
        and "sleeping" instead -}
        | (canPlay d L b) && length(fst(possPlays oh newLBoard))<3 = (d,L)
        | (canPlay d R b) && length(snd(possPlays oh newRBoard))<3 = (d,R)
        | otherwise  = tryBlock' rest oh b e p
        where 
            Just newLBoard = playDom p d b L
            Just newRBoard = playDom p d b R
    
            
    {-returns True if the given number is only one in the hand-}
    onlyPip :: Int -> Hand -> Bool
    onlyPip value hand 
        | len == 0 = False
        | len ==2 = True
        | otherwise = False
            where len = length[(x,y)| (x,y) <- hand, (x == value || y==value)] 


    {- applied only after checking so no need to check if there is an available one -}
    playDouble :: Tactics
    playDouble [d] b _ _ 
        | canPlay d L b  = (d,L)
        | otherwise = (d,R)
    playDouble (d:rest) b p s
        | canPlay d L b && isDouble d = (d,L)
        | canPlay d R b && isDouble d = (d,R)
        | otherwise = playDouble rest b p s



    {-  PLAYERS 
    each one tries a strategy and otherwise plays the highest scoring domino-}

    {- plays highest scoring domino -}
    playerZero :: DomsPlayer
    playerZero h b p (s1,s2)
        | canWin h b p score == True  = win61 h b p (s1,s2)
        | otherwise = playHighest h b p (s1,s2)
        where
            score 
             | p == P1 = s1
             | otherwise = s2
  


    {- plays double dominoes in the begining of the game -}
    playerOne :: DomsPlayer
    playerOne h b p (s1,s2)
        | canWin h b p score == True  = win61 h b p (s1,s2)
        | canDouble h b =  playDouble h b p (s1,s2) --play doubles in the beginning
        | otherwise = playHighest h b p (s1,s2)
            where 
                double = playDouble h b p (s1,s2)
                score 
                    | p == P1 = s1
                    | otherwise = s2
    

    {- tries to block the opponent when losing by 5 points-}
    playerTwo :: DomsPlayer
    playerTwo h b p (s1,s2)
        | canWin h b p score == True  =  win61 h b p (s1,s2)
        | ( canBlock h b p && p == P1 && ((s1-s2) > 5)) =  blockOpponent h b p (s1,s2)
        | ( canBlock h b p && p == P2 && ((s2-s1) > 5))  = blockOpponent h b p (s1,s2)
        | canTryBlock h b p && p == P1 && ((s1-s2) > 5)=  tryBlock h b p (s1,s2)
        | canTryBlock h b p && p == P2 && ((s2-s1) > 5) =  tryBlock h b p (s1,s2)
        | otherwise = playHighest h b p (s1,s2)
        where
            score 
                | p == P1 = s1
                | otherwise = s2
   

