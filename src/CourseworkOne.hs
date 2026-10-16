{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module CourseworkOne where

import Halatro.Constants
import Halatro.Types
import Data.Foldable (find)
import System.Random (Random)
import Data.Text.Internal.Builder (flush)
import Data.List
import Data.Bool (bool)
import GHC.Base (TrName(TrNameD))

suits :: [Suit]
suits = [Hearts,Diamonds,Clubs,Spades]
handTypes :: [HandType]
handTypes = [RoyalFlush,StraightFlush,FourOfAKind,FullHouse,Flush,Straight,ThreeOfAKind,TwoPair,Pair,HighCard]
--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

contains :: Hand -> HandType -> Bool -- depending what the handType b is, it will call a function to check if that handtype exists in the hand. 
contains xs b
    |b == Pair = findN xs 2
    |b == TwoPair = findTwoPair xs
    |b == ThreeOfAKind = findN xs 3
    |b == Straight = findStraight $ map rank xs
    |b == Flush = findFlush xs
    |b == FullHouse = findFullHouse xs
    |b == FourOfAKind = findN xs 4
    |b == StraightFlush = findStraightFlush xs
    |b == RoyalFlush = findRoyalFlush xs
    |b == HighCard = findHighCard xs
    |otherwise = null xs

------------------------------------------------------
findHighCard :: Hand -> Bool
findHighCard [] = False
findHighCard xs = not (findN xs 2 || findStraight (map rank xs ) || findFlush xs  ) -- if there is neither a pair nor straight nor flush in the non empty hand then it must be a high card. This is because all other hand types contain at least one of these hand types.

------------------------------------------------------
findNCount:: [Rank]-> Rank -> Int -- finds how many occurances of a certain rank occurs in a list
findNCount [] _ = 0
findNCount (x:xs) r = length $ filter (== r) (x:xs)

findN :: Hand -> Int -> Bool -- finds all values of in list whose rank occurs in the list n or more times.
findN [] _ = False
findN xs n  =
    let
        a = map rank xs
        b = [Card r s| (Card r s) <- xs , findNCount a r >= n  ]
    in
        not $  null b


---------------------------------------------------------
findFlush :: Hand -> Bool -- checks how many of each suit is in the list and if any of them is greater than 5 then it will return true if not false 
findFlush [] = False
findFlush xs =
    let
        h = sum [ 1 |(Card _ st) <- xs , st == Hearts]
        c = sum [ 1 |(Card _ st) <- xs , st == Clubs]
        d = sum [ 1 |(Card _ st) <- xs , st == Diamonds]
        s = sum [ 1 |(Card _ st) <- xs , st == Spades]
    in
        ((((h >= 5) || (c >= 5)) || (d >= 5)) || (s >= 5))

--------------------------------------------------------
findRoyalFlush:: Hand -> Bool -- for each suit, it checks whether the ten, jack, queen, king and aceare all in the list.
findRoyalFlush [] = False
findRoyalFlush xs =
    let
        a = [royalFlushCounter xs y | y <- suits]
    in
        or a

royalFlushCounter :: Hand -> Suit -> Bool -- given a certain suit it will check if the ten, jack, queen, king and ace of that suit are in the list
royalFlushCounter [] _ = False
royalFlushCounter xs test =  elem (Card Ten test) xs && elem (Card Jack test) xs && elem (Card Queen test) xs && elem (Card King test) xs && elem (Card Ace test) xs

---------------------------------------------------------
findStraightFlush:: Hand -> Bool -- for each card in the list it will check if there is a straight flush
findStraightFlush [] = False
findStraightFlush (Card r s : xs) = -- goes through every card in the list and checks if there is a straight but where all the cards have the same suit
    let
        a = [straightFlushCounter (Card rs sx) (Card r s : xs) |(Card rs sx) <- Card r s : xs ]
    in
        or  a

straightFlushCounter:: Card -> Hand -> Bool
straightFlushCounter _ [] = False
straightFlushCounter (Card r s) xs = -- checks whether the next 5 cards of the same suit are in the list and that they are not a jack queen or king since that would caus the staright to overlap.
    let
        q = elem (Card (specialSucc r) s) xs
        w = elem (Card (specialSucc $ specialSucc r) s) xs
        e = elem (Card (specialSucc $ specialSucc $ specialSucc r) s) xs
        t = elem (Card (specialSucc $ specialSucc $ specialSucc $ specialSucc r) s) xs --  stores the 5 consecutive cards to the card given and sees if they are in the list.
        a = q && w && e && t
    in
        (not ((r == Jack) || (r == Queen) || (r == King)) && a)

---------------------------------------------------------
findStraight :: [Rank] -> Bool
findStraight [] = False
findStraight xs = -- for every card in the list it checks if there is a straight where it is the bottom/ lowest ranked card in the straight.
    let
        a = [straightCounter x xs |x <- xs ]
    in
        or  a

straightCounter:: Rank -> [Rank]  -> Bool
straightCounter _ [] = False
straightCounter x xs = -- takes the rank and finds the 4 succsesive ranks and checks if all 4 of those are in the list. If that is true and the starting card isn't a jack, queen or king then it will return true. If not it will return false.
    let
        q = specialSucc x
        w = specialSucc q
        e = specialSucc w
        r = specialSucc e
        a = elem q xs && elem w xs && elem e xs && elem r xs
    in
        (not ((x == Jack) || (x == Queen) || (x == King)) && a)

specialSucc:: Rank -> Rank -- finds the rank with one rank higher than the one given. This was written because succ ace gives an error and this function fixes that error.
specialSucc a =
    if a == Ace then Two
    else succ a

----------------------------------------------------------
findFullHouse :: Hand -> Bool -- checks whether there is a full house in the string. It does this by checking if there is a 3 occasions of one rank and then 2 occasions of a rank that isnt the same rank as the threeOfAKind
findFullHouse [] =False
findFullHouse (Card r s: xs) =
    let
        a = findMaybeN (Card r s: xs) 3
    in
        findNWout (Card r s: xs) 2 a

findNWout :: Hand -> Int -> Maybe Rank -> Bool
findNWout _ _ Nothing = False
findNWout xs n (Just b) = not (null ([ 1 |x <- xs ,length (filter (\y -> rank y == rank x && rank y /= b ) xs)>= n]))


findMaybeN ::Hand -> Int -> Maybe Rank -- tries to find the first case of n repitions of a rank. if it cannot find the rank it will return nothing if it can it will return Maybe + the rank that was found.
findMaybeN [] _ = Nothing
findMaybeN (Card r _ : xs) n =
    let
        a = [r | x<- xs, rank x == r]
    in
        if length a >= (n-1) then Just $ head a
        else findMaybeN xs n


----------------------------------------------------------

findTwoPair :: Hand -> Bool -- finds a pair and then finds another pair in the list that is not equal to the first pair.
findTwoPair [] = False
findTwoPair xs =
    let
        y = findMaybeN xs 2
    in
        findNWout xs 2 y

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

--  searches through every Hand Type and checks if the hand contains it, it then returns the Head of the list since the Hand Types are in order of decreasing importance so the first item will be the best hand type. 
bestHandType :: Hand -> HandType
bestHandType [] = None
bestHandType xs =
    let
        as= [ y | y <- handTypes, contains xs y]
    in
        if null as then None else head as


--------------------------------------------------------------------------------
-- Part 3: score a played hand

whichCardsScore :: Hand -> [Card] -- for any hand of size 5 it will find the best hand type in the hand and then returns the cards which are in the actual hand type
whichCardsScore xs=
    let
        a = bestHandType xs
    in
        findBestCards xs a

-- if the hantype selected is one that requires 5 cards then the whole list can be returned since we can assume that the hand passed to this function will be 5 cards
findBestCards :: Hand -> HandType -> [Card]
findBestCards [] _ = []
findBestCards (Card r s :xs) h
    |h == RoyalFlush =  Card r s :xs
    |h == Pair = findNCards (Card r s :xs)  2
    |h == ThreeOfAKind = findNCards (Card r s :xs)  3 --all hand types which have repeated card ranks can be found using the same function.
    |h == HighCard = findHighestCard xs (Card r s)
    |h == FourOfAKind = findNCards (Card r s :xs)  4
    |h == Straight =  Card r s :xs
    |h == Flush = Card r s :xs
    |h == FullHouse = Card r s :xs
    |h == StraightFlush = Card r s :xs
    |h == TwoPair = findTwoPairCards  (Card r s :xs)
    |otherwise = []



findNCards :: Hand -> Int -> [Card] -- finds the first set of cards in the list in which the rank of the card repeats n (or more) times
findNCards [] _ =[]
findNCards (Card r s : xs) n =
    let
        a = [Card rankx suitx | (Card rankx suitx)<- xs, rankx == r] --  used recursion instead of a list comprehension since in recursion you are able to test the head against the rest of the list.

    in
        if length a >= (n-1) then a++ [Card r s] else findNCards xs n -- if the list created is equal in size to parameter given -1 then you know that there are n repitions of that rank in the list so the list can be returned if not we apply this function to the rest of the list.

findHighestCard :: Hand -> Card -> [Card]
findHighestCard [] x = [x]
findHighestCard (x:xs) r =
    if x > r then findHighestCard xs x
    else findHighestCard xs r -- iterates through the list and finds the card with the highest rank

findTwoPairCards :: Hand -> [Card]
findTwoPairCards [] =[]
findTwoPairCards (Card r s:xs) =
    let
        b = filter (\y -> rank y == r) xs -- filters the cards depending on which ones have the same rank as the head of the list
    in
        if not $ null b then [Card r s] ++ b ++findTwoPairCards xs -- finds a pair, creates a list which contains that pair, then calls the function on the rest of the list.
        else findTwoPairCards xs

scoreHand :: Hand -> Int -- given a hand of 5 cards it will find the best hand type present in the hand. it will then sum the scores of every card in the hand add the corresponding number of default chips given the hand type and then multiply the sum by the hand type multiplier.
scoreHand xs =
    let
        htype = bestHandType xs
        rs = findBestCards xs htype
        ys = map rank rs

        (c,m) = handTypeValues htype
        b= map rankScore ys
    in
        (sum b + c) * m


--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

highestScoringHand :: [Card] -> Hand -- given a hand of 5 cards, finds the best hand type, then finds teh least number of cards which can score the highest score.
highestScoringHand xs =
    let
        a = bestHandType xs
    in
        highestScoringHandWType xs a


highestScoringHandWType :: [Card] -> HandType -> Hand -- given what the current best hand type is. it will return the card which when scored create the hand type
highestScoringHandWType [] _ = []
highestScoringHandWType (Card r s : xs) h
    |h == Pair = findNCards (Card r s :xs)  2
    |h == HighCard = findHighestCard xs (Card r s)
    |h == ThreeOfAKind = findNCards (Card r s :xs)  3
    |h == RoyalFlush = findRoyalFlushCards (Card r s : xs) -- i made a seperate function for royal flush because all royal flushes have the same rank card so writing a function that finds one is going to be quicker than using findHighestHand
    |h == TwoPair = findHighestHand (Card r s :xs) 4
    |h == Straight = findHighestHand (Card r s :xs) 5
    |h == Flush = findHighestHand (Card r s :xs) 5
    |h == FullHouse = findHighestHand (Card r s :xs) 5
    |h == FourOfAKind = findHighestHand (Card r s :xs) 4
    |h == StraightFlush = findHighestHand (Card r s :xs) 5
    |otherwise = [] -- for none



findRoyalFlushCards:: [Card] -> [Card] -- tegh suits are filtered to see which one has a royal flush then a royal flush is created using that suit
findRoyalFlushCards xs =
    let
        as = filter (royalFlushCounter xs ) suits
        a = head as
    in
        [Card Ten a ,Card Jack a ,Card Queen a ,Card King a ,Card Ace a ]


findHighestHand :: [Card] -> Int -> [Card] -- finds the hand of size n that scores the highest out of all possible hands of size n. 
findHighestHand xs n =
    let
        a = findNCombinations xs n
        b = map scoreHand a
        c = maximum b
        ds = filter (\y -> scoreHand y == c) a -- finds the hand which scores the highest.

    in
        head ds



cardListRankScore :: [Card] -> Int -- finds the total score of every card in a hand
cardListRankScore  xs = sum $ map (rankScore . rank) xs


findNCombinations :: [Card] -> Int -> [[Card]] -- finds all the combinations of the cards of size n i.e all possible hands of size n from the larger hand
findNCombinations [] _ = []
findNCombinations xs n =
    let
        a:: [[Card]]
        a = subsequences xs
        b = filter (\y -> length y == n) a
    in
        b


--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move -- if the players hand is of size less than 5 then play all cards if not play the 5 highest cards
simpleAI _ xs = if length xs >5 then Move Play $ findHigh5 xs
else Move Play xs


findHigh5 :: [Card] -> [Card] -- finds the 5 cards of highest rank in the list
findHigh5 xs =  take 5 $ reverse $ sort xs


sensibleAI :: [Move] -> [Card] -> Move -- plays the hand in the list of cards which will score the highest.
sensibleAI _ cs =  Move Play (highestScoringHand cs)

isMovePlay :: Move -> Bool -- checks whether a move was a play or a discard
isMovePlay (Move Play _ )= True
isMovePlay (Move Discard _) = False


myAI :: [Move] -> [Card] -> Move -- ai which intends to score the highest score possible while also using discards.
myAI moves xs  =
    let
        played = sum  [ 1 | m<- moves , isMovePlay m ] -- finds the number of hands the user has played
        discarded = sum [ 1 |m <- moves ,  not $ isMovePlay m] -- finds the number of times the user has discarded a card.
        typeBefore = bestHandType xs -- finds the current best hand type in the hand
        bestHand = highestScoringHand xs -- finds the current best hand that can be played.
    in
        (if discarded == 3  then Move Play (highestScoringHand xs) else -- if discarded == 3 i.e. the player can no longer discard then the robot has to play its best hand.
            if played == 2 then moveGivenType xs typeBefore else -- if its the last hand of the game it will allways try to discard cards to make the hand better 
            if currentScoreGood bestHand then Move Play (highestScoringHand xs) --  if the score of the current best hand is deemed as "good" then the best hand is played
            else moveGivenType xs typeBefore) -- if all others are false then the ai will try to discard cards it doesnt believe it needs.

moveGivenType:: [Card] -> HandType -> Move
moveGivenType xs h
    | h == HighCard = checkOthers xs
    | h == Pair = checkOthers xs
    | h == TwoPair = checkWhenPairs xs -- if we have a two pair or a ThreeOfAKind then it is likely to get full house next turn 
    | h == ThreeOfAKind =  Move Discard (findNonPairs xs)
    | h == Straight = checkOthers xs
    | h == Flush = checkOthers xs
    | h == FullHouse = Move Discard (findNonPairs xs)
    | otherwise = Move Play (highestScoringHand xs)


currentScoreGood :: [Card] -> Bool -- scores a hand and checks whether its score is good enough to be played
currentScoreGood xs =  scoreHand xs > 200

checkOthers:: [Card]-> Move
checkOthers xs =
    let
        a = nonFlush xs
        b = nonStraight xs
    in
    if not  $ null a then Move Discard a
    else if not $ null b then Move Discard b
    else Move Discard (getLowest5 xs ) --highestScoringHand xs

checkWhenPairs :: [Card] -> Move -- checks for a potential flush then a straight and if none are found it will return all cards which are not in a pair or more to try to get a full house. 
checkWhenPairs xs =
    let
        a = nonFlush xs
        b = nonStraight xs
    in
        if not  $ null a then Move Discard a
        else if not $ null b  then Move Discard b
        else Move Discard (findNonPairs xs)


findNonPairs:: [Card] -> [Card] -- finds all the items in list whose rank only appears once
findNonPairs xs =
    let
        a = map rank xs
        b = [Card r s| (Card r s) <- xs , findNCount a r == 1 ]
    in
        b

boolToInt:: Bool -> Int -- maps a boolean value to an integer vallue 
boolToInt True = 1
boolToInt False = 0

howCloseStraight :: [Rank] -> Rank -> [Bool] -- makes a list of how many of the 4 consecutive ranks following x are in the list.
howCloseStraight xs x =
    let
        a = specialSucc x
        b = specialSucc a
        c = specialSucc b
        d = specialSucc c
    in
        [elem a xs, elem b xs, elem c xs, elem d xs]


nonStraight :: [Card]-> [Card] -- returns the cards which wouldnt likely make up a straight if cards were to be discarded or returns none if it doesnt think a straight can be made.
nonStraight xs =
    let
        y = map rank xs
        a = [[x,specialSucc  x,specialSucc $ specialSucc x,specialSucc $ specialSucc $ specialSucc x] |x<- y , sum (map boolToInt (howCloseStraight  y x )) == 3, not ( x == Jack ||  x == Queen ||  x == King )] -- if the starting card of the straight is either a jack a kiing or a queen then the straight is not valid.

    in
        if not (null a) then [Card r s|(Card r s) <-xs, notElem r (head a) ] else []
-- for each card in the list if the number of elements which would make up a straight includingt that card is 4 then the straight is added to the list , then all cards which do not conatin any of those ranks in the original list are added to a different list which is returned.



nonFlush:: [Card] -> [Card] -- finds all cards of each rank and compares the total of their rank's values. if it is the highest and the algorithm thinks a flush could be made out of those cards then it will return all the cards which are not in that list 
nonFlush xs =
    let
        (hs,hl) = findListAndScore xs Hearts
        (cs,cl) = findListAndScore xs Clubs
        (ds,dl) = findListAndScore xs Diamonds
        (ss,sl) = findListAndScore xs Spades
        a = [hs,ds,cs,ss]
    in
        if length hl >= 3 && maximum a == hs then  [Card r st| (Card r st ) <- xs , st /= Hearts] -- checks all list of almost flushes and if their score is is the highest of the list and its lenght is greater than three then all card not in that list are returned.
        else if length cl >= 3 && maximum a == cs then  [Card r st| (Card r st ) <- xs , st /= Clubs]
        else if length dl >= 3 && maximum a == ds then  [Card r st| (Card r st ) <- xs , st /= Diamonds]
        else if length sl >= 3 && maximum a == ss then  [Card r st| (Card r st ) <- xs , st /= Spades]
        else []


findListAndScore :: [Card] -> Suit -> (Int,[Card]) -- finds all instances of a suit and caluculates the sum of the ranks. if the number of instances of that suit < 3 then the score is set to 0
findListAndScore xs s =
    let
        a = filter (\y -> suit y == s) xs
        b = sum (map (rankScore . rank) a)
    in
        if length a >= 3 then (b,a) else (0,a)




getLowest5:: [Card]-> [Card] --  gets the 5 cards in the list with the lowest rank
getLowest5 xs = take 5 (sort xs )








