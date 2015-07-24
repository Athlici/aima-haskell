module AI.Search.Example.Quirkle where

import qualified Data.Map as M
import qualified Data.List as L

data Quirkle = Quirkle deriving (Show)

data QMove = [(Int,Int,Stone)]

data Color = Yellow | Orange | Red  | Green   | Blue     | Purple  deriving (Read,Show,Eq,Enum)
data Shape = Circle | Square | Star | Diamond | Octagram | Flower  deriving (Read,Show,Eq,Enum)
data Stone = Stone {color :: Color, shape :: Shape} deriving (Read,Show,Eq)

data Cell  = Set Stone | Free [Stone] deriving (Show)
type Board = M.Map (Int,Int) Cell

--Adapt later for multiple players
data Player = Min | Max

--this might not be suffiecient later, but probabilities are hard :(
type HandProbs = [(Stone,Fractional)] 

data QState = QS --consider putting score, hand and/or ophan in a list of players
    { board :: Board
    , hand  :: [Stone]
    , order :: [Player]
    , ophan :: [HandProbs]
    , score :: [Int]}

instance Game Quirkle where
--TODO: figure out, how to pass given hand and order from user interaction
    initial (Quirkle h o) = QS initboard h o opprobs nullscore where
        initboard = M.fromList [((i,j),Free allStones) | i <- [0..5], j <- [0..5], i==0 || j==0 ]
--        allStones = [Stone col sha | col <- [Yellow .. Purple], sha <- [Circle .. Flower]]
  --add the possibility of having 3 same stones already
        allStones = []
--0 score for every player, ord has to be finite and duplicate free
	nullscore = L.map (\x -> 0) o 
        opprobs = []  --do much statistics here, much wow

    toMove _ s = L.head $ order $ s -- L.head . order

    legalMoves = undefined --this is gonna be hard

    makeMove = undefined --this too...

    utility = undefined --my score minus maximum of opponents

    terminalTest = undefined --any hand empty
