module AI.Learning.Example.Restaurant where

import Control.Monad
import Control.Monad.Random
import qualified Graphics.Gnuplot.Simple as G
import System.IO.Unsafe

import AI.Learning.Core
import AI.Learning.DecisionTree
import qualified AI.Learning.RandomForest as RF
import AI.Util.Util

data Patrons = Empty | Some | Full deriving (Show,Eq,Ord,Enum,Bounded)
data Price = Cheap | Medium | Expensive deriving (Show,Eq,Ord,Enum,Bounded)
data Type = French | Thai | Burger | Italian deriving (Show,Eq,Ord,Enum,Bounded)
data Wait = None | Short | Med | Long deriving (Show,Eq,Ord,Enum,Bounded)

data Restaurant = Restaurant {
    alt :: Bool,        -- is there an alternative?
    bar :: Bool,        -- is there a bar?
    fri :: Bool,        -- is it a friday?
    hun :: Bool,        -- are you hungry?
    pat :: Patrons,     -- how many patrons are there?
    price :: Price,     -- how cheap is it?
    rain :: Bool,       -- is it raining?
    res :: Bool,        -- do you have a reservation?
    food :: Type,       -- what type of food is it?
    wait :: Wait,       -- what is the wait?
    willWait :: Bool    -- will you wait?
} deriving (Show,Eq,Ord)

atts :: [Att Restaurant]
atts = [ att alt "Alternative"
       , att bar "Bar"
       , att fri "Friday"
       , att hun "Hungry"
       , att pat "Patrons"
       , att price "Price"
       , att rain "Raining"
       , att res "Reservation"
       , att food "Food"
       , att wait "Wait" ]

randomRestaurantNoisy :: RandomGen g => Float -> Rand g Restaurant
randomRestaurantNoisy noise = do
  alt   <- getRandom
  bar   <- getRandom
  fri   <- getRandom
  hun   <- getRandom
  pat   <- getRandomEnum 3
  price <- getRandomEnum 3
  rain  <- getRandom
  res   <- getRandom
  food  <- getRandomEnum 4
  wait  <- getRandomEnum 4
  let mkR ww   = Restaurant alt bar fri hun pat price rain res food wait ww
      willWait = decide actualTree (mkR False)
  p <- getRandomR (0,1)
  return $ if p > noise
    then mkR willWait
    else mkR (not willWait)

randomRestaurant :: RandomGen g => Rand g Restaurant
randomRestaurant = do
  alt   <- getRandom
  bar   <- getRandom
  fri   <- getRandom
  hun   <- getRandom
  pat   <- getRandomEnum 3
  price <- getRandomEnum 3
  rain  <- getRandom
  res   <- getRandom
  food  <- getRandomEnum 4
  wait  <- getRandomEnum 4
  let mkR ww   = Restaurant alt bar fri hun pat price rain res food wait ww
      willWait = decide actualTree (mkR False)
  return (mkR willWait)

randomDataSetNoisy :: RandomGen g => Float -> Int -> Rand g [Restaurant]
randomDataSetNoisy noise n = replicateM n (randomRestaurantNoisy noise)

randomDataSet :: RandomGen g => Int -> Rand g [Restaurant]
randomDataSet n = replicateM n randomRestaurant

--------------------
-- Model builders --
--------------------

treeBuilder :: [Restaurant] -> [Bool] -> Restaurant -> Bool
treeBuilder as _ a =
  let tree = fitTree willWait atts as
   in decide tree a

forestBuilder :: Int -> Int -> [Restaurant] -> [Bool] -> Restaurant -> Bool
forestBuilder nTree nAtt as bs a = do
  let forest = unsafePerformIO $ evalRandIO $
               RF.randomForest nTree nAtt willWait atts as
   in RF.decide forest a

---------------------------------------
-- Demo of the decision tree library --
---------------------------------------

--runWithNoise :: RandomGen g =>
--       Builder Restaurant Bool
--    -> Int
--    -> Int
--    -> Float
--    -> Rand g Float
--runWithNoise builder nTrain nTest noise = do
--  xTrain <- randomDataSetNoisy noise nTrain
--  xTest  <- randomDataSetNoisy noise nTest
--  let yTrain = map willWait xTrain
--      yTest  = map willWait xTest
--  return (crossValidate builder xTrain yTrain xTest yTest)

--runNoNoise :: RandomGen g =>
--        Int
--     -> Int
--     -> Rand g Float
--runNoNoise nTrain nTest = do
--  xTrain <- randomDataSet nTrain
--  xTest  <- randomDataSet nTest
--  let yTrain = map willWait xTrain
--      yTest  = map willWait xTest
--  return (crossValidate treeBuilder xTrain yTrain xTest yTest)

--demo2 :: Float -> IO ()
--demo2 noise = do
--  vals <- evalRandIO $ do
--    let ns = [1..100]
--    mcrs <- forM ns $ \n -> do
--      sampleMcrs <- replicateM 100 $ runWithNoise treeBuilder n 100 noise
--      return (mean sampleMcrs)
--    return (zip ns $ map (*100) mcrs)

--  let xlabel = G.XLabel "Size of test set"
--      ylabel = G.YLabel "Misclassification Rate (%)"
--      title  = G.Title  "Decision Tree Demo (Restaurants)"

--  G.plotList [xlabel,ylabel,title] vals

--demo1 :: IO ()
--demo1 = do
--  vals <- evalRandIO $ do
--    let ns = [1..100]
--    mcrs <- forM ns $ \n -> do
--      sampleMcrs <- replicateM 100 $ runNoNoise n 100
--      return (mean sampleMcrs)
--    return (zip ns $ map (*100) mcrs)

--  let xlabel = G.XLabel "Size of test set"
--      ylabel = G.YLabel "Misclassification Rate (%)"
--      title  = G.Title  "Decision Tree Demo (Restaurants)"

--  G.plotList [xlabel,ylabel,title] vals

--------------------------------------
-- The decision tree in Figure 18.2 --
--------------------------------------

actualTree :: DTree Restaurant () Bool
actualTree = do
  patrons <- attribute pat "Patrons"
  case patrons of
    Empty -> return False
    Some  -> return True
    Full  -> do
      time <- attribute wait "WaitTime"
      case time of
        None  -> return True
        Short -> do
          hungry <- attribute hun "Hungry"
          if not hungry
            then return True
            else do
              alternative <- attribute alt "Alternative"
              if not alternative
                then return True
                else do
                  raining <- attribute rain "Rain"
                  return raining
        Med   -> do
          alternative <- attribute alt "Alternative"
          if not alternative
            then do
              reservation <- attribute res "Reservation"
              if reservation
                then return True
                else do
                  hasBar <- attribute bar "Bar"
                  return hasBar
            else do
              friday <- attribute fri "Fri/Sat"
              return friday
        Long  -> return False

------------------------------------------
-- This is the example in AIMA Fig 18.3 -- 
------------------------------------------

restaurants = 
  [ Restaurant True False False True Some Expensive False True French None True
  , Restaurant True False False True Full Cheap False False Thai Med False
  , Restaurant False True False False Some Cheap False False Burger None True
  , Restaurant True False True True Full Cheap True False Thai Short True
  , Restaurant True False True False Full Expensive False True French Long False
  , Restaurant False True False True Some Medium True True Italian None True
  , Restaurant False True False False Empty Cheap True False Burger None False
  , Restaurant False False False True Some Medium True True Thai None True
  , Restaurant False True True False Full Cheap True False Burger Long False
  , Restaurant True True True True Full Expensive False True Italian Short False
  , Restaurant False False False False Empty Cheap False False Thai None False
  , Restaurant True True True True Full Cheap False False Burger Med True ]

fittedTree = fitTree willWait atts restaurants


