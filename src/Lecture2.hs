{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

{-# LANGUAGE ScopedTypeVariables   #-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    , Chest(..)
    , dragonFight) where

-- VVV If you need to import libraries, do it after this line ... VVV

import Data.Foldable (find)
import Data.Char (isSpace)

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct list = let
  go :: Int -> [Int] -> Int
  go _ (0 : _) = 0
  go p [] = p
  go p (x : xs) = go (p * x) xs
  in go 1 list


{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n list
  | n < 0 = (Nothing, list)
  | otherwise = let
    (left, right) = splitAt n list
    removed = find (const True) (take 1 right)
    newList = (left ++ drop 1 right)
    in (removed, newList)

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: [Char] -> [Char]
dropSpaces = takeWhile (not . isSpace) . dropWhile isSpace


{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores insight, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)
data Knight = Knight
    { knightHealth            :: Int
    , knightAttackAmount      :: Int
    , knightEndurance         :: Int
    }
  deriving (Show)

data Chest a = Chest
  { chestGold     :: Int
  , chestTreasure :: Maybe a
  }
  deriving (Show)

data DragonColour = Red | Black | Green
  deriving (Show)

data Dragon = Dragon
  { dragonColour     :: DragonColour
  , dragonExperience :: Int
  , dragonHealth     :: Int
  , dragonFirePower  :: Int
  }
  deriving (Show)

data Reward a = Reward
  { rewardExperience :: Int
  , rewardTreasure   :: Maybe a
  , rewardGold       :: Int
  }
  deriving (Show)

data Outcome a = KinghtDies
  | KnightRuns
  | DragonDies (Reward a)
  deriving (Show)

reward :: Dragon -> Chest a -> Reward a
reward (Dragon Green experience _ _) chest =
  Reward
  { rewardExperience = experience
  , rewardTreasure = Nothing
  , rewardGold = chestGold chest }
reward dragon chest =
  Reward
  { rewardExperience = dragonExperience dragon
  , rewardTreasure = chestTreasure chest
  , rewardGold = chestGold chest }

dragonFight :: forall a . Knight -> Dragon -> Chest a -> Outcome a
dragonFight knight dragon chest =
  let
    go :: Int -> Knight -> Dragon -> Outcome a
    go step k d
      | dragonHealth    d <= 0 = DragonDies (reward d chest)
      | knightHealth    k <= 0 = KinghtDies
      | knightEndurance k <= 0 = KnightRuns
      | otherwise = -- next move
        let
          k2 = k {
            knightEndurance = knightEndurance k - 1
          , knightHealth =
              if mod step 10 == 0
                then knightHealth k - dragonFirePower d
                else knightHealth k
          }
          d2 = d {
            dragonHealth = dragonHealth d - knightAttackAmount k
          }
        in go (step + 1) k2 d2
  in go 1 knight dragon

{-

>>> dragonFight Knight { knightHealth = 10, knightAttackAmount = 2, knightEndurance = 10}  Dragon { dragonColour = Red, dragonExperience = 5, dragonHealth = 5, dragonFirePower = 100 }  Chest { chestGold = 100, chestTreasure = Just "Ruby" }
DragonDies (Reward {rewardExperience = 5, rewardTreasure = Just "Ruby", rewardGold = 100})
  
  Green dragons melt the treasure:
>>> dragonFight Knight { knightHealth = 10, knightAttackAmount = 2, knightEndurance = 10}  Dragon { dragonColour = Green, dragonExperience = 5, dragonHealth = 5, dragonFirePower = 100 }  Chest { chestGold = 100, chestTreasure = Just "Ruby" }
DragonDies (Reward {rewardExperience = 5, rewardTreasure = Nothing, rewardGold = 100})

>>> dragonFight Knight { knightHealth = 10, knightAttackAmount = 2, knightEndurance = 100}  Dragon { dragonColour = Green, dragonExperience = 5, dragonHealth = 50, dragonFirePower = 100 }  Chest { chestGold = 100, chestTreasure = Just "Ruby" }
KinghtDies

>>> dragonFight Knight { knightHealth = 2000, knightAttackAmount = 2, knightEndurance = 2}  Dragon { dragonColour = Green, dragonExperience = 5, dragonHealth = 50, dragonFirePower = 100 }  Chest { chestGold = 100, chestTreasure = Just "Ruby" }
KnightRuns

-}


----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing [] = True -- Consider empty lists always increasing
isIncreasing [_] = True -- last element in the list is always increasing
isIncreasing (x : y : ys) = x < y && isIncreasing (y : ys)

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge l1 l2 =
  let
    go :: [Int] -> [Int] -> [Int]
    go [] b = b
    go a [] = a
    go (a : as) (b : bs)
      | a == b = (a : b : go as bs)
      | a < b = (a : go as (b : bs))
      | otherwise = (b : go (a : as) bs)
  in
    go l1 l2


{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort [a, b]
  | a <= b = [a, b]
  | otherwise = [b, a]
mergeSort a =
  let
    (l1, l2) = splitAt 2 a
  in
     merge (mergeSort l1) (mergeSort l2)


{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.

>>> eval [("y", 1), ("x", 2)] (Add (Var "y") (Add (Var "x") (Lit 10)))
Right 13
-}
eval :: Variables -> Expr -> Either EvalError Int
eval vars expr =
  let
    go :: Expr -> Either EvalError Int
    go (Lit value) = Right value
    go (Var name) = case lookup name vars of
      Just value -> Right value
      Nothing -> Left (VariableNotFound name)
    go (Add a b) = case (go a, go b) of
        (Left aValue, _) -> Left aValue
        (_, Left bValue) -> Left bValue
        (Right aValue, Right bValue) -> Right (aValue + bValue)
  in
    go expr


{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.

>>> constantFolding (Add (Var "x") (Add (Lit 10) (Add (Var "y") (Add (Lit 15) (Lit 20)))))
Add (Lit 45) (Add (Var "x") (Var "y"))

-}
constantFolding :: Expr -> Expr
constantFolding expr = let
    go :: Expr -> ([Int], [String]) -> ([Int], [String])
    go (Lit value) (ls, vs) = (value : ls, vs)
    go (Var name) (ls, vs) = (ls, name : vs)
    go (Add a b) (ls, vs) =
      let
        (aLs, aVs) = go a ([], [])
        (bLs, bVs) = go b ([], [])
      in
        (aLs ++ bLs ++ ls, aVs ++ bVs ++ vs)

    sumExpr :: Expr -> [Expr] -> Expr
    sumExpr a [] = a
    sumExpr a (b : bs) = Add a (sumExpr b bs)

    (usedLs, usedVs) = go expr ([], [])

    constantLiteral = sum usedLs

  in case constantLiteral of
    0 -> case usedVs of
      [] -> Lit 0
      (v : vs) -> sumExpr (Var v) (map Var vs)
    _ -> sumExpr (Lit constantLiteral) (map Var usedVs)
