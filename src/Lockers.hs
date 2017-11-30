module Lockers
  ( LockerMap
  , LockerState(..)
  , lockerLookup
  ) where

import qualified Data.Map as Map
import Data.Maybe

data LockerState
  = Free
  | Taken
  deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Integer (LockerState, Code)

lockerLookup :: Integer -> LockerMap -> Either String Code
-- lockerLookup n m =
--   let locker = Map.lookup n m
--   in if isNothing locker
--        then Left "No locker with this number"
--        else if (fst . fromJust) locker == Taken
--               then Left "This locker is already taken"
--               else Right ((snd . fromJust) locker)
lockerLookup n m =
  case Map.lookup n m of
    Nothing -> Left $ "Locker number " ++ show n ++ " doesn't exist!"
    Just (state, code) ->
      case state of
        Taken -> Left $ "Locker number " ++ show n ++ " is already taken!"
        _ -> Right code
