{-# LANGUAGE ScopedTypeVariables #-}

module Task3
  ( -- * Types
    ConcurrentHashTable(..)
    -- * Functions
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.STM (STM, atomically, readTVar, readTVarIO, throwSTM)
import Control.Concurrent.STM.TArray (TArray)
import Control.Concurrent.STM.TVar (TVar, newTVar, writeTVar)
import Control.Monad (forM_)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Hashable (Hashable, hash)

-- | A hash table cell type that can either store (key, value) or show its absence.
newtype Entry k v = Entry (Maybe (k, v))

-- | The aliased type displays a real container containing
-- a table for better code readability.
type Storage k v = TArray Int (Entry k v)

-- | The main type representing a multi-threaded hash table.
-- Stores the container and the current size. Supports transactional operations.
-- The open addressing algorithm with linear search is used.
data ConcurrentHashTable k v = ConcurrentHashTable
  { elements :: TVar (Storage k v)
  , size     :: TVar Int
  }

-- | Returns empty 'Entry'. Uses for better code readability.
emptyCell :: Entry k v
emptyCell = Entry Nothing

-- | Builds 'Entry' by provided key and value.
mkEntry :: k -> v -> Entry k v
mkEntry key val = Entry (Just (key, val))

-- | Calculates the expected index of the item in 'Storage'.
calcId :: Hashable k => k -> Int -> Int
calcId k sz = hash k `mod` sz

-- | Atomically creates an empty table of 16 elements.
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  size  <- newTVar 0
  elems <- newArray (0, 16) emptyCell >>= newTVar
  return $ ConcurrentHashTable elems size

-- | Atomically returns the value matching the provided key, wrapped in 'Maybe'.
getCHT :: forall k v. (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key cht = atomically $ do
  table  <- readTVar $ elements cht
  (_, n) <- getBounds table

  let keyId = calcId key n
  readArray table keyId >>= \entry -> retOrSkip entry keyId table keyId n

  where
    findValue :: Storage k v -> Int -> Int -> Int -> STM (Maybe v)
    findValue elems stId sz id
      | id == stId = return Nothing
      | otherwise  = readArray elems id >>= \entry -> retOrSkip entry id elems stId sz

    retOrSkip :: Eq k => Entry k v -> Int -> Storage k v -> Int -> Int -> STM (Maybe v)
    retOrSkip (Entry Nothing)                 _   _     _  _    = return Nothing
    retOrSkip (Entry (Just (curKey, curVal))) ind elems sz stId
      | curKey == key = return $ Just curVal
      | otherwise     = findValue elems stId sz ((ind + 1) `mod` sz)

-- | Atomically insert (key, value) into the table.
-- If the number of elements exceeds 3/4 of the capacity,
-- then the table doubles the size.
putCHT :: forall k v. (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key val cht = atomically $ do
  sz     <- readTVar $ size cht
  table  <- readTVar $ elements cht
  (_, n) <- getBounds table

  if (n * 3 `div` 4) <= sz + 1
  then expandCHT table (n * 2) >>= \newTable -> writeTVar (elements cht) newTable
  else return ()

  correctTable <- readTVar $ elements cht
  (_, correctCapacity) <- getBounds correctTable
  isNewElem <- insert key val correctTable (calcId key correctCapacity) correctCapacity

  if isNewElem
  then writeTVar (size cht) (sz + 1)
  else return ()

  where
    insert :: Eq k => k -> v -> Storage k v -> Int -> Int -> STM (Bool)
    insert insKey insVal elems id n = do
      Entry entryCont <- readArray elems id
      case entryCont of
        Nothing -> writeArray elems id (mkEntry insKey insVal) >> return True
        Just (curKey, curVal) ->
          if curKey == insKey
          then writeArray elems id (mkEntry insKey insVal) >> return False
          else insert insKey insVal elems ((id + 1) `mod` n) n

    expandCHT :: Storage k v -> Int -> STM (Storage k v)
    expandCHT oldData newSz = do
      (_, oldSz) <- getBounds oldData
      newData    <- newArray (0, newSz) emptyCell
      forM_ [0..oldSz] $ \i -> do
        Entry entryCont <- readArray oldData i
        case entryCont of
          Nothing           -> return True
          Just (oldK, oldV) -> insert oldK oldV newData (calcId oldK newSz) newSz
      return newData

-- | Returns the size of the table at the time of the query.
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT cht = readTVarIO $ size cht
