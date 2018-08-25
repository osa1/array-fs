{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.FS where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Array as A
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.LruCache as LRU
import Data.Pool
import Prelude hiding (read)
import System.IO

data Array a = Array
    { _arrayLRU       :: !(MVar (LRU.LruCache Word (MVar (A.Array Word a))))
        -- ^ The cache, indexed by chunk index
    , _arrayOffset    :: !(Word -> Word)
        -- ^ Given index of an item return offset of the item in file
    , _arrayChunkSize :: !Word
        -- ^ How many elements to load into cache in one read
    , _arrayHandles   :: !(Pool Handle)
        -- ^ Pool of file handles to the file.
    , _arrayRead      :: !(Handle -> IO a)
        -- ^ How to read one `a` from the handle.
    }

new :: FilePath
         -- ^ Path to the file to read elements from.
    -> (Word -> Word)
         -- ^ Mapping of element indices to file offsets. E.g. `f 5` should
         -- return offset of 5th element in the file.
    -> Word
         -- ^ Number of chunks: how many elements to load into the cache in one
         -- read.
    -> Word
         -- ^ How many chunks to keep in cache.
    -> (Handle -> IO a)
         -- ^ How to read one `a` from a handle. If you have a data with
         -- `Binary` instance use `readBinary`.
    -> IO (Array a)

new file offset read_size chunks read_fn = do
    lru_var <- newMVar (LRU.empty (fromIntegral chunks))

    pool <- createPool
              (openFile file ReadMode)
              hClose
              1 -- stripes
              (fromIntegral (30 :: Int)) -- close handles in 30 seconds
              8 -- at most 8 handles

    return Array
      { _arrayLRU = lru_var
      , _arrayOffset = offset
      , _arrayChunkSize = read_size
      , _arrayHandles = pool
      , _arrayRead = read_fn
      }

readBinary
    :: B.Get a
    -> Int
         -- ^ How much to load from file to parse, in bytes. If parse is partial
         -- more data with this size will be loaded, until parsing finishes.
    -> Handle
    -> IO a
readBinary get load_size h =
    go (B.runGetIncremental get)
  where
    go (B.Fail _ _ _) = error "Parse failed" -- TODO
    go (B.Partial cont) = BS.hGet h load_size >>= go . cont . Just
    go (B.Done _ _ a) = return a

read :: Word -> Array a -> IO a
read n arr@(Array lru_var _ chunk_size _ _) = do
    let chunk_n = n `div` chunk_size

    chunk_var <- modifyMVar lru_var $ \lru ->
      case LRU.lookup chunk_n lru of
        Just (chunk_var, lru') ->
          return (lru', chunk_var)
        Nothing -> do
          chunk_var <- newEmptyMVar

          -- TODO error handling
          _ <- forkIO $
            catch (loadChunk chunk_n chunk_var arr)
                  (\(_ :: SomeException) ->
                      putMVar chunk_var (error "Chunk loading failed"))

          return (LRU.insert chunk_n chunk_var lru, chunk_var)

    chunk <- readMVar chunk_var
    return (chunk A.! (n `mod` chunk_size))

loadChunk :: Word -> MVar (A.Array Word a) -> Array a -> IO ()
loadChunk chunk_n chunk_ref (Array _ offset chunk_size handles read_fn) = do
    let chunk_offset = offset (chunk_n * chunk_size)
    withResource handles $ \h -> do
      let offsets = map offset [chunk_offset .. chunk_offset*chunk_size - 1]
      as <- forM offsets $ \o -> do
              hSeek h AbsoluteSeek (fromIntegral o)
              read_fn h
      putMVar chunk_ref (A.array (0, chunk_size-1) (zip [0..] as))
