module HW3.Action where

import Control.Exception (Exception, throw)
import Control.Monad.Cont (liftIO)
import qualified Data.ByteString as B (readFile, writeFile)
import qualified Data.Sequence as S (fromList)
import Data.Set (Set, member)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import HW3.Base
import System.Directory
  ( createDirectory,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    setCurrentDirectory,
  )
import System.Random (getStdGen, setStdGen, uniformR)

--import System.Random.Stateful (applyAtomicGen, globalStdGen)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}

instance Functor HIO where
  fmap f (HIO rh) = HIO (fmap f . rh)

instance Applicative HIO where
  pure a = HIO (const (pure a))
  (HIO f) <*> (HIO a) = HIO (\s -> f s <*> a s)

instance Monad HIO where
  (HIO a) >>= f = HIO (\s -> a s >>= (\x -> runHIO (f x) s))

instance HiMonad HIO where
  runAction (HiActionRead fp) =
    HIO
      ( \s ->
          if AllowRead `member` s
            then do
              exists <- liftIO $ doesFileExist fp
              if exists
                then do
                  str <- liftIO $ B.readFile fp
                  return $ case decodeUtf8' str of
                    (Right text) -> HiValueString text
                    _ -> HiValueBytes str
                else do
                  list <- liftIO $ listDirectory fp
                  return $ HiValueList $ S.fromList $ fmap (HiValueString . T.pack) list
            else throw $ PermissionRequired AllowRead
      )
  runAction (HiActionWrite fp str) =
    HIO
      ( \s ->
          if AllowWrite `member` s
            then do
              liftIO $ B.writeFile fp str
              return HiValueNull
            else throw $ PermissionRequired AllowWrite
      )
  runAction (HiActionMkDir fp) =
    HIO
      ( \s ->
          if AllowWrite `member` s
            then do
              liftIO $ createDirectory fp
              return HiValueNull
            else throw $ PermissionRequired AllowWrite
      )
  runAction (HiActionChDir fp) =
    HIO
      ( \s ->
          if AllowRead `member` s
            then do
              liftIO $ setCurrentDirectory fp
              return HiValueNull
            else throw $ PermissionRequired AllowRead
      )
  runAction HiActionCwd =
    HIO
      ( \s ->
          if AllowRead `member` s
            then do
              cwd <- liftIO getCurrentDirectory
              return $ HiValueString $ T.pack cwd
            else throw $ PermissionRequired AllowRead
      )
  runAction HiActionNow =
    HIO
      ( \s ->
          if AllowTime `member` s
            then do
              time <- liftIO getCurrentTime
              return $ HiValueTime time
            else throw $ PermissionRequired AllowTime
      )
  runAction (HiActionRand x y) =
    HIO
      ( const $ do
          stdGen <- getStdGen
          let (rand, gen) = uniformR (x, y) stdGen
           in do
                setStdGen gen
                return $ HiValueNumber $ toRational rand
      )
  runAction (HiActionEcho text) =
    HIO
      ( \s ->
          if AllowWrite `member` s
            then do
              liftIO $ putStrLn $ T.unpack text
              return HiValueNull
            else throw $ PermissionRequired AllowWrite
      )
