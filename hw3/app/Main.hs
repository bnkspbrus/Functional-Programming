module Main (main) where

import Control.Monad.Cont (liftIO)
import Data.Set (fromList)
import HW3.Action (HiPermission (..), runHIO)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Prettyprinter (line)
import Prettyprinter.Render.Terminal (putDoc)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, outputStrLn, runInputT)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just input -> do
          case parse input of
            (Left err) -> outputStr $ errorBundlePretty err
            (Right expr) -> do
              result <- liftIO $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              case result of
                (Left err) -> outputStrLn $ show err
                (Right value) -> liftIO $ putDoc $ prettyValue value <> line
          loop
