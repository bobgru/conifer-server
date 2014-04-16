{- # LANGUAGE MultiParamTypeClasses, RankNTypes #-}

-- | Database testing utility functions and combinators
module Utils
where

--import Control.Monad (forM_,unless,when)
--import Data.Aeson (Value(..))
--import qualified Data.ByteString.Char8 as B
--import Data.Convertible.Base -- (convert)
--import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
--import qualified Data.Map as M
--import Data.Maybe
--import qualified Data.Text as T
--import qualified Data.Vector as V
--import qualified Data.Yaml as Y
--import Database.HDBC
--import RMS.HDBCAesonAdapter
import System.Exit
import System.FilePath
--import System.IO
import System.Process
import System.Random
import System.Time

-- | Pad a string with the specified number of leading zeroes.
zeroExtend :: Int      -- ^ Desired length of string
           -> String   -- ^ Original string
           -> String
zeroExtend n s = take n' ['0' | i <- [1..]] ++ s
    where n' = max 0 (n - length s)

-- | Create a unique name using a prefix, the current date, and a random number.
uniqueName :: String     -- ^ Prefix
           -> IO String
uniqueName s = do
    now <- getClockTime
    ct  <- toCalendarTime now
    r   <- rand
    return $ mkUniqueName s ct r

-- | Format a name using a prefix, a date, and an integer.
mkUniqueName :: String         -- ^ Prefix
             -> CalendarTime   -- ^ A date and time (typically now)
             -> Int            -- ^ A number (typically random)
             -> String
mkUniqueName s ct r = intercalate "-" [s, y, m, d, show r]
    where y = (show . ctYear) ct
          m = (zeroExtend 2 . show . fromEnum . ctMonth) ct
          d = (zeroExtend 2 . show . ctDay) ct

-- | Return a random integer.
rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

-- | Execute a command and report the result--i.e. the process exit code.
execCmd :: (String, [String])  -- ^ A command and its arguments
        -> String              -- ^ Standard input for the command
        -> IO Bool
execCmd (name, args) input = do
    (ec, _, _) <- readProcessWithExitCode name args input
    return $ ec == ExitSuccess

-- | Execute a command and report the result, dumping all text from stdout
-- and stderr to the terminal.
execCmdDump :: (String, [String])  -- ^ A command and its arguments
            -> String              -- ^ Standard input for the command
            -> IO Bool
execCmdDump (name, args) input = do
    (ec, out, err) <- readProcessWithExitCode name args input
    putStrLn $ "ExitCode:" ++ show ec
    putStrLn $ "stdout:" ++ out
    putStrLn $ "stderr:" ++ err
    if ec == ExitSuccess then return True else return False

-- Functions to prepare arguments for execution as processes.

execDrawConifer name = execCmd (drawConiferCmd name)
drawConiferCmd  name = ("../conifer/.cabal-sandbox/bin/individual",
                        ["-w", show 400, "-o", name, "-u"])

drawConiferToFile :: FilePath -> String -> String -> IO String
drawConiferToFile dir prefix ud = do
    name <- (uniqueName prefix)
    let fname = name ++ ".svg"
    let svgPath = dir </> fname
    execDrawConifer svgPath ud
    return fname