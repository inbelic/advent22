module D7.D7 where 

import Parser
import Control.Applicative 
import Control.Monad (liftM)
import Data.List (transpose)
import qualified Data.Map as Map

type DirName = String
data Dir = Dir
  { files   :: [File]
  , subDirs :: Map.Map DirName (Int, Int)
  }

emptyDir :: Dir
emptyDir = Dir [] Map.empty

type File = Int

data Command
  = CdRet
  | Cd DirName
  | Ls [DirName] [File]
  deriving (Show)

cdRetP :: Parser Command
cdRetP = fmap (const CdRet) $ stringP "$ cd .."

cdP :: Parser Command
cdP = fmap Cd $ stringP "$ cd " *> spanP ((/=) '\n')

dirP :: Parser DirName
dirP = stringP "dir " *> spanP ((/=) '\n')

fileP :: Parser Int
fileP = intP <* spanP ((/=) '\n')

eitherP :: Parser (Either DirName Int)
eitherP = (fmap Left dirP) <|> (fmap Right fileP)

lsP :: Parser Command
lsP = stringP "$ ls\n" *> p 
  where p = Parser $ \input -> do
              xs <- parse lsP' input
              Just . fmap toLs $ xs

lsP' :: Parser [Either DirName Int]
lsP' = whileP (eitherP <* charP '\n')

toLs :: [Either DirName Int] -> Command
toLs = foldr toLs' (Ls [] [])

toLs' :: Either DirName Int -> Command -> Command
toLs' (Left dir) (Ls dirs files) = Ls (dir:dirs) files
toLs' (Right file) (Ls dirs files) = Ls dirs (file:files)
toLs' _ _ = undefined

commandP :: Parser Command
commandP = lsP <|> cdRetP <|> cdP

load :: Parser [Command]
load = linesP commandP

solveDir :: [Command] -> Dir -> Maybe ([Command], Int, Int)
solveDir [] dir = solveDir [CdRet] dir
solveDir (cmd:cmds) (Dir fs sds) = case cmd of
    CdRet -> do
      let cur = sum . Map.foldr (\x acc -> fst x : acc) [] $ sds
      let acc = sum . Map.foldr (\x acc -> snd x : acc) [] $ sds
      let cur' = sum fs + cur
      case cur' <= maxSize of
        True -> Just (cmds, cur', acc + cur')
        False -> Just (cmds, cur', acc)
    (Ls dirs files) -> solveDir cmds dir'
      where dir' = Dir files . foldr f sds $ dirs
            f dn ds = Map.insert dn (0,0) ds
    (Cd subDirName) -> do
      (cmds', cur, acc) <- solveDir cmds emptyDir
      solveDir cmds' . Dir fs . Map.insert subDirName (cur, acc) $ sds

solveDir' :: Int -> [Command] -> Dir -> Maybe ([Command], Int, Int)
solveDir' nd [] dir = solveDir' nd [CdRet] dir
solveDir' nd (cmd:cmds) (Dir fs sds) = case cmd of
    CdRet -> do
      let cur = sum . Map.foldr (\x acc -> fst x : acc) [] $ sds
      let cur' = sum fs + cur
      let acc = filter ((<=) nd) . Map.foldr (\x acc -> snd x : acc) [cur'] $ sds
      let acc' = case null acc of
                   True -> 0
                   False -> minimum acc
      Just (cmds, cur', acc')
    (Ls dirs files) -> solveDir' nd cmds dir'
      where dir' = Dir files . foldr f sds $ dirs
            f dn ds = Map.insert dn (0,0) ds
    (Cd subDirName) -> do
      (cmds', cur, acc) <- solveDir' nd cmds emptyDir
      solveDir' nd cmds' . Dir fs . Map.insert subDirName (cur, acc) $ sds

maxSize = 100000
ttlSize = 70000000
neededSize = 30000000

run :: IO ()
run = do
  putStrLn "Day Seven solutions are..."
  (Just (_, cmds)) <- liftM (parse load) $ readFile "app/D7/commands.txt"
  let (Just ([], ttl, filteredTtl)) = solveDir (tail cmds) $ emptyDir
  putStrLn $ show filteredTtl
  let toFree = neededSize - (ttlSize - ttl)
  let (Just ([], _, minDelete)) = solveDir' toFree (tail cmds) $ emptyDir
  putStrLn $ show minDelete
