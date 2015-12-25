import Control.Exception
import Control.Monad.Loops
import Data.Char (isSpace)
import Data.List (permutations, foldl')
import Data.Maybe (fromMaybe)
import System.Environment
import System.Process
import System.IO hiding (stdin, stdout, stderr)
import System.Random
import qualified Data.IntMap as M
import Debug.Trace

type Map = M.IntMap

data Color = White | Red deriving (Eq, Show)

invertColor :: Color -> Color
invertColor c
  | c == White = Red
  | otherwise  = White

invertPosition :: Int -> Int
invertPosition p = (24 - 1) - p

data Side  = Side {
  checkers :: Map Int,
  bar :: !Int
} deriving (Eq, Show)

won :: Side -> Bool
won (Side cs b) = M.null cs && b == 0

data Board = Board {
  white :: Side,
  red :: Side
}

generateMoves :: Side -> Side -> Int -> [(Int, Int)]
generateMoves us them die
    | bar us /= 0 = tryPosition (-1)
    | low >= 18 && low + die > 24 = (low, 24) : ys
    | low >= 18 = ys
    | otherwise = xs
  where tryPosition from = case M.lookup (invertPosition to) $ checkers them of
                               Nothing -> [m]
                               Just 1 -> [m]
                               _ -> []
          where to = from + die
                m = (from, to)
        f p = filter((flip p 24) . snd) .
              concatMap tryPosition .
              M.keys $
              checkers us
        xs = f (<)
        ys = f (<=)
        low = fromMaybe 24 . fmap (fst . fst) . M.minViewWithKey . checkers $ us

newGame :: Board
newGame = Board {
  white = side,
  red = side
} where side = Side {
                 checkers = M.fromList [(0, 2), (11, 5), (16, 3), (18, 5)],
                 bar = 0
               }

applyMove :: Side -> Side -> Int -> Int -> (Side, Side)
applyMove us them from to = (Side checkers_us bar_us,
                             Side checkers_them bar_them)
  where to' = invertPosition to
        bar_them = case M.lookup to' $ checkers them of
                       Nothing -> bar them
                       Just 1  -> bar them + 1
                       _       -> assert False 0
        decr Nothing  = Nothing
        decr (Just 1) = Nothing
        decr (Just k) = Just (k - 1)
        incr Nothing  = Just 1
        incr (Just k) = Just (k + 1)
        checkers_them = M.alter decr to' (checkers them)
        (checkers_us, bar_us)
            | from == -1 = (g $ checkers us, assert (bar us > 0) $ bar us - 1)
            | to >= 24   = (f $ checkers us, assert (bar us == 0) $ bar us)
            | otherwise  = (f . g $ checkers us, assert (bar us == 0) $ bar us)
          where f = M.alter decr from
                g = M.alter incr to

data Player = Player {
  stdin  :: Handle,
  stdout :: Handle,
  stderr :: Handle
} deriving (Show)

send :: Player -> String -> IO ()
send = hPutStrLn . stdin

receive :: Player -> IO String
receive (Player{stdout = h, stderr = e}) =
  catch (hGetLine h) $ \err -> do s <- hGetContents e
                                  putStrLn "error: "
                                  putStrLn s
                                  throw (err :: SomeException)


rollDie :: IO Int
rollDie = randomRIO (1, 6)

differentDice :: IO (Int, Int)
differentDice =
    do a <- rollDie
       b <- rollDie
       if a == b
         then differentDice
         else return (a, b)

possibleStates :: [Int] -> Side -> Side -> [(Side, Side)]
possibleStates dice side0 side1 = concatMap helper (permutations dice)
  where helper :: [Int] -> [(Side, Side)]
        helper = map (\(a, b, _) -> (a, b)) . foldl' f [(side0, side1, False)]
          where f acc die =
                  do (side0', side1', passed) <- acc
                     let moves = generateMoves side0' side1' die
                     if null moves
                        then return (side0', side1', True)
                        else if passed
                               then []
                               else do (from, to) <- moves
                                       let (from', to') = applyMove side0' side1' from to
                                       return (from', to', False)

runMatch :: Player -> Player -> IO Color
runMatch whitePlayer redPlayer =
    do (whiteDie, redDie) <- differentDice
       send whitePlayer "white"
       send redPlayer   "red"
       let loop :: Player -> Player ->
                   Color  -> Color  ->
                   Side   -> Side   ->
                   Int    -> Int    -> IO Color
           loop player0 player1 color0 color1 side0 side1 die0 die1 =
               do sendDice player0 die0 die1
                  sendDice player1 die0 die1
                  --print color0
                  --print $ "d " ++ show die0 ++ " " ++ show die1
                  moves <- getMoves player0
                  let (side0', side1') = foldl' applyMove' (side0, side1) moves
                  let dice =
                        if die0 == die1
                           then map (const die0) [1..4]
                           else [die0, die1]
                  let states = possibleStates dice side0 side1
                  if (side0', side1') `elem` states
                     then if won side0'
                              then return color0
                              else do mapM_ (uncurry $ sendMove player1) moves
                                      send player1 "e"
                                      die0' <- rollDie
                                      die1' <- rollDie
                                      loop player1 player0
                                           color1 color0
                                           side1' side0'
                                           die0' die1'
                     else do print (side0, side1)
                             print (side0', side1')
                             print states
                             error "invalid moves"
       let board = newGame
       let (player0, player1, color0, color1, side0, side1) =
             if whiteDie > redDie
                then (whitePlayer, redPlayer, White, Red, white board, red board)
                else assert (redDie > whiteDie) $ (redPlayer, whitePlayer, Red, White, red board, white board)
       loop player0 player1 color0 color1 side0 side1 whiteDie redDie
    where sendDice p die0 die1 = send p $ "d " ++ show die0 ++ " " ++ show die1
          sendMove p from to   = send p $ "m " ++ show from ++ " " ++ show to
          applyMove' (us, them) = uncurry $ applyMove us them
          getMoves :: Player -> IO [(Int, Int)]
          getMoves p = go
            where go =
                    do s <- receive p
                       --print s
                       let trim = let f = reverse . dropWhile isSpace in f . f
                       let s' = trim s
                       if s' == "e"
                           then return []
                           else case s' of
                                  ('m':s'') -> case reads s'' of
                                                 [(x, s'')] ->
                                                   let s''' = trim s''
                                                   in case reads s''' of
                                                        [(y, "")] ->
                                                          fmap ((f x, f y):) go
                                                        [] -> error "blah"
                                                 [] -> error "blah"
                                  _ -> error "blah"
                  f 255 = -1
                  f x = x
main :: IO ()
main = do [white, red] <- getArgs
          putStrLn $ "White: " ++ show white
          putStrLn $ "Red: " ++ show red
          let go i w = do whitePlayer <- runInteractiveCommand white >>= f
                          redPlayer <-   runInteractiveCommand red >>= f
                          color <- runMatch whitePlayer redPlayer
                          putStrLn $ show i ++ ": " ++ show color
                          let i' = i + 1
                          if i' == games
                             then return w
                             else if color == White
                                      then go i' (w + 1)
                                      else go i' w
          w <- go 0 0
          putStrLn $ "White: " ++ show w
          putStrLn $ "Red: " ++ show (games - w)
  where f (stdin', stdout', stderr', _) = do g stdin'
                                             return $  Player {
                                                         stdin  = stdin',
                                                         stdout = stdout',
                                                         stderr  = stderr'
                                                       }
          where g = flip hSetBuffering NoBuffering
        games = 200
