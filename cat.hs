-- ファイルの内容を連結して標準出力に出力する。

import System.Environment (getArgs)
import System.IO (getContents, readFile)
import Data.List (intersperse, nub, (\\))
import Text.Printf (printf)

cat :: (a, b) -> [String] -> IO ()
cat _ [] = return ()
cat options files = do loop 1 options files
  where
    contentsOf f = if f == "-" then getContents else readFile f
    loop _ _ [] = return ()
    loop n opts (f:fs)  = do
      contents <- contentsOf f
      let contents'= fst opts n . lines . snd opts $ contents
          n' = length contents' + 1
      putStr $ unlines contents'
      loop (n') opts fs



parse :: [String] -> ((a, b), [String])
parse [] = ([], ["-"])
parse args = (setFunc options, files)
  where
    args' = [x | x <- args, head x == '-' , length x >= 2]
    options = nub . concat . map (words . intersperse ' ' . tail) $ args'
    files = args \\ options

setFunc :: [String] -> (a, b)
setFunc [] = (id, id)
setFunc opt = (showNum x, f opt)
  where
    f [] = id
    f (x:xs)
      | x == "s" = flip (.) squeeze (f xs)
      | x == "E" = flip (.) showEnd (f xs)
      | x == "T" = flip (.) showTab (f xs)
      | x == "v" = flip (.) showNonP (f xs)

showNum :: String -> Int -> [String] -> [String]
showNum opt n str
  | opt == "n" = addLineNum n str
  | opt == "b" = if str = "" then "" else addLineNum n str
  where
    addLineNum m sx = zipWith (\i s -> printf "%6d %s" i s) [m..] sx

main = do
  args <- getArgs
  let (options, files) = parse args
      optfunc = selectFunc options
  cat options files

{-
--import System.Console.GetOpt
--import System.IO (getContents, readFile, stderr, stdout, BufferMode,
--                  isEOF,hSetBuffering, BufferMode(NoBuffering), hPutStrLn)
--import System.Exit
--import System.Environment
--import Data.List
--import Data.Char
--import Control.Monad
--import Text.Printf
-- 
--main = do
--    (args, files) <- getArgs >>= parse
--    when (Unbuffered `elem` args) $ hSetBuffering stdout NoBuffering
--    mapM_ (cat args) files
-- 
--withFile s f = putStr . unlines . f . lines =<< open s
--  where
--    open f = if f == "-" then getContents else readFile f
-- 
--cat [] f = withFile f id
--cat as f = withFile f (newline . number . visible as)
--  where
--    number  s    = if Blanks `elem` as then numberSome s else ifset Number numberAll s
--    newline s    = ifset Dollar (map (++"$")) s
--    visible as s = foldl' (flip render) s as
--    ifset a f    = if a `elem` as then f else id
-- 
--render Squeeze   = map head. groupBy (\x y -> all (all isSpace) [x,y])
--render Tabs      = map $ concatMap (\c -> if c == '\t' then "^I" else [c])
--render Invisible = map $ concatMap visible
--  where
--    visible c | c == '\t' || isPrint c = [c]
--              | otherwise              = init . tail . show $ c
--render _ = id
-- 
--numberLine      = printf "%6d  %s"
--numberAll s     = zipWith numberLine [(1 :: Integer)..] s
--numberSome s    = reverse . snd $ foldl' draw (1,[]) s
--  where
--    draw (n,acc) s
--            | all isSpace s = (n,   s : acc)
--            | otherwise     = (n+1, numberLine n s : acc)
-- 
--data Flag
--    = Blanks                -- -b
--    | Dollar                -- -e 
--    | Squeeze               -- -s
--    | Tabs                  -- -t
--    | Unbuffered            -- -u
--    | Invisible             -- -v
--    | Number                -- -n
--    | Help                  -- --help
--    deriving (Eq,Ord,Enum,Show,Bounded)
-- 
--flags =
--   [Option ['b'] []       (NoArg Blanks)
--        "Implies the -n option but doesn't count blank lines."
--   ,Option ['e'] []       (NoArg Dollar)
--        "Implies the -v option and also prints a dollar sign (`$') at the end of each line."
--   ,Option ['n'] []       (NoArg Number)
--        "Number the output lines, starting at 1."
--   ,Option ['s'] []       (NoArg Squeeze)
--        "Squeeze multiple adjacent empty lines, causing the output to be single spaced."
--   ,Option ['t'] []       (NoArg Tabs)
--        "Implies the -v option and also prints tab characters as `^I'."
--   ,Option ['u'] []       (NoArg Unbuffered)
--        "The output is guaranteed to be unbuffered (see setbuf(3))."
--   ,Option ['v'] []       (NoArg Invisible)
--        "Displays non-printing characters so they are visible."
--   ,Option []    ["help"] (NoArg Help)
--        "Print this help message"
--   ]
-- 
--parse argv = case getOpt Permute flags argv of
--    (args,fs,[]) -> do
--        let files = if null fs then ["-"] else fs
--        if Help `elem` args
--            then do hPutStrLn stderr (usageInfo header flags)
--                    exitWith ExitSuccess
--            else return (nub (concatMap set args), files)
-- 
--    (_,_,errs)      -> do
--        hPutStrLn stderr (concat errs ++ usageInfo header flags)
--        exitWith (ExitFailure 1)
-- 
--    where header = "Usage: cat [-benstuv] [file ...]"
-- 
--          set Dollar = [Dollar, Invisible]
--          set Tabs   = [Tabs,   Invisible]
--          set f      = [f]
-}
