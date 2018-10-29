-- ファイルの内容を連結して標準出力に出力する。

import System.Environment (getArgs)
import System.IO (getContents, readFile)
import Data.List (intersperse, nub, (\\))
import Data.Char (ord, chr)
import Text.Printf (printf)


main = do
  args <- getArgs
  let (options, files) = parse args
      optfunc = setFunc options
  cat optfunc files


cat :: (Int -> [String] -> [String], String -> String) -> [String] -> IO ()
cat _ [] = return ()
cat (func1, func2) files = do loop 1 (func1, func2) files
  where
    loop _ _ [] = return ()
    loop n (f1, f2) (f:fs)  = do
      contents <- contentsOf f
      let contents'= (f1 n) . lines .f2 $ contents
          n' = length contents' + 1
      putStr $ unlines contents'
      loop (n') (f1, f2) fs
    contentsOf f = if f == "-" then getContents else readFile f


parse :: [String] -> ([String], [String])
parse [] = ([], ["-"])
parse args = (options, files)
  where
    args' = [x | x <- args, head x == '-' , length x >= 2]
    options = nub . concat . map (words . intersperse ' ' . tail) $ args'
    files = args \\ args'


setFunc :: [String] -> (Int -> [String] -> [String], String -> String)
setFunc [] = (\_ s -> s, \s -> s)
setFunc opts = (f opts, f' opts)
  where
    f [] = (\_ s -> s) 
    f (x:xs)
      | x == "n" || x == "b" = (showNum x) 
      | otherwise            = f xs

    f' [] = id 
    f' (x:xs)
      | x == "s"  = flip (.) squeeze (f' xs)
      | x == "E"  = flip (.) showEnd (f' xs)
      | x == "T"  = flip (.) showTab (f' xs)
      | x == "v"  = flip (.) showNonP (f' xs)
      | otherwise = f' xs


squeeze :: String -> String
squeeze [] = []
squeeze (x:xs)
  | x == '\n' = "\n" ++ f xs
  | otherwise =  [x] ++ squeeze xs
  where f [] = []
        f [s] = "\n"
        f (s:t:us)
          | s == '\n' && t == '\n' = f (t:us)
          | otherwise = [s] ++ squeeze (t:us)


showNum :: String -> Int -> [String] -> [String]
showNum _ _ [] = []
showNum opt n (x:xs)
  | opt == "b" && null x = [x] ++ showNum opt n xs
  | otherwise            = [addLineNum n x] ++ showNum opt (n + 1) xs
  where
    addLineNum i s = printf "%6d %s" i s


showEnd :: String -> String
showEnd [] = []
showEnd (x:xs)
  | x == '\n' = "$\n" ++ showEnd xs
  | otherwise =  [x]  ++ showEnd xs


showTab :: String -> String
showTab [] = []
showTab (x:xs)
  | x == '\t' = "^I" ++ showTab xs
  | otherwise =  [x] ++ showTab xs


showNonP :: String -> String
showNonP [] = []
showNonP (x:xs)
  | isCtrChar x  = showCtrChar x ++ showNonP xs
  | isMetaChar x = showMetaChar x ++ showNonP xs
  | otherwise    = [x] ++ showNonP xs


isCtrChar :: Char -> Bool
isCtrChar x
  | 0x00 <= code && code <= 0x08 = True
  | 0x0b <= code && code <= 0x1f = True
  | code == 0x7f                 = True
  | otherwise                    = False
  where code = ord x


showCtrChar :: Char -> String
showCtrChar x
  | code == 0x7f = "^?"
  | otherwise    = "^" ++ [list !! code]
  where code = ord x
        list  = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"


isMetaChar :: Char -> Bool
isMetaChar x
  | 0x80 <= code && code <= 0xff = True
  | otherwise                    = False
  where code = ord x


showMetaChar :: Char -> String
showMetaChar x
  | 0x80 <= code && code <= 0x9f = "M-^" ++ [list !! i]
  | code == 0xff = "M-^?"
  | otherwise = "M-" ++ [chr i]
  where code  = ord x
        i     = code - 0x80
        list  = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"

