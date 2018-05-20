import Data.Char
import System.IO
import System.Exit
import System.Environment
import Numeric.Natural

digits :: String
digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

lendig :: Integer
lendig = toInteger (length digits)

findpos :: Char -> String -> Natural
findpos c str = finner c str 0
  where
    finner _ ""     a = a
    finner c (x:xs) a = if c == x then a else finner c xs (a + 1)

inner :: String -> Natural -> Natural -> Either String Natural
inner ""     _    0   = Right 0
inner ""     _    acc = Right acc
inner (x:xs) base acc = case digit of
                          Right d -> inner xs base ((acc * base) + d)
                          Left s  -> Left s
  where
    digit = if (dval >= base)
            then (Left ("frombase: " ++ (show x) ++ " not a valid digit in base " ++ (show base)))
            else Right dval
    dval = if (base <= 35)
           then findpos (toUpper x) digits
           else findpos x digits

frombase :: String -> Natural -> Either String Natural
frombase num base = inner num base 0

help :: IO ()
help = do
  putStrLn "frombase base nums..."
  putStrLn "Print positive integers in a given positive integral base as positive integers in base ten"
  putStrLn ("Digits are: " ++ digits)
  putStrLn "For bases 35 and below, lowercase letters a-z are mapped to uppercase letters A-Z"

version :: IO ()
version = putStrLn "frombase version 1.0"

trynat :: String -> String -> IO Natural
trynat tag str = case ((reads str)::[(Natural, String)]) of
  [(val, _)] -> return val
  _          -> hPutStrLn stderr ("frombase: " ++ tag ++ " " ++ str ++ " not a valid natural number") >> exitFailure

dofrombase :: Natural -> String -> IO ()
dofrombase base str = do
  case (frombase str base) of
    Right res -> putStrLn (show res)
    Left err  -> hPutStrLn stderr err

getbase :: String -> IO Natural
getbase str = do
  val <- trynat "base" str
  if ((toInteger val) <= 1) 
    then (hPutStrLn stderr ("frombase: base " ++ str ++ " invalid") >> exitFailure) 
    else if ((toInteger val) > lendig) 
         then (hPutStrLn stderr ("frombase: base " ++ str ++ " too large") >> exitFailure) 
         else return val

parse :: [String] -> IO ()
parse ["-h"]        = help
parse ["--help"]    = help
parse ["-v"]        = version
parse ["--version"] = version
parse []            = help >> (exitWith ExitSuccess)
parse (bstr:vals)   = do { base <- getbase bstr; mapM_ (\s -> dofrombase base s) vals; }

main :: IO ()
main = getArgs >>= parse >> (exitWith ExitSuccess)
