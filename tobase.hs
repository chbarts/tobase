import System.IO
import System.Exit
import System.Environment
import Numeric.Natural

digits :: String
digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

lendig :: Integer
lendig = toInteger (length digits)

inner :: Natural -> Natural -> String -> String
inner 0   _    ""  = "0"
inner 0   _    acc = acc
inner num base acc = inner (div num base) base ([(digits !! (fromIntegral (mod num base)))] ++ acc)

tobase :: Natural -> Natural -> String
tobase num base = inner num base []

help :: IO ()
help = do
  putStrLn "tobase base nums..."
  putStrLn "Print positive integers in a given positive integral base"

version :: IO ()
version = putStrLn "tobase version 1.0"

trynat :: String -> String -> IO Natural
trynat tag str = case ((reads str)::[(Natural, String)]) of
  [(val, _)] -> return val
  _          -> hPutStrLn stderr ("tobase: " ++ tag ++ " " ++ str ++ " not a valid natural number") >> exitFailure

dotobase :: Natural -> String -> IO ()
dotobase base str = do
  val <- trynat "number" str
  putStrLn (tobase val base)

getbase :: String -> IO Natural
getbase str = do
  val <- trynat "base" str
  if ((toInteger val) <= 1) 
    then (hPutStrLn stderr ("tobase: base " ++ str ++ " invalid") >> exitFailure) 
    else if ((toInteger val) > lendig) 
         then (hPutStrLn stderr ("tobase: base " ++ str ++ " too large") >> exitFailure) 
         else return val

parse :: [String] -> IO ()
parse ["-h"]        = help
parse ["--help"]    = help
parse ["-v"]        = version
parse ["--version"] = version
parse []            = help >> (exitWith ExitSuccess)
parse (bstr:vals)   = do { base <- getbase bstr; mapM_ (\s -> dotobase base s) vals; }

main :: IO ()
main = getArgs >>= parse >> (exitWith ExitSuccess)
