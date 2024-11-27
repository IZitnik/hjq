module Main where
import System.Environment (getArgs)
import Json


main :: IO ()
main = do args <- getArgs
          if args == []
            then putStrLn "arg filename required"
            else do json <- readFile (head args)
                    case parseJSON json of
                      Just j  -> print j
                      Nothing -> putStrLn "Invalid JSON"
