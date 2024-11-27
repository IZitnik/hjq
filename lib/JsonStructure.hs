module JsonStructure where

data JSON = Null
          | Bool   Bool
          | Number Double
          | String String
          | Array  [JSON]
          | Object [(String, JSON)]
instance Show JSON where
  show Null         = "null"
  show (Bool b)     = show b
  show (Number n)   = show n
  show (String s)   = show s
  show (Array a)    = "[" ++ strJoin ", " (map show a) ++ "]"
  show (Object o)   = "{" ++ strJoin ", " (map showObj o) ++ "}"
    where showObj :: (String, JSON) -> String
          showObj (key, con) = (show key) ++ ": " ++ (show con)

strJoin :: String -> [String] -> String
strJoin _ []     = ""
strJoin _ [x]    = x
strJoin s (x:xs) = x ++ s ++ (strJoin s xs)
