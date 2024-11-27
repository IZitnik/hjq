module JsonParse where
import JsonStructure

whitespace :: Char -> Bool
whitespace ' '  = True
whitespace '\t' = True
whitespace '\n' = True
whitespace '\r' = True
whitespace _    = False

parseEscape :: String -> Maybe (Char, String)
parseEscape []          = Nothing
parseEscape ('u':rest)  = undefined
parseEscape (c:rest) = if c `elem` "\"\\/bfnrt"
                        then Just (c, rest)
                        else Nothing

parseString :: String -> Maybe (String, String)
parseString []          = Nothing
parseString ('"':rest)  = Just ("", rest)
parseString ('\\':rest) = do (escaped, rest') <- parseEscape rest
                             (str, rest'')    <- parseString rest'
                             Just (escaped:str, rest'')
parseString (c:rest)    = do (str, rest') <- parseString rest
                             Just (c:str, rest')

parseList :: String -> Maybe (JSON, String)
parseList [] = Nothing
parseList (c:rest)
  | whitespace c = parseList rest
  | c == ']'     = Just (Array [], rest)
parseList str = do (json, rest) <- parseJSON' str
                   (c, rest')   <- Just (head rest, tail rest)
                   case c of
                     ']' -> Just (Array [json], rest')
                     ',' -> do (Array jsons, rest'') <- parseList rest'
                               Just (Array (json:jsons), rest'')
                     _   -> Nothing

parseObj :: String -> Maybe (JSON, String)
parseObj []  = Nothing
parseObj str = do (String key, (':':rest)) <- parseJSON' str
                  (json, (c:rest'))        <- parseJSON' rest
                  case c of
                    '}' -> Just (Object [(key, json)], rest')
                    ',' ->
                       do (Object objs, rest'') <- parseObj rest'
                          Just (Object ((key, json):objs), rest'')
                    _   -> Nothing

parseJSON' :: String -> Maybe (JSON, String)
parseJSON' ('n':'u':'l':'l':rest)     = Just (Null, rest)
parseJSON' ('t':'r':'u':'e':rest)     = Just (Bool True, rest)
parseJSON' ('f':'a':'l':'s':'e':rest) = Just (Bool False, rest)
parseJSON' ('"':rest) = do (str, rest') <- parseString rest
                           Just (String str, rest')
parseJSON' ('[':rest) = parseList rest
parseJSON' ('{':rest) = parseObj rest
parseJSON' (c:rest)
  | whitespace c = parseJSON' rest
  | otherwise    = undefined -- TODO: parse number
parseJSON' [] = Nothing
