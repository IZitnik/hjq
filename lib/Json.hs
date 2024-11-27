module Json where
import JsonParse (parseJSON')
import JsonStructure

parseJSON :: String -> Maybe JSON
parseJSON str = do (json, _) <- parseJSON' str
                   Just json
