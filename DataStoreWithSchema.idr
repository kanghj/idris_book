module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)


-- data DataStore : Type where
  -- MkData : (schema : Schema) -> (size : Nat) -> (items : Vect size (SchemaType schema)) -> DataStore
record DataStore where
  constructor MkData
  schema : Schema
  size: Nat
  items: Vect size (SchemaType schema)

--
-- size : DataStore -> Nat
-- size (MkData schema' size' items') = size'
--
-- items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
-- items (MkData schema' size' items') = items'


addToStore : (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs


data Command : Schema -> Type where
              SetSchema : (newschema : Schema) -> Command schema
              Add : SchemaType schema -> Command schema
              Get : Integer -> Command schema
              -- | Size
              -- | Search
              Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
        case span (/= '"') xs of
          (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
          _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt x = case span isDigit x of
                        ("", rest) => Nothing
                        (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schema1 .+. schema2) input =
  case parsePrefix schema1 input of
    Nothing => Nothing
    (Just (l_val, input')) => case parsePrefix schema2 input' of
      Nothing => Nothing
      Just (r_val, input'') => Just ((l_val, r_val), input'')



parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema rest = case parsePrefix schema rest of
                                 Just (res, "") => Just res
                                 {- parsePrefix scceeds but there's input remaining => more input than schema requires -}
                                 Just _ => Nothing
                                 Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) =
  case xs of
    [] => Just SString
    _ => case parseSchema xs of
           Nothing => Nothing
           (Just xs_sch) => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) =
  case xs of
    [] => Just SInt
    _ => case parseSchema xs of
          Nothing => Nothing
          (Just xs_sch) => Just (SInt .+. xs_sch)
parseSchema _ = Nothing


parseCommand : (schema: Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                  Nothing => Nothing
                                  Just restok => Just (Add restok)
parseCommand schema "get" val = case all isDigit (unpack val) of
                          False => Nothing
                          True => Just (Get (cast val))
-- parseCommand "size" _ = Just Size
-- parseCommand "search" str = Just (Search str)
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest
        = do schemaok <- parseSchema (words rest)
             Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing


parse : (schema: Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
              case span (/= ' ') input of
                (cmd, args) => parseCommand schema cmd (ltrim args)

display: (SchemaType schema) -> String
display {schema = SString} x = show x
display {schema = SInt} x = show x
display {schema = (y .+. z)} (item1, item2) = (display item1) ++ ", " ++ (display item2)

getEntry : (x : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry x store =  case integerToFin x (size store) of
                          Nothing => Just ("Out of Range\n", store)
                          (Just pos) => Just (display (index pos (items store)) ++ "\n", store)


search: (x : String) -> (storeItems : Vect n String) -> (accumulator : String) -> String
search {n = Z} x [] accumulator = accumulator
search {n = (S len)} x (y :: ys) accumulator = case isInfixOf x y of
  False => search x ys accumulator
  True => search x ys accumulator ++ (show len) ++ " " ++ y ++ "\n"

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                          Z => Just (MkData schema _ [])
                          S k => Nothing



processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                          Nothing => Just ("Invalid command\n", store)
                          Just (Add x) => Just ("ID " ++ show (size store) ++ "\n", addToStore store x)
                          Just (Get x) => getEntry x store
                          -- Just (Size) => Just ("Size is " ++ show (size store) ++ "\n", store)
                          -- Just (Search x) => Just (search x (items store) "", store)
                          Just (SetSchema schema') =>
                            case setSchema store schema' of
                              Nothing => Just ("Can't update schema\n", store)
                              Just store' => Just ("Ok\n", store')
                          Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
