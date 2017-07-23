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
              Add : SchemaType schema -> Command schema
              Get : Integer -> Command schema
              -- | Size
              -- | Search
              Quit : Command schema

parseCommand : (schema: Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = Just (Add (?parseBySchema rest))
parseCommand schema "get" val = case all isDigit (unpack val) of
                          False => Nothing
                          True => Just (Get (cast val))
-- parseCommand "size" _ = Just Size
-- parseCommand "search" str = Just (Search str)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing


parse : (schema: Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
              case span (/= ' ') input of
                (cmd, args) => parseCommand schema cmd (ltrim args)

getEntry : (x : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry x store =  case integerToFin x (size store) of
                          Nothing => Just ("Out of Range\n", store)
                          (Just pos) => Just (?diplay (index pos (items store)) ++ "\n", store)


search: (x : String) -> (storeItems : Vect n String) -> (accumulator : String) -> String
search {n = Z} x [] accumulator = accumulator
search {n = (S len)} x (y :: ys) accumulator = case isInfixOf x y of
  False => search x ys accumulator
  True => search x ys accumulator ++ (show len) ++ " " ++ y ++ "\n"



processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                          Nothing => Just ("Invalid command\n", store)
                          Just (Add x) => Just ("ID " ++ show (size store) ++ "\n", addToStore store  x)
                          Just (Get x) => getEntry x store
                          -- Just (Size) => Just ("Size is " ++ show (size store) ++ "\n", store)
                          -- Just (Search x) => Just (search x (items store) "", store)
                          Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
