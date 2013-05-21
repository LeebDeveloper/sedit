module Main
    ( main, createStmt
    ) where


import System.Environment
import Data.List
import Text.ParserCombinators.Parsec

data InputArgs = InputArgs
    {   
        prevVsn :: FilePath,
        currVsn :: FilePath,
        diff    :: Maybe FilePath
    } deriving (Show)


main = do
    a <- getArgs
    case parseArgs a of
        Just ia -> putStrLn $ show ia
        Nothing -> putStrLn "Usage:\nsedit <previous version> <current version> [<output diff>]"
    

parseArgs :: [String] -> Maybe InputArgs
parseArgs (prev:curr:xs) = Just InputArgs
    {
        prevVsn = prev,
        currVsn = curr,
        diff    = head' xs
    }
parseArgs _ = Nothing


head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x


data SqlColumnType = 
    SimpleType {
        typeName     :: String,
        typeSize     :: Int,
        typeUnsigned :: Bool}
    | Enum {values :: [String]}
    deriving (Show)

data SqlDefaultValue = Null | NotNull | DefaultValue { value :: String} deriving (Show)

data SqlColumn = SqlColumn
    {
        colName     :: String,
        colType     :: SqlColumnType,
        colDefault  :: SqlDefaultValue
    } deriving (Show)


data SqlTable = SqlTable
    {
        tabName    :: String,
        tabEngine  :: String,
        tabCharset :: String ,
        tabColumns :: [SqlColumn]
    } deriving (Show)


space' = many space
identifier = many (alphaNum <|> char '_')

createStmt :: Parser SqlTable
createStmt = do 
    { 
        space'; string "CREATE"; space'; string "TABLE"; space'; 
        tableName <- identifier; space';
        char '(';
        columns <- columnListStmt;
        char ')';
        engineName  <- engineStmt;
        charsetName <- charsetStmt;
        return SqlTable {tabName = tableName, tabEngine = engineName, tabCharset = charsetName, tabColumns = columns}
    }

engineStmt = do 
    {
        space'; string "ENGINE"; space'; char '='; space';
        engineName <- identifier;
        return engineName
    }

charsetStmt = do
    {
        space'; string "DEFAULT"; space'; string "CHARSET"; space'; char '='; space';
        charsetName <- identifier;
        return charsetName
    }

columnListStmt = sepBy columnStmt (char ',')


columnStmt :: Parser SqlColumn
columnStmt = do
    {
        space'; char '`';
        ident    <- identifier;
        char '`';
        ctype    <- columTypeStmt;
        cdefault <- columnDefaultStmt;
        return SqlColumn {colName = ident, colType = ctype, colDefault = cdefault}
    }


columTypeStmt = do 
    {
        return SimpleType {typeName = "bigint", typeSize = 20, typeUnsigned = True}
    }

columnDefaultStmt = do
    {
        return NotNull
    }