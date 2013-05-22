module Sedit.SqlTypes (
            SqlTable(..),
            SqlColumn(..),
            SqlColumnType(..),
            SqlDefaultValue(..),
            SqlColumnIndex(..)
        ) where


data SqlColumnIndex = SqlColumnIndex
    {
        indexName    :: String,
        indexColumns :: [String],
        indexPrimary :: Bool
    } deriving (Show)

data SqlColumnType = 
    SimpleType {
        typeName     :: String,
        typeSize     :: Int,
        typeUnsigned :: Bool}
    | Enum {values :: [String]}
    deriving (Show)

data SqlDefaultValue = Null | NotNull | DefaultValue {value :: String} deriving (Show)

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
        tabColumns :: [SqlColumn],
        tabIndexes :: [SqlColumnIndex]
    } deriving (Show)