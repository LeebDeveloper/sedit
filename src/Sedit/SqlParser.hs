module Sedit.SqlParser ( createStmt ) where


import Text.ParserCombinators.Parsec
import Sedit.SqlTypes

data TableContentDecl = ColumnDecl SqlColumn | IndexDecl SqlColumnIndex deriving (Show)

space'     = many space
identifier = many1 (alphaNum <|> char '_')

createStmt :: Parser SqlTable
createStmt = do 
    { 
        space'; string "CREATE"; space'; string "TABLE"; space'; 
        tableName <- identifier; space';
        char '(';
        content <- contentStmt;
        char ')';
        engineName  <- engineStmt;
        charsetName <- charsetStmt;
        return SqlTable {
            tabName = tableName, 
            tabEngine = engineName, 
            tabCharset = charsetName, 
            tabColumns = [x | ColumnDecl x <- content],
            tabIndexes = [x | IndexDecl x <- content]
        }
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


contentStmt = sepBy (choice [indexStmt, columnStmt]) (char ',')


columnStmt = do
    {
        space'; char '`';
        ident    <- identifier;
        char '`';
        ctype    <- columTypeStmt;
        cdefault <- columnDefaultStmt;
        return (ColumnDecl $ SqlColumn {colName = ident, colType = ctype, colDefault = cdefault})
    }


columTypeStmt = do 
    {
        space';
        name     <- identifier;
        space';
        size     <- option 0 (do {
                space'; char '('; space';
                val <- many1 digit;
                space'; char ')'; space';
                return (read val)
            });
        space';        
        unsigned <- option "" (string "unsigned");
        space';
        return SimpleType {typeName = name, typeSize = size, typeUnsigned = (unsigned == "unsigned")}
    }

columnDefaultStmt = do
    {
        space';
        defaultValue <- choice [defaultValueStmt, notNullStmt];
        space';
        return defaultValue
    }


notNullStmt = do {string "NOT"; space'; string "NULL"; return NotNull;}
defaultValueStmt = do { 
        string "DEFAULT"; space';
        val <- choice [nullStmt, intValueStmt, strValue1Stmt, strValue2Stmt];
        return val
    }

nullStmt      = do {string "NULL"; return Null}
intValueStmt  = do {val <- many1 digit; return DefaultValue{value = val}}
strValue1Stmt = do {char '"'; val <- many (noneOf ['"']); char '"'; return DefaultValue{value = val}}
strValue2Stmt = do {char '\''; val <- many (noneOf ['\'']); char '\''; return DefaultValue{value = val}}

indexListStmt = do
    {
        char ',';
        idxs <- sepBy indexStmt (char ',');
        return idxs
    }

indexStmt = do
    {
        space';
        primary <- option "" (string "PRIMARY");
        space'; string "KEY"; space'; char '`';
        name    <- identifier;
        char '`'; space'; char '(';
        columns <- sepBy (do {space'; char '`'; i <- identifier; char '`'; space'; return i}) (char ',');
        char ')';
        return (IndexDecl $ SqlColumnIndex {indexName = name, indexColumns = columns, indexPrimary = (primary == "PRIMARY")});
    }
