module Sedit.SqlParser ( createStmt ) where


import Text.ParserCombinators.Parsec
import Sedit.SqlTypes

data TableContentDecl = ColumnDecl SqlColumn | IndexDecl SqlColumnIndex deriving (Show)

space'     = many space

idstring   = many1 (alphaNum <|> char '_')
identifier = do 
    {
        space'; char '`';
        i <- idstring;
        char '`'; space';
        return i;
    }

createStmt :: Parser SqlTable
createStmt = do 
    { 
        space'; string "CREATE"; space'; string "TABLE"; space'; 
        tableName <- identifier; space';
        char '('; space';
        content <- contentStmt;
        space'; char ')';
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
        engineName <- idstring;
        return engineName
    }

charsetStmt = do
    {
        space'; string "DEFAULT"; space'; string "CHARSET"; space'; char '='; space';
        charsetName <- idstring;
        return charsetName
    }


contentStmt = sepBy (choice [indexStmt, columnStmt]) (do {char ','; space';})


columnStmt = do
    {
        ident    <- identifier;
        ctype    <- columTypeStmt;
        cdefault <- columnDefaultStmt;
        return (ColumnDecl $ SqlColumn {colName = ident, colType = ctype, colDefault = cdefault})
    }


columTypeStmt = choice [enumStmt, primitiveTypeStmt]


enumStmt = do
    {
        space'; string "enum"; space'; char '(';
        v <- sepBy  (do {
                        space'; char '\''; 
                        s <- many1 (noneOf "'");
                        char '\''; space';
                        return s}) 
                    (char ',');
        char ')'; space';
        return Enum { values = v}
    }


primitiveTypeStmt = do 
    {
        space';
        name     <- idstring;
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
        return PrimitiveType {typeName = name, typeSize = size, typeUnsigned = (unsigned == "unsigned")}
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
        primary <- option "" (string "PRIMARY");
        unique  <- option "" (string "UNIQUE");
        space'; string "KEY"; space';
        name    <- optionMaybe identifier;
        space'; char '(';
        columns <- sepBy identifier (char ',');
        char ')';
        return (IndexDecl $ SqlColumnIndex {
                                indexName = name, 
                                indexColumns = columns, 
                                indexPrimary = (primary == "PRIMARY"),
                                indexUnique  = (unique == "UNIQUE")});
    }
