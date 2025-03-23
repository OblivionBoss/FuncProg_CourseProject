{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L

-- Define our parser type
type Parser = Parsec Void Text

-- Data types to represent different query commands
data Query = Query
  { selectFieldsQ :: [Text],
    fromTableQ :: Text,
    whereClauseQ :: Maybe Text,
    groupByQ :: Maybe [Text],
    havingClauseQ :: Maybe Text,
    windowClauseQ :: Maybe (Text, Text),  -- window name and window definition
    valuesClauseQ :: Maybe [Text],
    compoundOperatorQ :: Maybe Text,
    orderByClauseQ :: Maybe [Text],
    limitClauseQ :: Maybe Int,
    offsetClauseQ :: Maybe Int,
    recursiveCTEQ :: Maybe Text,
    distinctQ :: Bool,
    allQ :: Bool,
    joinClauseQ :: Maybe Text
  }
  deriving (Show)

-- data CreateIndexData = CreateIndexData
--   { indexNameCIP :: Text,
--     tableNameCIP :: Text,
--     uniqueCIP :: Bool,
--     ifNotExistsCIP :: Bool,
--     schemaNameCIP :: Maybe Text,
--     indexedColumnsCIP :: [IndexedColumnData],
--     whereClauseCIP :: Maybe Text
--   } deriving (Show)

data IndexedColumnData = IndexedColumnData
  { columnName :: Text,
    collation :: Maybe Text,
    ascDesc :: Maybe Text
  } deriving (Show)

data Command
  = Select Query
  | AddColumn Text Text
  | AddConstraint Text Text
  | AlterTable Text
  | AlterColumn Text Text Text
  | DeleteFrom Text (Maybe Text)
  | CreateTable Text [(Text, Text)]
  | DropTable Text Bool -- Supports IF EXISTS
  | InsertInto Text [Text] [Text]
  | Update Text [(Text, Text)] (Maybe Text)
  | UpdateLimited Text [(Text, Text)] (Maybe Text) (Maybe Int) (Maybe Text)
  | Join Text Text Text (Maybe Text) -- Supports different join types
  | OrderBy Text Bool
  | Limit Int
  | CreateIndex Text Text Bool Bool (Maybe Text) [IndexedColumnData] (Maybe Text)
  | DropIndex Text Bool -- Supports IF EXISTS
  | CreateView Text Query
  | DropView Text Bool -- Supports IF EXISTS
  | CreateProcedure Text Text
  | ExecProcedure Text
  | BackupDatabase Text
  | TruncateTable Text
  | GrantPermission Text Text Text
  | RevokePermission Text Text Text
  | Exists Text
  | ForeignKey Text Text Text
  | Having Text
  | PrimaryKey Text
  | Rownum Int
  | Set Text Text
  | Top Int
  | Union Bool Query Query
  | Values [Text]
  | View Text Query
  | DropConstraint Text Text
  | Analyze Text
  | Attach Text Text
  | Begin
  | BeginDeferred
  | BeginImmediate
  | BeginExclusive
  | Commit
  | CreateTrigger Text Text Query
  | CreateVirtualTable Text Text
  | Detach Text
  | Pragma Text
  | Reindex (Maybe Text)
  | Release Text
  | Rollback (Maybe Text)
  | Savepoint Text
  | Vacuum
  deriving (Show)

-- Parser helpers

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

identifier :: Parser Text
identifier = pack <$> some (letterChar <|> char '_')

stringLiteral :: Parser Text
stringLiteral = pack <$> (char '\'' *> manyTill L.charLiteral (char '\''))

valueParser :: Parser Text
valueParser = stringLiteral <|> (pack <$> some digitChar) <|> identifier

-- Query parsers

selectParser :: Parser Query
selectParser = do
  recursiveCTE <- optional (symbol "WITH RECURSIVE" *> lexeme identifier)
  _ <- symbol "SELECT"
  distinct <- option False (symbol "DISTINCT" >> return True)
  all' <- option False (symbol "ALL" >> return True)
  fields <- sepBy (lexeme identifier) (symbol ",")
  _ <- symbol "FROM"
  table <- lexeme identifier
  joinClause <- optional (symbol "JOIN" *> lexeme (pack <$> someTill anySingle 
    (lookAhead 
      (   try (symbol "WHERE") 
      <|> try (symbol "GROUP BY") 
      <|> try (symbol "HAVING") 
      <|> try (symbol "WINDOW") 
      <|> try (symbol "VALUES") 
      <|> try (symbol "ORDER BY") 
      <|> try (symbol "LIMIT") 
      <|> ("" <$ eof)))))
  condition <- optional (symbol "WHERE" *> lexeme (pack <$> someTill anySingle 
    (lookAhead 
      (   try (symbol "GROUP BY") 
      <|> try (symbol "HAVING") 
      <|> try (symbol "WINDOW") 
      <|> try (symbol "VALUES") 
      <|> try (symbol "ORDER BY") 
      <|> try (symbol "LIMIT") 
      <|> ("" <$ eof)))))
  groupBy <- optional (symbol "GROUP BY" *> sepBy (lexeme identifier) (symbol ","))
  having <- optional (symbol "HAVING" *> lexeme (pack <$> someTill anySingle 
    (lookAhead 
      (   try (symbol "WINDOW") 
      <|> try (symbol "VALUES") 
      <|> try (symbol "ORDER BY") 
      <|> try (symbol "LIMIT") 
      <|> ("" <$ eof)))))
  windowClause <- optional (symbol "WINDOW" *> ((,) <$> 
    (lexeme identifier <* symbol "AS") <*> 
    lexeme (pack <$> someTill anySingle 
      (lookAhead 
        ( try (symbol "VALUES") 
        <|> try (symbol "ORDER BY") 
        <|> try (symbol "LIMIT") 
        <|> ("" <$ eof))))))
  valuesClause <- optional (symbol "VALUES" *> (symbol "(" *> sepBy (lexeme identifier) (symbol ",") <* symbol ")"))
  compoundOperator <- optional (lexeme (pack <$> someTill anySingle 
    (   try (symbol "ORDER BY") 
    <|> try (symbol "LIMIT") 
    <|> ("" <$ eof))))  
  orderByClause <- optional (symbol "ORDER BY" *> sepBy (lexeme identifier) (symbol ","))
  limitClause <- optional (symbol "LIMIT" *> L.decimal)
  offsetClause <- optional (symbol "OFFSET" *> L.decimal)
  return $ Query fields table condition groupBy having windowClause valuesClause compoundOperator orderByClause limitClause offsetClause recursiveCTE distinct all' joinClause

addColumnParser :: Parser Command
addColumnParser = do
  _ <- symbol "ALTER TABLE"
  _schemaName <- optional (lexeme identifier <* symbol ".")
  tableName <- lexeme identifier
  _ <- symbol "ADD" *> symbol "COLUMN"
  columnName <- lexeme identifier
  columnType <- lexeme (pack <$> some (letterChar <|> digitChar <|> char '(' <|> char ')'))
  return $ AddColumn tableName (columnName <> " " <> columnType) -- Combine name and type as a single string

alterColumnParser :: Parser Command
alterColumnParser = do
  _ <- symbol "ALTER TABLE"
  _schemaName <- optional (lexeme identifier <* symbol ".")
  tableName <- lexeme identifier
  _ <- symbol "ALTER" *> symbol "COLUMN"
  columnName <- lexeme identifier
  _ <- symbol "TO"
  newColumnName <- lexeme identifier
  newColumnType <- lexeme (pack <$> some (letterChar <|> digitChar <|> char '(' <|> char ')'))
  return $ AlterColumn tableName columnName (newColumnName <> " " <> newColumnType)

columnDefParser :: Parser (Text, Text)
columnDefParser = do
  colName <- lexeme identifier -- Parse column name
  colType <- lexeme identifier -- Parse column type (e.g., INT, VARCHAR)
  return (colName, colType)

createTableParser :: Parser Command
createTableParser = do
  _ <- symbol "CREATE TABLE"
  tableName <- lexeme identifier -- Expect table name
  _ <- symbol "(" -- Expect opening parenthesis
  columns <- columnDefParser `sepBy` symbol "," -- Parse column definitions
  _ <- symbol ")" -- Expect closing parenthesis
  return $ CreateTable tableName columns

insertIntoParser :: Parser Command
insertIntoParser = do
  _ <- symbol "INSERT INTO"
  table <- lexeme identifier
  _ <- symbol "("
  columns <- sepBy (lexeme identifier) (symbol ",")
  _ <- symbol ")"
  _ <- symbol "VALUES"
  _ <- symbol "("
  values <- sepBy (lexeme valueParser) (symbol ",")
  _ <- symbol ")"
  return $ InsertInto table columns values

updateParser :: Parser Command
updateParser = do
  _ <- symbol "UPDATE"
  table <- lexeme identifier
  _ <- symbol "SET"
  updates <- sepBy ((,) <$> lexeme identifier <*> (symbol "=" *> lexeme valueParser)) (symbol ",")
  condition <- optional (symbol "WHERE" *> lexeme (pack <$> someTill anySingle eof))
  return $ Update table updates condition

dropTableParser :: Parser Command
dropTableParser = do
  _ <- symbol "DROP TABLE"
  exists <- optional (symbol "IF EXISTS") -- Maybe Text
  table <- lexeme identifier -- Text
  let existsFlag = case exists of
        Just _ -> True
        Nothing -> False
  return $ DropTable table existsFlag

deleteFromParser :: Parser Command
deleteFromParser = do
    _ <- symbol "DELETE FROM"
    table <- lexeme identifier
    condition <- optional (symbol "WHERE" *> lexeme (pack <$> someTill anySingle eof))
    return $ DeleteFrom table condition

createIndexParser :: Parser Command
createIndexParser = do
  _ <- symbol "CREATE"
  uniqueCIP <- option False (symbol "UNIQUE" >> return True)
  _ <- symbol "INDEX"
  ifNotExistsCIP <- option False (symbol "IF" *> symbol "NOT" *> symbol "EXISTS" >> return True)
  schemaNameCIP <- optional (lexeme identifier <* symbol ".")
  indexNameCIP <- lexeme identifier
  _ <- symbol "ON"
  tableNameCIP <- lexeme identifier
  _ <- symbol "("
  indexedColumnsCIP <- sepBy indexedColumnParser (symbol ",")
  _ <- symbol ")"
  whereClauseCIP <- optional (symbol "WHERE" *> lexeme (pack <$> some anySingle)) -- Simplified expr parsing
  return $ CreateIndex indexNameCIP tableNameCIP uniqueCIP ifNotExistsCIP schemaNameCIP indexedColumnsCIP whereClauseCIP

indexedColumnParser :: Parser IndexedColumnData
indexedColumnParser = do
  columnName <- lexeme identifier
  collation <- optional (symbol "COLLATE" *> lexeme identifier)
  ascDesc <- optional (symbol "ASC" <|> symbol "DESC")
  return $ IndexedColumnData columnName collation ascDesc

dropIndexParser :: Parser Command
dropIndexParser = do
    _ <- symbol "DROP INDEX"
    exists <- optional (symbol "IF EXISTS")
    indexName <- lexeme identifier
    let existsFlag = case exists of
            Just _ -> True
            Nothing -> False
    return $ DropIndex indexName existsFlag

createViewParser :: Parser Command
createViewParser = do
    _ <- symbol "CREATE VIEW"
    viewName <- lexeme identifier
    _ <- symbol "AS"
    query <- selectParser
    return $ CreateView viewName query

dropViewParser :: Parser Command
dropViewParser = do
    _ <- symbol "DROP VIEW"
    exists <- optional (symbol "IF EXISTS")
    viewName <- lexeme identifier
    let existsFlag = case exists of
            Just _ -> True
            Nothing -> False
    return $ DropView viewName existsFlag

analyzeParser :: Parser Command
analyzeParser = do
  _ <- symbol "ANALYZE"
  schemaName <- optional (lexeme identifier <* symbol ".")
  tableName <- optional (lexeme identifier)
  indexOrTableName <- optional (lexeme identifier)
  return $ case (schemaName, tableName, indexOrTableName) of
    (Just schema, Nothing, Just tableOrIndex) -> Analyze (schema <> "." <> tableOrIndex)
    (Nothing, Just tableOrIndex, Nothing) -> Analyze tableOrIndex
    (Just schema, Nothing, Nothing) -> Analyze schema
    (Nothing, Nothing, Nothing) -> Analyze ""  -- Analyze the whole database
    _ -> error "Invalid ANALYZE statement" -- Handle other invalid combinations

attachParser :: Parser Command
attachParser = do
  _ <- symbol "ATTACH"
  _ <- symbol "DATABASE"
  filePath <- lexeme (pack <$> some anySingle) -- Parse any expression
  _ <- symbol "AS"
  schemaName <- lexeme identifier
  return $ Attach filePath schemaName

beginParser :: Parser Command
beginParser = do
  _ <- symbol "BEGIN"
  transactionType <- optional (symbol "DEFERRED" <|> symbol "IMMEDIATE" <|> symbol "EXCLUSIVE")
  _ <- optional (symbol "TRANSACTION")
  return $ case transactionType of
    Just "DEFERRED" -> BeginDeferred
    Just "IMMEDIATE" -> BeginImmediate
    Just "EXCLUSIVE" -> BeginExclusive
    Nothing -> Begin
    _ -> error "no more cases"

commitParser :: Parser Command
commitParser = do
  _ <- symbol "COMMIT" <|> symbol "END"
  _ <- optional (symbol "TRANSACTION")
  return Commit

createTriggerParser :: Parser Command
createTriggerParser = do
    _ <- symbol "CREATE TRIGGER"
    triggerName <- lexeme identifier
    _ <- symbol "BEFORE" <|> symbol "AFTER" <|> symbol "INSTEAD OF"
    _ <- symbol "INSERT" <|> symbol "UPDATE" <|> symbol "DELETE"
    _ <- symbol "ON"
    tableName <- lexeme identifier
    _ <- symbol "BEGIN"
    query <- selectParser
    _ <- symbol "END"
    return $ CreateTrigger triggerName tableName query

createVirtualTableParser :: Parser Command
createVirtualTableParser = do
    _ <- symbol "CREATE VIRTUAL TABLE"
    tableName <- lexeme identifier
    _ <- symbol "USING"
    moduleName <- lexeme identifier
    return $ CreateVirtualTable tableName moduleName

detachParser :: Parser Command
detachParser = do
    _ <- symbol "DETACH DATABASE"
    dbName <- lexeme identifier
    return $ Detach dbName

pragmaParser :: Parser Command
pragmaParser = do
    _ <- symbol "PRAGMA"
    pragmaName <- lexeme identifier
    return $ Pragma pragmaName

reindexParser :: Parser Command
reindexParser = do
    _ <- symbol "REINDEX"
    tableName <- optional (lexeme identifier)
    return $ Reindex tableName

releaseParser :: Parser Command
releaseParser = do
    _ <- symbol "RELEASE SAVEPOINT"
    savepointName <- lexeme identifier
    return $ Release savepointName

rollbackParser :: Parser Command
rollbackParser = do
  _ <- symbol "ROLLBACK"
  _ <- optional (symbol "TRANSACTION")
  savepointName <- optional (symbol "TO" *> symbol "SAVEPOINT" *> lexeme identifier)
  return $ Rollback savepointName

savepointParser :: Parser Command
savepointParser = do
    _ <- symbol "SAVEPOINT"
    savepointName <- lexeme identifier
    return $ Savepoint savepointName

updateLimitedParser :: Parser Command
updateLimitedParser = do
    _ <- symbol "UPDATE"
    table <- lexeme identifier
    _ <- symbol "SET"
    updates <- sepBy ((,) <$> lexeme identifier <*> (symbol "=" *> lexeme valueParser)) (symbol ",")
    condition <- optional (symbol "WHERE" *> lexeme (pack <$> someTill anySingle 
      (lookAhead 
        (   try (symbol "LIMIT") 
        <|> try (symbol "ORDER BY") 
        <|> ("" <$ eof)))))
    limitClause <- optional (symbol "LIMIT" *> L.decimal)
    orderByClause <- optional (symbol "ORDER BY" *> lexeme identifier)
    return $ UpdateLimited table updates condition limitClause orderByClause

vacuumParser :: Parser Command
vacuumParser = symbol "VACUUM" >> return Vacuum

commandParser :: Parser Command
commandParser =
  try (Select <$> selectParser)
    <|> try addColumnParser
    <|> try alterColumnParser
    <|> try dropTableParser
    <|> try createTableParser
    <|> try insertIntoParser
    <|> try updateParser
    <|> try deleteFromParser
    <|> try createIndexParser
    <|> try dropIndexParser
    <|> try createViewParser
    <|> try dropViewParser
    <|> try analyzeParser
    <|> try attachParser
    <|> try beginParser
    <|> try commitParser
    <|> try createTriggerParser
    <|> try createVirtualTableParser
    <|> try detachParser
    <|> try pragmaParser
    <|> try reindexParser
    <|> try releaseParser
    <|> try rollbackParser
    <|> try savepointParser
    <|> try updateLimitedParser
    <|> try vacuumParser
    

-- Running the parser (DONT TOUCH)

parseSQLtxt :: Text -> Either (ParseErrorBundle Text Void) Command
parseSQLtxt = parse commandParser "query"

parseSQL :: String -> Either (ParseErrorBundle Text Void) Command
parseSQL = parseSQLtxt . pack