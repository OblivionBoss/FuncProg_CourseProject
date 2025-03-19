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
  { selectFields :: [Text]
  , fromTable    :: Text
  , whereClause  :: Maybe Text
  , groupBy      :: Maybe [Text]
  , havingClause :: Maybe Text
  } deriving (Show)

data Command
  = Select Query
  | AddColumn Text Text
  | AddConstraint Text Text
  | AlterTable Text
  | AlterColumn Text Text Text
  | DeleteFrom Text (Maybe Text)
  | CreateTable Text [(Text, Text)]
  | DropTable Text Bool  -- Supports IF EXISTS
  | InsertInto Text [Text] [Text]
  | Update Text [(Text, Text)] (Maybe Text)
  | Join Text Text Text (Maybe Text) -- Supports different join types
  | OrderBy Text Bool
  | Limit Int
  | CreateIndex Text Text Bool
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
  _ <- symbol "SELECT"
  fields <- sepBy (lexeme identifier) (symbol ",")
  _ <- symbol "FROM"
  table <- lexeme identifier
  condition <- optional (symbol "WHERE" *> lexeme (pack <$> someTill anySingle (lookAhead (symbol "GROUP BY" <|> symbol "HAVING" <|> ("" <$ eof)))))
  groupBy <- optional (symbol "GROUP BY" *> sepBy (lexeme identifier) (symbol ","))
  having <- optional (symbol "HAVING" *> lexeme (pack <$> someTill anySingle ("" <$ eof)))
  return $ Query fields table condition groupBy having

addColumnParser :: Parser Command
addColumnParser = do
  _ <- symbol "ALTER TABLE"
  table <- lexeme identifier
  _ <- symbol "ADD" *> symbol "COLUMN"  -- Ensure correct tokenization
  column <- lexeme identifier
  columnType <- lexeme (pack <$> some (letterChar <|> digitChar <|> char '(' <|> char ')'))  -- Handles types like VARCHAR(255)
  return $ AddColumn table columnType


-- addConstraintParser :: Parser Command
-- addConstraintParser = do
--   _ <- symbol "ALTER TABLE"
--   table <- identifier
--   _ <- symbol "ADD CONSTRAINT"
--   constraint <- identifier
--   return $ AddConstraint table constraint

alterColumnParser :: Parser Command
alterColumnParser = do
  _ <- symbol "ALTER TABLE"
  table <- lexeme identifier
  _ <- symbol "ALTER" *> symbol "COLUMN"   -- Ensure correct tokenization
  column <- lexeme identifier
  columnType <- lexeme identifier
  return $ AlterColumn table column columnType

columnDefParser :: Parser (Text, Text)
columnDefParser = do
  colName <- lexeme identifier    -- Parse column name
  colType <- lexeme identifier    -- Parse column type (e.g., INT, VARCHAR)
  return (colName, colType)

createTableParser :: Parser Command
createTableParser = do
  _ <- symbol "CREATE TABLE"
  tableName <- lexeme identifier  -- Expect table name
  _ <- symbol "("                 -- Expect opening parenthesis
  columns <- columnDefParser `sepBy` symbol ","  -- Parse column definitions
  _ <- symbol ")"                 -- Expect closing parenthesis
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
  condition <- optional (symbol "WHERE" *> lexeme valueParser) -- More precise condition parsing
  return $ Update table updates condition

-- existsParser :: Parser Command
-- existsParser = do
--   _ <- symbol "EXISTS"
--   table <- identifier
--   return $ Exists table

-- setParser :: Parser Command
-- setParser = do
--   _ <- symbol "SET"
--   column <- lexeme identifier  -- Ensure it consumes trailing spaces
--   _ <- lexeme (symbol "=")      -- Allow spaces before and after "="
--   value <- valueParser
--   return $ Set column value

-- dropConstraintParser :: Parser Command
-- dropConstraintParser = do
--   _ <- symbol "ALTER TABLE"
--   table <- lexeme identifier
--   _ <- symbol "DROP CONSTRAINT"
--   constraint <- lexeme identifier
--   return $ DropConstraint table constraint

dropTableParser :: Parser Command
dropTableParser = do
  _ <- symbol "DROP TABLE"
  exists <- optional (symbol "IF EXISTS") -- Maybe Text
  table <- lexeme identifier              -- Text
  let existsFlag = case exists of
                      Just _  -> True
                      Nothing -> False
  return $ DropTable table existsFlag

commandParser :: Parser Command
commandParser = try (Select <$> selectParser) -- try edit this, too much try
            <|> try addColumnParser
            -- <|> try addConstraintParser
            <|> try alterColumnParser
            <|> try dropTableParser
            <|> try createTableParser
            <|> try insertIntoParser
            <|> try updateParser
            -- <|> try existsParser
            -- <|> try setParser
            -- <|> try dropConstraintParser

-- Running the parser

parseQuerytxt :: Text -> Either (ParseErrorBundle Text Void) Command
parseQuerytxt = parse commandParser "query"

parseQuery :: String -> Either (ParseErrorBundle Text Void) Command
parseQuery = parseQuerytxt . pack
