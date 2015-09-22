module Data.Config.Parser
       ( ConfigFile(..)
       , Section(..)
       , Assignment(..)
       , ParseError
       , parseConfig
       ) where

import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Text

newtype ConfigFile = ConfigFile [Section]
                   deriving (Show, Eq)

data Section = Section
  { secName :: Text
  , secAssignments :: [Assignment]
  }
  deriving (Show, Eq)

data Assignment = Assignment
  { assVariable :: Text
  , assValue :: Text
  }
  deriving (Show, Eq)

parseConfig :: Text -> Either ParseError ConfigFile
parseConfig = parse (configFile <* eof) "(string)"

configFile :: Parser ConfigFile
configFile = do
  skipMany emptyLine
  ConfigFile <$> many section

section :: Parser Section
section = Section <$> sectionHeader <*> many assignment

sectionHeader :: Parser Text
sectionHeader = T.strip . T.pack <$>
  between (char '[') (char ']' >> lineEnd)
    (many1 (noneOf "]#'") <?> "section name")

  <?> "section header"

assignment :: Parser Assignment
assignment = (<?> "assignment") $ do
  var <- variable
  tok '='
  val <- value
  lineEnd
  return $ Assignment var val

variable :: Parser Text
variable = T.pack <$> lexeme (many1 alphaNum)
  <?> "variable name"

value :: Parser Text
value = T.strip . T.pack <$> many1 (noneOf "#\n")
  <?> "value"

whitespace :: Parser ()
whitespace = void (oneOf " \t" <?> "whitespace")

lineEnd1 :: Parser ()
lineEnd1 = comment <|> void newline

emptyLine :: Parser ()
emptyLine =  (many1 whitespace >> (lineEnd1 <|> eof))
         <|> lineEnd1

lineEnd :: Parser ()
lineEnd =  (lineEnd1 >> skipMany emptyLine)
       <|> eof

comment :: Parser ()
comment = (<?> "comment") $ do
  void $ char '#'
  void $ anyChar `manyTill` newline

tok :: Char -> Parser ()
tok c = void $ lexeme (char c)

lexeme :: Parser a -> Parser a
lexeme parser = skipMany whitespace *> parser <* skipMany whitespace
