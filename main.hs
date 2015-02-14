import Control.Applicative ((<*))
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Text.Parsec hiding (State, space, spaces)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec as P

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

type Program = [Stmt]

type Identifier = String

data Type =
    TInt
  | TFloat
  | TBool
  deriving (Eq, Show)

data Value =
    Int Integer
  | Float Double
  | Bool Bool
  deriving (Eq, Show)

data Expr =
    Lit Value
  | Var Identifier
  deriving (Eq, Show)

data Stmt =
    Identifier ::: Type
  | Identifier := Expr
  | Block Program
  deriving (Eq, Show)

lexer = Token.makeTokenParser emptyDef
int   = Token.integer lexer
float = Token.float lexer

vartype :: Parser Type
vartype =  (string "int"   >> return TInt)
       <|> (string "float" >> return TFloat)
       <|> (string "bool"  >> return TBool)
       <?> "type"

identifier :: Parser Identifier
identifier = do
  first <- letter <|> char '_'
  rest  <- many $ alphaNum <|> char '_'

  let ident = first : rest
  if ident `elem` ["var", "int", "float", "bool", "begin", "end"]
    then unexpected $ "reserved word " ++ ident
    else return ident

bool :: Parser Bool
bool =  (string "true"  >> return True)
    <|> (string "false" >> return False)
    <?> "true or false"

value :: Parser Value
value =  fmap Float (try float)
     <|> fmap Int (try int)
     <|> fmap Bool (try bool)
     <?> "value"

space :: Parser ()
space = void $ oneOf " \t\n"

spaces :: Parser ()
spaces = void $ many space

spaces1 :: Parser ()
spaces1 = void $ many1 space

decl :: Parser Stmt
decl = do
  string "var" >> spaces
  i <- identifier <* spaces
  t <- vartype
  char ';'
  return $ i ::: t

assign :: Parser Stmt
assign = do
  i <- identifier
  spaces >> char '=' >> spaces
  e <- expr
  char ';'
  return $ i := e

block :: Parser Stmt
block = do
  string "begin" >> spaces1
  p <- program
  string "end"
  return $ Block p

expr :: Parser Expr
expr =  try (fmap Lit value)
    <|> fmap Var identifier
    <?> "expression"

stmt :: Parser Stmt
stmt = (try decl <|> block <|> assign) <* spaces

program :: Parser Program
program = many (try stmt)

--------------------------------------------------------------------------------

type Env = Map Identifier Type

data Frame =
    Frame Env Frame
  | None

instance Show Frame where
  show None = "None"
  show (Frame e f) = assocs ++ "---\n" ++ show f
    where assocs = unlines . map show $ M.assocs e

type Check = ExceptT TypeError (State Frame)

data TypeError =
    NotInScope Identifier
  | Mismatch Type Type
  deriving (Eq, Show)

emptyFrame :: Frame
emptyFrame = Frame M.empty None

local :: (Frame -> Frame) -> Check a -> Check a
local f c = do
  frame <- get
  put (f frame)
  x <- c
  put frame
  return x

addType :: Identifier -> Type -> Frame -> Frame
addType i t f = case f of
  Frame e f' -> Frame (M.insert i t e) f'
  None       -> None

newFrame :: Check ()
newFrame = do
  frame <- get
  put $ Frame M.empty frame

restoreFrame :: Check ()
restoreFrame = do
  frame <- get
  case frame of
    Frame e f -> put f
    None -> return ()

checkIdent :: Identifier -> Check Type
checkIdent i = do
  frame <- get
  case frame of
    None -> throwError $ NotInScope i
    Frame env frame' -> case M.lookup i env of
                          Just t  -> return t
                          Nothing -> local (const frame') (checkIdent i)

checkExpr :: Expr -> Check Type
checkExpr e = case e of
  Lit (Int _)   -> return TInt
  Lit (Float _) -> return TFloat
  Lit (Bool _)  -> return TBool
  Var i         -> checkIdent i

checkStmt :: Stmt -> Check ()
checkStmt s = case s of
  i ::: t -> modify (addType i t)
  i := e  -> do t1 <- checkIdent i
                t2 <- checkExpr e
                when (t1 /= t2) $ throwError (Mismatch t1 t2)
  Block p -> newFrame >> check p >> restoreFrame

check :: Program -> Check ()
check []     = return ()
check (x:xs) = checkStmt x >> check xs

runCheck :: Frame -> Check a -> (Either TypeError a, Frame)
runCheck frame = flip runState frame . runExceptT

main = do
  let s = "var x int;\nx = 10;\nbegin\n  var x bool;\n  x = 10;\nend"

  -- Print program
  putStrLn s
  putStrLn ""

  -- Parse
  putStr "Parsing..."
  case parse (program <* eof) "<program>" s of
    Left e -> print e
    Right p -> do
      -- Parse OK
      putStrLn "OK"
      mapM_ print p
      putStrLn ""

      -- Type check
      putStr "Checking types..."
      let (result, frame) = runCheck emptyFrame $ check p
      case result of
        Left err -> putStrLn "Error" >> print err
        Right t  -> putStrLn "OK"

      putStrLn "Frame Stack:"
      print frame
