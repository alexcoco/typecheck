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
  deriving (Eq)

instance Show Type where
  show TInt   = "integer"
  show TFloat = "float"
  show TBool  = "boolean"

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

showEnv :: Env -> String
showEnv e = unlines $ map aux $ M.assocs e
  where aux (i,t) = i ++ ": " ++ show t

data Frame =
    Frame Env Frame
  | None

instance Show Frame where
  show None = "None"
  show (Frame e f) = assocs ++ "---\n" ++ show f
    where assocs = unlines . map show $ M.assocs e

type Check = ExceptT TypeError (State (String, Frame))

data TypeError =
    NotInScope Identifier
  | Mismatch Type Type
  deriving (Eq, Show)

emptyFrame :: Frame
emptyFrame = Frame M.empty None

local :: (Frame -> Frame) -> Check a -> Check a
local f c = do
  (log, frame) <- get
  put (log, f frame)
  x <- c
  put (log, frame)
  return x

addType :: Identifier -> Type -> (String, Frame) -> (String, Frame)
addType i t (l,f) = case f of
  Frame e f' -> (l, Frame (M.insert i t e) f')
  None       -> (l, None)

newFrame :: Check ()
newFrame = do
  (log, frame) <- get
  put (log, Frame M.empty frame)

restoreFrame :: Check ()
restoreFrame = do
  (log, frame) <- get

  let log' = case frame of
               Frame e f -> log ++ "\n" ++ showEnv e
               None -> log

  case frame of
    Frame e f -> put (log', f)
    None -> return ()

checkIdent :: Identifier -> Check Type
checkIdent i = do
  (log, frame) <- get
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
  Block p -> newFrame >> check p

check :: Program -> Check ()
check []     = restoreFrame
check (x:xs) = checkStmt x >> check xs

runCheck :: String -> Frame -> Check a -> (Either TypeError a, (String, Frame))
runCheck s f = flip runState (s, f) . runExceptT

main = do
  src <- readFile "test.tc"

  -- Parse
  case parse (program <* eof) "<program>" src of
    Left e -> print e
    Right p -> do -- Parse OK

      -- Type check
      let (result, (log, frame)) = runCheck "" emptyFrame $ check p
      case result of
        Left err -> do putStrLn $ "Type Error: " ++ show err
                       putStrLn log
        Right _  -> putStrLn "Type OK" >> putStrLn log
