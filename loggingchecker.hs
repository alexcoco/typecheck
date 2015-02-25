import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader

type Checker e w a = ExceptT e (Writer w) a
type Env = [String]
data Op = Refer String | Declare String
data MyError = UnreferencedError deriving (Show)

typecheck :: Env -> [Op] -> Checker MyError String ()
typecheck _ [] = return ()
typecheck e ((Declare x):xs) = do
  tell $ "Declaring " ++ x ++ "\n"
  typecheck (x:e) xs
typecheck e ((Refer x):xs) = do
  if x `elem` e 
    then typecheck e xs
    else throwError UnreferencedError

potato =
  runWriter $ runExceptT $ typecheck [] [Declare "x", Declare "y", Refer "x", Refer "z", Declare "z"]


main = do
  putStrLn . show $ potato
