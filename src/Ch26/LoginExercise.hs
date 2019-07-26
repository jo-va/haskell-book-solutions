{-# LANGUAGE OverloadedStrings #-}

-- Example from two-wrongs.com:
-- A gentle introduction to monad transformers

module Ch26.LoginExercise where

import Data.Text
import Data.Text.IO as T
import Data.Map as Map
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data ExceptT e m a = ExceptT {
    runExceptT :: m (Either e a)
}

instance Functor m => Functor (ExceptT e m) where
    fmap f (ExceptT mea) = ExceptT $ (fmap . fmap) f mea

instance Applicative m => Applicative (ExceptT e m) where
    pure = ExceptT . pure . pure
    ExceptT mef <*> ExceptT mea = ExceptT $
        (<*>) <$> mef <*> mea

instance Monad m => Monad (ExceptT e m) where
    return = pure
    ExceptT mea >>= f = ExceptT $
        mea >>= either (return . Left) (runExceptT . f)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither = ExceptT . return

instance MonadTrans (ExceptT e) where
    lift = ExceptT . fmap return
            
instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO = lift . liftIO

throwE :: Monad m => e -> ExceptT e m a
throwE = liftEither . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT c m a) -> ExceptT c m a
catchE throwing handler = ExceptT $ do
    x <- runExceptT throwing
    case x of
      Left failure -> runExceptT $ handler failure
      Right success -> return $ Right success

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

test :: IO ()
test = do
    runExceptT loginDialogue
    return ()

loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
    let retry = userLogin `catchE` wrongPasswordHandler
    token <- retry `catchE` printError
    liftIO $ T.putStrLn $ append "Logged in with token: " token

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
    liftIO $ T.putStrLn "Wrong password, one more chance."
    userLogin
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
    liftIO . T.putStrLn $ case err of
        WrongPassword -> "Wrong password. No more chances."
        NoSuchUser    -> "No user with that email exists."
        InvalidEmail  -> "Invalid email address entered."
    throwE err

userLogin :: ExceptT LoginError IO Text
userLogin = do
    token    <- getToken
    userpw   <- maybe (throwE NoSuchUser) return (Map.lookup token users)
    password <- liftIO $ T.putStrLn "Enter your password:" >> T.getLine

    if userpw == password
       then return token
       else throwE WrongPassword

getToken :: ExceptT LoginError IO Text
getToken = do
    liftIO $ T.putStrLn "Enter email address:"
    input <- liftIO T.getLine
    liftEither $ getDomain input

getDomain :: Text -> Either LoginError Text
getDomain email =
    case splitOn "@" email of
      [name, domain] -> Right domain
      _              -> Left InvalidEmail

