{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.UserSpec where

-------------------------------------------------------------------------------
import Control.Monad.Freer
import Test.Hspec
-------------------------------------------------------------------------------
import Database.User
import Models.User
-------------------------------------------------------------------------------

runDatabaseUser' :: Eff (DatabaseUser ': effs) ~> Eff effs
runDatabaseUser' =
  interpret $ \case
    GetUsers -> return $ [ UserR 1 "Test1", UserR 2 "Test2" ]
    GetUser uid -> return . Just $ UserR uid "Test"
    CreateUser u -> return u
    UpdateUser _u -> return ()
    DeleteUser _uid -> return ()

spec :: Spec
spec = parallel $ do
  describe "DatabaseUser" $ do
    it "getUsers" $ do
      let v = run $ runDatabaseUser' getUsers
      length v`shouldBe` 2
