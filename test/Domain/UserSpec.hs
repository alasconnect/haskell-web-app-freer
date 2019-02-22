{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.UserSpec where

-------------------------------------------------------------------------------
import Control.Monad.Freer
import Test.Hspec
-------------------------------------------------------------------------------
import Domain.User
import Models.User
-------------------------------------------------------------------------------

runDomainUser' :: Eff (DomainUser ': effs) ~> Eff effs
runDomainUser' =
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
      let v = run $ runDomainUser' getUsers
      length v`shouldBe` 2
