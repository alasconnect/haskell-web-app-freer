module Types where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data FakeConn = FakeConn
data AppContext
  = AppContext
  { conn :: FakeConn -- in a real system this would be a pool
  }
