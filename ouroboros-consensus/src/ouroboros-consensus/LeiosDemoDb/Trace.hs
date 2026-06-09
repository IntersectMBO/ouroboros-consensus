module LeiosDemoDb.Trace (TraceLeiosDb (..)) where

data TraceLeiosDb
  = -- | A UNIQUE/PRIMARY KEY constraint was violated by an INSERT, the
    -- offending row was silently ignored. Fields: table name, then a
    -- human-readable description of the colliding key.
    TraceLeiosDbInsertCollision String String
  deriving Show
