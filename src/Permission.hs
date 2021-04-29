module Permission where

-- TODO: check action permission
data Permission action = Granted action
                       | Forbidden
  deriving (Show, Eq)

instance Functor Permission where
  fmap _ Forbidden = Forbidden
  fmap f (Granted a) = Granted $ f a

instance Applicative Permission where
  pure a = Granted a

  (<*>) Forbidden _ = Forbidden
  (<*>) (Granted grantOp) grantAct = fmap grantOp grantAct

instance Monad Permission where
  return = pure

  (Granted action) >>= f = f action
  Forbidden >>= _ = Forbidden

data Permissions model = CanRetrieve model
                       | CanCreate model
                       | CanUpdate model
                       | CanDelete model
  deriving (Show, Eq)
