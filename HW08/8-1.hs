--import Prelude hiding (Maybe, Nothing, Just)
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

instance Functor Maybe_ where
  fmap = liftM

instance Applicative Maybe_ where
  pure = return
  (<*>) = ap

main = do
  print $ "son1's mother is : " ++ show (mother "son1")
  print $ "son2's father is : " ++ show (father "son2")
  print $ maternalGrandfather "son1"
  print $ maternalGrandmother "son2"
  print $ paternalGrandfather "son3"

data Maybe_ a = Just_ a | Nothing_ String
                deriving (Show)

instance Monad Maybe_ where
  --return :: a -> Maybe_ a
  return x = Just_ x

  --(>>=) :: (Maybe_ a) -> (a -> Maybe_ b) -> Maybe_ b
  (>>=) (Just_ x) f = f x
  (>>=) (Nothing_ s) f = Nothing_ $ s

type Person = String

father :: Person -> Maybe_ Person
father son = case son of
  "son1" -> return "father1"
  "son2" -> return "father2"
  otherwise -> Nothing_ $ "found father falied in " ++ show son
  
mother :: Person -> Maybe_ Person
mother son = case son of
  "son1" -> return "mother1"
  "son2" -> return "mother2"
  otherwise -> Nothing_ $ "found mother falied in " ++ show son

maternalGrandfather :: Person -> Maybe_ Person
maternalGrandfather son = (return son) >>= mother >>= father

maternalGrandmother :: Person -> Maybe_ Person
maternalGrandmother son = (return son) >>= mother >>= mother

paternalGrandfather :: Person -> Maybe_ Person
paternalGrandfather son = (return son) >>= father >>= father

paternalGrandmother :: Person -> Maybe_ Person
paternalGrandmother son = (return son) >>= father >>= mother
