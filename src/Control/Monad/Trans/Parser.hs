module Control.Monad.Trans.Parser
  (
  -- * Parser types
   ResultM(..), toM
  , ParserT(..), liftIR, liftP
  -- * Running parsers
  , feedM, feedMWith
  , runParserTOnly
  , runParserTWith
  -- ** Merging transformers
  , runStateParserT
  , runWriterParserT
  -- ** Result conversion
  , failResultM
  , zeroResultM
  , maybeResultM
  , eitherResultM
  ,) where

import Data.Attoparsec.Combinator (feed)
import Data.Attoparsec.Internal.Types (IResult(..))
import Data.Monoid (Monoid, mempty, mappend, (<>))

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (MonadPlus, mzero, ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)

-- * Parser Types

data ResultM i m r
  = FailM i String
  | DoneM i r
  | PartialM (ParserT i m r)

instance Monad m => Functor (ResultM i m) where
  fmap f (FailM i s)  = FailM i s
  fmap f (DoneM i r)  = DoneM i (f r)
  fmap f (PartialM p) = PartialM (fmap f p)

toM :: Monad m => IResult i r -> ResultM i m r
toM (Fail i _ s) = FailM i s
toM (Done i r)   = DoneM i r
toM (Partial p)  = PartialM . ParserT $ return . toM . p

newtype ParserT i m r = ParserT { runParserT :: i -> m (ResultM i m r) }

liftP :: Monad m => (i -> IResult i r) -> ParserT i m r
liftP p = ParserT $ return . toM . p

liftIR :: (Monad m, Monoid i) => IResult i r -> ParserT i m r
liftIR = liftP . feed

instance Monad m => Functor (ParserT i m) where
  fmap f mr = pure f <*> mr

instance Monad m => Applicative (ParserT i m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ParserT i m) where
  return r = ParserT $ \i -> return (DoneM i r)
  mr >>= f = ParserT $ \i -> runParserT mr i >>= rec f
    where
      rec f (FailM i s)  = return (FailM i s)
      rec f (DoneM i r)  = runParserT (f r) i
      rec f (PartialM p) = return . PartialM $ p >>= f
  fail s = ParserT $ \i -> return $ FailM i s

instance MonadTrans (ParserT i) where
  lift mr = ParserT $ \i -> mr >>= return . DoneM i

instance MonadIO m => MonadIO (ParserT i m) where
  liftIO io = lift (liftIO io)

-- * Running parsers

feedM :: (Monad m, Monoid i) => ResultM i m r -> i -> m (ResultM i m r)
feedM (FailM i' s) i = return $ FailM (i' <> i) s
feedM (DoneM i' r) i = return $ DoneM (i' <> i) r
feedM (PartialM p) i = runParserT p i

feedMWith :: (Monad m, Monoid i, Eq i) => m i -> ResultM i m r -> m (ResultM i m r)
feedMWith mi r = mi >>= \i -> case i == mempty of
  True -> return r
  _ -> feedM r i >>= feedMWith mi

runParserTOnly :: Monad m => ParserT i m r -> i -> m (Either String r)
runParserTOnly p i = runParserT p i >>= return . eitherResultM

runParserTWith :: (Monad m, Monoid i, Eq i) => m i -> ParserT i m r -> ParserT i m r
runParserTWith mi p = ParserT $ \i -> runParserT p i >>= feedMWith mi

-- * Merging transformers

runStateParserT :: Monad m => ParserT i (StateT r m) () -> r -> ParserT i m r
runStateParserT p r = ParserT $ \i -> runStateT (runParserT p i) r >>= return . rec
  where rec (DoneM i (), r')  = DoneM i r'
        rec (FailM i s, _)    = FailM i s
        rec (PartialM p', r') = PartialM $ runStateParserT p' r'

runWriterParserT :: (Monad m, Monoid r) => ParserT i (WriterT r m) () -> ParserT i m r
runWriterParserT p = ParserT $ \i -> runWriterT (runParserT p i) >>= return . rec
  where rec (DoneM i (), r)  = DoneM i r
        rec (FailM i s, _)   = FailM i s
        rec (PartialM p', r) = PartialM . fmap (mappend r) $ runWriterParserT p'

-- * Result Conversion

failResultM :: Monad m => ResultM i m r -> m r
failResultM = either fail return . eitherResultM

zeroResultM :: MonadPlus m => ResultM i m r -> m r
zeroResultM = maybe mzero return . maybeResultM

maybeResultM :: ResultM i m r -> Maybe r
maybeResultM (DoneM _ r) = Just r
maybeResultM _ = Nothing

eitherResultM :: ResultM i m r -> Either String r
eitherResultM (DoneM _ r)  = Right r
eitherResultM (FailM _ s)  = Left s
eitherResultM (PartialM _) = Left "Result: incomplete input"
