{-# LANGUAGE TupleSections #-}
module StateModule where

import Control.Monad (liftM, ap)

newtype State s a = State (s -> (a,s)) -- s is de state en a


instance Monad (State s)  where
    -- return :: a -> m a == a -> state s a
    return a = State (a,)
    -- m a -> (a -> m b) -> m b
    (State f) >>=  g = State $ \s -> let (r1, s') = f s
                                         (State h) = g r1
                                in h s'

instance Functor (State s) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (State sf) = State ( \s -> let (result, newState) = sf s in (f result, newState) )

instance Applicative (State s) where
    pure = return
    (<*>) = ap

-- Run the state transformer on given state, return the a value and the resulting state.
run :: State s a -> s -> (a, s)
run (State f) = f

-- Get the state.
get :: State s s
get = State $ \s -> (s, s)

-- Set the state.
put :: s -> State s ()
put newState = State $ const ((), newState)

-- Modify the state.
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
