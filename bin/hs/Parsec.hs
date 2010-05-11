{-# LANGUAGE CPP #-}

module Parsec (
#if MIN_VERSION_parsec(3,0,0)
      module Control.Applicative
    , module Text.Parsec
    , module Text.Parsec.String
#else
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
#endif
    ) where

import Control.Applicative

#if MIN_VERSION_parsec(3,0,0)
import Text.Parsec
import Text.Parsec.String
#else
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
#endif
