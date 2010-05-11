{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_parsec
#define MIN_VERSION_parsec(x,y,z) 0
#endif

#if MIN_VERSION_parsec(3,0,0)
module Parsec (
      module Control.Applicative
    , module Text.Parsec
    , module Text.Parsec.String
    ) where

import Control.Applicative hiding (many,optional,(<|>))
import Text.Parsec
import Text.Parsec.String
#else
module Parsec (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
#endif
