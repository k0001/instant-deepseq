{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Instant.Functions.DeepSeq
  ( -- $defaults
    grnfDefault
  , RepGNFData
    -- * Internals
  , GNFData(..)
  ) where

import           Control.DeepSeq (NFData(rnf))
import           Generics.Instant

--------------------------------------------------------------------------------
-- $defaults
--
-- You can use 'grnfDefault' as your generic 'rnf' for any 'Representable' type
-- as follows:
--
-- @
-- instance 'NFData' MyType where rnf = 'grnfDefault'
-- @

grnfDefault :: (Representable a, GNFData (Rep a)) => a -> ()
grnfDefault = \a -> grnf (from a)
{-# INLINABLE grnfDefault #-}

-- | @'RepGNFData'@ is simply a synonym for
-- @('Representable' a, 'GNFData' ('Rep' a))@ with the convenient
-- kind @(* -> 'GHC.Exts.Constraint')@
class (Representable a, GNFData (Rep a)) => RepGNFData a
instance (Representable a, GNFData (Rep a)) => RepGNFData a

--------------------------------------------------------------------------------

class GNFData a where
  grnf :: a -> ()

instance GNFData Z where
  grnf _ = error
    "Generics.Instant.Functions.DeepSeq.GNFData Z grnf - impossible"

instance GNFData U where
  grnf !U = ()
  {-# INLINABLE grnf #-}

instance GNFData a => GNFData (CEq c p q a) where
  grnf !(C a) = grnf a `seq` ()
  {-# INLINABLE grnf #-}

instance (GNFData a, GNFData b) => GNFData (a :*: b) where
  grnf !(a :*: b) = grnf a `seq` grnf b `seq` ()
  {-# INLINABLE grnf #-}

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
  grnf !(L a) = grnf a `seq` ()
  grnf !(R b) = grnf b `seq` ()
  {-# INLINABLE grnf #-}

instance NFData a => GNFData (Var a) where
  grnf !(Var a) = rnf a `seq` ()
  {-# INLINABLE grnf #-}

instance NFData a => GNFData (Rec a) where
  grnf !(Rec a) = rnf a `seq` ()
  {-# INLINABLE grnf #-}
