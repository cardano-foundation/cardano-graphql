module Cardano.Prelude.Base
       ( module X
       , identity
       ) where

import           Protolude as X hiding (Hashable, hash, hashUsing, hashWithSalt,
                     identity, (.))

import           Control.Category (id)
import           Control.Category as X hiding (id)
import           Numeric.Natural as X

-- | Rename `id` to `identity` to allow `id` as a variable name
identity :: Category cat => cat a a
identity = id
