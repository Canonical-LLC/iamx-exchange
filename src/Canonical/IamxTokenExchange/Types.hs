module Canonical.IamxTokenExchange.Types
  ( Config (..)
  , Action (..)
  ) where

import           Ledger
import           PlutusTx
import           PlutusTx.Prelude
import           GHC.Generics

data Config = Config
  { iamxWallet        :: !PubKeyHash
  , iamxPolicyId      :: !CurrencySymbol
  , iamxTokenName :: !TokenName
  } deriving (Generic)

makeLift ''Config

data Action
  = Burn
  | Keep
  | Exchange
      { rewardAddress :: !PubKeyHash
      , generation    :: !Integer
      }

unstableMakeIsData ''Action
