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
  , iamxTokenName     :: !TokenName
  , initialUtxo       :: !TxOutRef
  } deriving (Generic)

makeLift ''Config

data Action
  = InitialMint
  | Burn
  | Keep
  | Exchange
      { rewardAddress :: !PubKeyHash
      , generation    :: !Integer
      }

unstableMakeIsData ''Action
