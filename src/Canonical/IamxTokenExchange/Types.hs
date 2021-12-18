module Canonical.IamxTokenExchange.Types
  ( Config (..)
  , Action (..)
  ) where

import           Ledger
import           PlutusTx
import           PlutusTx.Prelude
import           GHC.Generics
import qualified Prelude as P

data Config = Config
  { iamxWallet        :: !PubKeyHash
  , iamxTokenName     :: !TokenName
  , initialUtxo       :: !TxOutRef
  } deriving (P.Show, Generic)

makeLift ''Config

data Action
  = InitialMint
  | Burn
  | Reward
      { rewardAddress :: !PubKeyHash
      , generation    :: !Integer
      }

unstableMakeIsData ''Action
