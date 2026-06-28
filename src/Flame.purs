-- | Entry module for a default Flame application
module Flame (module Exported) where

import Flame.Application (Application, noMessages, Update, mount, mount_) as Exported
import Flame.Types (Html, Key, PreApplication, AppId(..), Subscription) as Exported
