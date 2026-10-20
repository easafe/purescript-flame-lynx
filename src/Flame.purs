-- | Entry module for a default Flame application
module Flame (module Exported) where

import Flame.Application (Application, Route, makeRoute, makeGuardedRoute, mountRoutes, navigate, noMessages, Update, mount) as Exported
import Flame.Types (Html, Key, PreApplication, AppId(..), Subscription) as Exported
