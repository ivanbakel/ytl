{-# OPTIONS_GHC -Wno-orphans -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Trans
  ( module Yesod.Trans.Class
  ) where

import Yesod.Site.Util
import Yesod.Trans.Class
import Yesod.Trans.TH

import Data.Copointed
import Yesod.Core

defaultYesodInstanceExcept [| copoint |] [d|
  instance {-# OVERLAPPABLE #-}
    ( SiteTrans t
    , RenderRoute (t site)
    , SiteCompatible site (t site)
    , Copointed t
    , Yesod site
    ) => Yesod (t site) where
  |]
