{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Yesod.Trans.Class
  ( SiteTrans (..)
  ) where

import Yesod.Site.Class
import Yesod.Site.Util

import Data.Coerce
import Yesod.Core
  ( Approot (..)
  , guessApproot
  , HtmlUrl
  , RenderRoute (..)
  , ScriptLoadPosition (..)
  , Yesod (..)
  )

class SiteTrans (t :: * -> *) where
  lift :: (MonadSite m) => m site a -> m (t site) a

  mapSiteT
    :: (MonadSite m, MonadSite n, SiteCompatible site site')
    => (m site a -> n site' b)
    -> m (t site) a -> n (t site') b

coerceHtmlUrl
  :: SiteCompatible site site'
  => HtmlUrl (Route site) -> HtmlUrl (Route site')
coerceHtmlUrl url = url . (. coerce)
