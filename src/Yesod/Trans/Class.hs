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

-- | The class of site transformations
--
-- A site transformation is a wrapper around a Yesod foundation site which
-- augments it with additional functionality.
class SiteTrans (t :: * -> *) where
  -- | Lift a Yesod computation to a transformed computation, typically by
  -- wrapping the foundation site directly
  lift :: (MonadSite m) => m site a -> m (t site) a

  -- | Transform the Yesod computation under a site transformation
  mapSiteT
    :: (MonadSite m, MonadSite n, SiteCompatible site site')
    => (m site a -> n site' b)
    -> m (t site) a -> n (t site') b

