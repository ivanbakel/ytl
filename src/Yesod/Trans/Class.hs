{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Yesod.Trans.Class
  ( SiteTrans (..)
  ) where

import Yesod.Site.Class
import Yesod.Site.Util

-- | The class of site transformations
--
-- A site transformation is a wrapper around a Yesod foundation site which
-- augments it with additional functionality.
class SiteTrans (t :: * -> *) where
  -- | Lift a Yesod computation to a transformed computation, typically by
  -- wrapping the foundation site directly
  lift :: (MonadSite m) => m site a -> m (t site) a

  -- | Transform the Yesod computation under a site transformation
  --
  -- Unlike transformers and mtl, ytl does not allow transformers which are
  -- not functors in the category of (Yesod) monads. This is because all such
  -- transformed monads must still be isomorphic to @ReaderT@ (since that is
  -- their underlying representation in Yesod).
  mapSiteT
    :: (MonadSite m, MonadSite n, SiteCompatible site site')
    => (m site a -> n site' b)
    -> m (t site) a -> n (t site') b

