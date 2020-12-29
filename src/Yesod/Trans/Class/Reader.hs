{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Yesod.Trans.Class.Reader
  ( ReaderSite (..)
  , runReaderSite

  , SiteReader (..)
  ) where

import Yesod.Site.Class
import Yesod.Trans.Class

import Data.Copointed
import Yesod.Core
  ( RenderRoute (..)
  )

-- | The class of sites which can read some data
class SiteReader r site | site -> r where
  {-# MINIMAL (ask | reader), local #-}
  -- | Get the data value
  ask :: (MonadSite m) => m site r
  ask = reader id
  -- | Extract a value from the data
  reader :: (MonadSite m) => (r -> a) -> m site a
  reader f = f <$> ask

  -- | Run a computation with a transformed version of the current data
  -- value
  local :: (MonadSite m) => (r -> r) -> m site a -> m site a

instance {-# OVERLAPPABLE #-}
  (SiteTrans t, SiteReader r site) => SiteReader r (t site) where
  ask = lift ask
  reader f = lift $ reader f

  local f = mapSiteT (local f)

-- | A site transformation which extends a site with some additional data
-- which can be read
data ReaderSite r site = ReaderSite
  { readVal :: r
  , unReaderSite :: site
  }

-- | Compute the effect of 'ReaderSite' by passing in the data value to be
-- used when reading
runReaderSite
  :: (MonadSite m)
  => r
  -> m (ReaderSite r site) a
  -> m site a
runReaderSite r
  = withSiteT (ReaderSite r)

instance Copointed (ReaderSite r) where
  copoint = unReaderSite

instance {-# OVERLAPPING #-} SiteReader r (ReaderSite r site) where
  ask = do
    ReaderSite r _ <- askSite
    pure r

  local f = withSiteT (\(ReaderSite r site) -> ReaderSite (f r) site)

instance RenderRoute site => RenderRoute (ReaderSite r site) where
  newtype Route (ReaderSite r site) = ReaderRoute (Route site)
  renderRoute (ReaderRoute route) = renderRoute route

deriving instance Eq (Route site) => Eq (Route (ReaderSite r site))

instance SiteTrans (ReaderSite r) where
  lift = withSiteT unReaderSite

  mapSiteT runner argM = do
    ReaderSite r _ <- askSite
    withSiteT unReaderSite $ runner $ withSiteT (ReaderSite r) argM
