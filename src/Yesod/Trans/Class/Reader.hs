{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Trans.Class.Reader
  ( ReaderSite

  , MonadSiteReader (..)
  ) where

import Yesod.Site.Class
import Yesod.Trans.Class

import Data.Copointed
import Yesod.Core
  ( RenderRoute (..)
  )

class MonadSiteReader r site where
  ask :: (MonadSite m) => m site r
  ask = reader id
  reader :: (MonadSite m) => (r -> a) -> m site a
  reader f = f <$> ask

  local :: (MonadSite m) => (r -> r) -> m site a -> m site a

instance {-# OVERLAPPABLE #-}
  (SiteTrans t, MonadSiteReader r site) => MonadSiteReader r (t site) where
  ask = lift ask
  reader f = lift $ reader f

  local f = mapSiteT (local f)

data ReaderSite r site = ReaderSite
  { readVal :: r
  , unReaderSite :: site
  }

instance Copointed (ReaderSite r) where
  copoint = unReaderSite

instance MonadSiteReader r (ReaderSite r site) where
  ask = do
    ReaderSite r site <- askSite
    pure r

  local f = withSiteT (\(ReaderSite r site) -> ReaderSite (f r) site)

instance RenderRoute site => RenderRoute (ReaderSite r site) where
  newtype Route (ReaderSite r site) = ReaderRoute (Route site)
  renderRoute (ReaderRoute route) = renderRoute route

deriving instance Eq (Route site) => Eq (Route (ReaderSite r site))

instance SiteTrans (ReaderSite r) where
  lift = withSiteT unReaderSite

  mapSiteT runner argM = do
    ReaderSite r site <- askSite
    withSiteT unReaderSite $ runner $ withSiteT (ReaderSite r) argM
