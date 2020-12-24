{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Trans.Class.Writer
  ( WriterSite

  , MonadSiteWriter (..)
  ) where

import Yesod.Site.Class
import Yesod.Trans.Class
import Yesod.Trans.Class.Reader

import Data.Copointed
import Data.IORef
import Yesod.Core
  ( liftIO
  , RenderRoute (..)
  )

-- TODO: Decide if we want a lazy/strict distinction
class (Monoid w) => MonadSiteWriter w site where
  writer :: (MonadSite m) => (a, w) -> m site a
  writer (a, w) = tell w >> pure a
  tell :: (MonadSite m) => w -> m site ()
  tell w = writer ((), w)

  listen :: (MonadSite m) => m site a -> m site (a, w)

  pass :: MonadSite m => m site (a, w -> w) -> m site a

instance {-# OVERLAPPABLE #-}
  (SiteTrans t, MonadSiteWriter w site) => MonadSiteWriter w (t site) where
  writer = lift . writer
  tell = lift . tell

  listen = mapSiteT listen
  pass = mapSiteT pass

newtype WriterSite w site = WriterSite
  { unWriterSite :: ReaderSite (IORef w) site
    -- "Is an IORef safe in Yesod sites?"
    --
    -- Yes, because the Yesod code uses one to build its web pages.
  }

instance Copointed (WriterSite w) where
  copoint = copoint . unWriterSite

instance (Monoid w) => MonadSiteWriter w (WriterSite w site) where
  tell v = withSiteT unWriterSite do
    wRef <- ask
    liftIO $ modifyIORef' wRef (<> v)

  listen argM = do
    a <- argM
    withSiteT unWriterSite do
      wRef <- ask
      w <- liftIO $ readIORef wRef
      pure (a, w)

  pass modM = do
    (a, f) <- modM
    withSiteT unWriterSite do
      wRef <- ask
      liftIO $ modifyIORef' wRef f
      pure a

instance RenderRoute site => RenderRoute (WriterSite w site) where
  newtype Route (WriterSite w site) = WriterRoute (Route (ReaderSite (IORef w) site))
  renderRoute (WriterRoute route) = renderRoute route

deriving instance Eq (Route site) => Eq (Route (WriterSite w site))

instance SiteTrans (WriterSite w) where
  lift = withSiteT unWriterSite . lift

  mapSiteT runner argM = do
    withSiteT unWriterSite $ mapSiteT runner $ withSiteT WriterSite argM
