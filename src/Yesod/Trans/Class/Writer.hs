{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Trans.Class.Writer
  ( WriterSite (..)
  , runWriterSite

  , SiteWriter (..)
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

-- | The class of sites which have some writing output
class (Monoid w) => SiteWriter w site where
  {-# MINIMAL (writer | tell), listen, pass #-}

  -- | Write something to the output, returning a wrapped value
  writer :: (MonadSite m) => (a, w) -> m site a
  writer (a, w) = tell w >> pure a

  -- | Write something to the output
  tell :: (MonadSite m) => w -> m site ()
  tell w = writer ((), w)

  -- | Run a computation, returning the resulting contents of the output
  listen :: (MonadSite m) => m site a -> m site (a, w)

  -- | Run a computation which modifies the output
  pass :: MonadSite m => m site (a, w -> w) -> m site a

instance {-# OVERLAPPABLE #-}
  (SiteTrans t, SiteWriter w site) => SiteWriter w (t site) where
  writer = lift . writer
  tell = lift . tell

  listen = mapSiteT listen
  pass = mapSiteT pass

-- | A site transformation which extends a site with some writing output
newtype WriterSite w site = WriterSite
  { unWriterSite :: ReaderSite (IORef w) site
    -- "Is an IORef safe in Yesod sites?"
    --
    -- Yes, because the Yesod code uses one to build its web pages.
  }

-- | Compute the effect of a 'WriterSite', getting back the output after having
-- run the computation
runWriterSite
  :: (MonadSite m, Monoid w)
  => m (WriterSite w site) a
  -> m site (a, w)
runWriterSite inner = do
  wRef <- liftIO (newIORef mempty)
  a <- runReaderSite wRef $ withSiteT WriterSite $ inner
  w <- liftIO $ readIORef wRef
  pure (a, w)

instance Copointed (WriterSite w) where
  copoint = copoint . unWriterSite

instance (Monoid w) => SiteWriter w (WriterSite w site) where
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
