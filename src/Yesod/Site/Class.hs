{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Yesod.Site.Class
  ( MonadSite (..)
  ) where

import Yesod.Site.Util

import Control.Monad.Reader
import Yesod.Core.Types

-- | A unified class instance for Yesod's site-using monads
--
-- This is used for functions which work for both 'WidgetFor' and 'HandlerFor'.
class (forall site. MonadIO (m site)) => MonadSite (m :: * -> * -> *) where
  -- | Get the site itself in a computation
  askSite :: m site site

  -- | Run a computation under a given site transformation
  --
  -- This is the main entry point for site transformations - note that the
  -- site parameter is contravariant.
  withSiteT
    :: SiteCompatible site site'
    => (site -> site')
    -> m site' a
    -> m site a

instance MonadSite HandlerFor where
  askSite = do
    hd <- ask
    pure (getSite hd)

  withSiteT siteT (HandlerFor innerHandler)
    = HandlerFor (innerHandler . withSite siteT)

instance MonadSite WidgetFor where
  askSite = do
    wd <- ask
    pure (getWidgetSite wd)

  withSiteT siteT (WidgetFor innerWidget)
    = WidgetFor (innerWidget . withWidgetSite siteT)

