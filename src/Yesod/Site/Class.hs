{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Yesod.Site.Class
  ( MonadSite (..)
  ) where

import Yesod.Site.Util

import Control.Monad.Reader
import Yesod.Core.Types

class (forall site. MonadIO (m site)) => MonadSite (m :: * -> * -> *) where
  askSite :: m site site

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

