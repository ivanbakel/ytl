{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Yesod.Site.Util
  ( SiteCompatible

  , getSite
  , getWidgetSite

  , withSite
  , withWidgetSite
  ) where

import Data.Coerce
import Yesod.Core.Types

getSite :: HandlerData child site -> site
getSite = rheSite . handlerEnv

getWidgetSite :: WidgetData site -> site
getWidgetSite = getSite . wdHandler

type SiteCompatible site site' = (Coercible (Route site) (Route site'), Coercible (Route site') (Route site))

withWidgetSite
  :: SiteCompatible site site'
  => (site -> site')
  -> WidgetData site
  -> WidgetData site'
withWidgetSite f WidgetData{..}
  = WidgetData
      { wdRef = coerce wdRef
      , wdHandler = withSite f wdHandler
      }

withSite
  :: SiteCompatible site site'
  => (site -> site')
  -> HandlerData site site
  -> HandlerData site' site'
withSite f = withSubSite f . withSuperSite f

withSubSite
  :: SiteCompatible site site'
  => (site -> site')
  -> HandlerData site parent
  -> HandlerData site' parent
withSubSite f HandlerData{..}
  = let RunHandlerEnv{..} = handlerEnv
    in
      HandlerData
        { handlerEnv = RunHandlerEnv
            { rheChild = f rheChild
            , rheRoute = coerce rheRoute
            , rheRouteToMaster = rheRouteToMaster . coerce
            , ..
            }
        , ..
        }

withSuperSite
  :: SiteCompatible site site'
  => (site -> site')
  -> HandlerData child site
  -> HandlerData child site'
withSuperSite f HandlerData{..}
  = let RunHandlerEnv{..} = handlerEnv
    in
      HandlerData
        { handlerEnv = RunHandlerEnv
            { rheSite = f rheSite
            , rheRender = rheRender . coerce
            , rheRouteToMaster = coerce . rheRouteToMaster
            , ..
            }
        , ..
        }

