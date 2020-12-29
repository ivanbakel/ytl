{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Trans.TH
  ( defaultYesodInstanceExcept
  ) where

import Yesod.Site.Util
import Yesod.Trans.Class

import Data.Coerce
import Yesod.Core
  ( Approot (..)
  , guessApproot
  , HtmlUrl
  , RenderRoute (..)
  , ScriptLoadPosition (..)
  , Yesod (..)
  )

import Language.Haskell.TH

coerceHtmlUrl
  :: SiteCompatible site site'
  => HtmlUrl (Route site) -> HtmlUrl (Route site')
coerceHtmlUrl url = url . (. coerce)

-- | Fills in an instance for the 'Yesod' class for a 'SiteTrans' wrapper with
-- implementations which just invoke the base class, *except* for those implementations
-- which are defined on the instance itself.
--
-- This is useful for 'SiteTrans' implementations that want to modify some of
-- the 'Yesod' behaviour of the site, but mostly want to delegate behaviour to
-- the base site. Instead of writing out a whole 'Yesod' instance, you can just
-- override the class methods that you need. The rest will be filled in with
-- working implementations that do what you expect.
defaultYesodInstanceExcept
  :: Q Exp -- ^ How to go from the wrapped site to the base site.
           -- If the instance is for a type of the form 't site', then this
           -- should be an expression of type 't site -> site'. This operation
           -- is necessary to use some of the default implementations.
  -> Q [Dec] -- ^ The partial 'Yesod' instance. Should just be a since @instance@
             -- declaration, except that its body can be as empty as you like. For
             -- example:
             --
             -- @
             -- defaultYesodInstanceExcept [| myLowerer |] [d|
             --   instance (Yesod site) => Yesod (MyWrapper site) where
             --     yesodMiddleware = ... -- insert some middleware
             --
             --     -- But everything else should be defined in the default way
             --   |]
             -- @
             --
             -- This declaration will include the custom definition for
             -- 'yesodMiddleware', as well as implementations for the other
             -- class methods that just delegate to the base class.
  -> Q [Dec]
defaultYesodInstanceExcept baseSite partialInstanceQ = do
  [InstanceD overlap ctxt instanceHead exceptions] <- partialInstanceQ
  defaultImplementations <- defaultImplementationsQ

  let fullBody = exceptions <> (filter (`undeclaredIn` exceptions) defaultImplementations)

  pure [InstanceD overlap ctxt instanceHead fullBody]
  where
    decName :: Dec -> Maybe Name
    decName (FunD name _) = Just name
    decName (ValD (VarP name) _ _) = Just name
    decName _ = Nothing

    undeclaredIn :: Dec -> [Dec] -> Bool
    undeclaredIn dec
      | Just name <- decName dec
      = not . any (maybe False (== name) . decName)
    undeclaredIn _ = const True

    defaultImplementationsQ = [d|
      $(pure $ VarP 'approot) = case $([|approot|]) of
        ApprootRelative -> ApprootRelative
        ApprootStatic t -> ApprootStatic t
        ApprootMaster f -> ApprootMaster (f . $(baseSite))
        ApprootRequest f -> ApprootRequest (f. $(baseSite))
        -- Approot is non-exhaustive, so for API compatibility, we need a
        -- (apparently redundant) fallthrough case
        _ -> guessApproot

      $(pure $ VarP 'errorHandler) = lift . $([|errorHandler|])

      $(pure $ VarP 'defaultLayout) = mapSiteT $([|defaultLayout|])

      $(pure $ VarP 'urlParamRenderOverride) = \site route ->
        $([|urlParamRenderOverride|]) ($(baseSite) site) (coerce route)

      $(pure $ VarP 'isAuthorized) = \route isWrite ->
        lift ($([|isAuthorized|]) (coerce route) isWrite)

      $(pure $ VarP 'isWriteRequest)
        = lift . $([|isWriteRequest|]) . coerce

      $(pure $ VarP 'authRoute)
        = fmap coerce . $([|authRoute|]) . $(baseSite)

      $(pure $ VarP 'cleanPath) = $([|cleanPath|]) . $(baseSite)

      $(pure $ VarP 'joinPath) = $([|joinPath|]) . $(baseSite)

      $(pure $ VarP 'addStaticContent) = \fn mime content -> do
        ret <- lift $ $([|addStaticContent|]) fn mime content
        pure $ case ret of
          Nothing -> Nothing
          Just (Left t) -> Just (Left t)
          Just (Right (route, params))
            -> Just (Right (coerce route, params))

      $(pure $ VarP 'maximumContentLength) = \site mRoute ->
        $([|maximumContentLength|]) ($(baseSite) site) (coerce <$> mRoute)

      $(pure $ VarP 'maximumContentLengthIO) = \site mRoute ->
        $([|maximumContentLengthIO|]) ($(baseSite) site) (coerce <$> mRoute)

      $(pure $ VarP 'makeLogger)
         = $([|makeLogger|]) . $(baseSite)

      $(pure $ VarP 'messageLoggerSource)
        = $([|messageLoggerSource|]) . $(baseSite)

      $(pure $ VarP 'jsLoader) = \site ->
        case $([|jsLoader|]) ($(baseSite) site) of
            BottomOfBody -> BottomOfBody
            BottomOfHeadBlocking -> BottomOfHeadBlocking
            BottomOfHeadAsync async
              -> BottomOfHeadAsync
                  (\urls mHtml ->
                      coerceHtmlUrl $ async urls (coerceHtmlUrl <$> mHtml))

      $(pure $ VarP 'jsAttributes)
        = $([|jsAttributes|]) . $(baseSite)

      $(pure $ VarP 'jsAttributesHandler)
        = lift $([|jsAttributesHandler|])

      $(pure $ VarP 'makeSessionBackend)
        = $([|makeSessionBackend|]) . $(baseSite)

      $(pure $ VarP 'fileUpload)
        = $([|fileUpload|]) . $(baseSite)

      $(pure $ VarP 'shouldLogIO)
        = $([|shouldLogIO|]) . $(baseSite)

      $(pure $ VarP 'yesodMiddleware)
        = mapSiteT $([|yesodMiddleware|])

      $(pure $ VarP 'yesodWithInternalState) = \site mRoute ->
        $([|yesodWithInternalState|]) ($(baseSite) site) (coerce <$> mRoute)

      $(pure $ VarP 'defaultMessageWidget) = \html url ->
        lift $ $([|defaultMessageWidget|]) html (url . coerce)
      |]
