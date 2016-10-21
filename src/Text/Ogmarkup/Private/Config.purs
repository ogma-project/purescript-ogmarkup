{-|
Module      : Text.Ogmarkup.Private.Config
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

This module provides the 'GenConf' typeclass which is used to configure the
'Text.Ogmarkup.Private.Generator's monad.
-}

module Text.Ogmarkup.Private.Config where

import Prelude     (id)
import Data.Monoid (append)
import Data.Maybe  (Maybe(..), fromMaybe)

import Text.Ogmarkup.Private.Typography (Typography, Space(..))

-- | A 'Template' is just synonym for a template of one argument.
type Template a = a -> a

-- | An instance of the 'GenConf' typeclass can be given as a parameter to
-- a 'Text.Ogmarkup.Private.Generator.Generator'.  In order to prevent GHC
-- to overabstract this typeclass, there can be only one instance of
-- GenConf for one datatype. In other words, one datatype can only handle
-- one return type @c@.
--
-- For each template, we give a prefered layout and some hints about the
-- default implementation (which is almost always the identity function,
-- ignoring all the parameters but the generated output.
--
-- __Warning:__ 'GenConf' is a multiparam typeclass (see
-- @MultiParamTypeClasses@ GHC extension). In order to make new instances,
-- the following extensions need to be enabled:
--
--     * @FlexibleInstances@
--     * @MultiParamTypeClasses@
--
-- Otherwise, GHC will not accept your instance statement.
data GenConf a = GC { typography         :: Typography a
                    , documentTemplate   :: Template a
                    , errorTemplate      :: Template a
                    , storyTemplate      :: Template a
                    , asideTemplate      :: Maybe String -> Template a
                    , paragraphTemplate  :: Template a
                    , tellerTemplate     :: Template a
                    , dialogueTemplate   :: String -> Template a
                    , thoughtTemplate    :: String -> Template a
                    , replyTemplate      :: Template a
                    , betweenDialogue    :: a
                    , emphTemplate       :: Template a
                    , strongEmphTemplate :: Template a
                    , authorNormalize    :: Maybe String -> String
                    , printSpace         :: Space -> a
                    }

htmlGenConfig :: Typography String
              -> GenConf String
htmlGenConfig t = GC { typography:         t
                     , documentTemplate:   htmlDoc
                     , errorTemplate:      htmlError
                     , storyTemplate:      htmlStory
                     , asideTemplate:      htmlAside
                     , paragraphTemplate:  htmlPar
                     , tellerTemplate:     id
                     , dialogueTemplate:   htmlDial
                     , thoughtTemplate:    htmlThou
                     , replyTemplate:      htmlReply
                     , betweenDialogue:    "</p><p>"
                     , emphTemplate:       htmlEmph
                     , strongEmphTemplate: htmlStrongEmph
                     , authorNormalize:    \s -> case s of Just a -> a
                                                           _      -> ""
                     , printSpace:         \s -> case s of Normal -> " "
                                                           Nbsp   -> "&nbsp;"
                                                           _      -> ""
                     }
  where
    htmlDoc doc = template "<article>" doc "</article>"
    htmlError err = template "<span class=\"ogma-error\">" err "</span>"
    htmlReply r = template "<span class=\"ogma-reply\">" r "</span>"
    htmlAside cls txt = let open = template "<blockquote class=\"ogma-aside "
                                            (fromMaybe "" cls)
                                            "\">"
                        in template open txt "</blockquote>"
    htmlStory txt = template "<div class=\"ogma-story\">" txt "</div>"
    htmlDial aut txt = let open = template "<span class=\"ogma-dialogue "
                                           aut
                                           "\">"
                       in template open txt "</span>"
    htmlThou aut txt = let open = template "<span class=\"ogma-thought "
                                           aut
                                           "\">"
                       in template open txt "</span>"
    htmlPar p = template "<p>" p "</p>"
    htmlEmph txt = template  "<em>" txt "</em>"
    htmlStrongEmph txt = template "<strong>" txt "</strong>"

    template b t a = b `append` t `append` a
