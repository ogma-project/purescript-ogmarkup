{-|
Module      : Text.Ogmarkup
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

The ogmarkup library provides an ogmarkup document compiler. This module is the
only one you should need to import in your project.

The library is still in an early stage of development, hence the "experimental"
stability. Be aware the exposed interface may change in future realase.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Ogmarkup
    ( module Text.Ogmarkup.Private.Config
    , module Text.Ogmarkup.Private.Ast
    , module Text.Ogmarkup.Private.Generator
    , module Text.Ogmarkup.Private.Typography
      -- * Parse and Generate
    , ogmarkup
    ) where

import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)

import Text.Ogmarkup.Private.Config (GenConf, Template, htmlGenConfig)
import Text.Ogmarkup.Private.Ast (Mark(..))
import Text.Ogmarkup.Private.Generator (document, runGenerator)
import Text.Ogmarkup.Private.Parser as Parser
import Text.Ogmarkup.Private.Typography

-- | From a String, parse and generate an output according to a generation configuration.
--   The inner definitions of the parser and the generator imply that the output
--   type has to be an instance of the 'IsString' and 'Monoid' classes.
ogmarkup :: forall a
          . Monoid a
         => String         -- ^ The input string
         -> GenConf a   -- ^ The generator configuration
         -> a
ogmarkup input conf = case Parser.parse Parser.document input of
                        Right ast -> runGenerator (document ast) conf
                        Left _    -> mempty -- should not happen
