{-|
Module      : Text.Ogmarkup.Private.Typography
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

This module provides the 'Typography' datatype along with two default instances
for French and English.
-}

module Text.Ogmarkup.Private.Typography where

import Data.Ord     (class Ord, Ordering(..))
import Data.Eq      (class Eq)
import Prelude      (($), pure)
import Data.Maybe   (Maybe(..))

import Text.Ogmarkup.Private.Ast as Ast

-- * Inner spaces representation

-- | Deal with typographic spaces, especially when it comes to
--   separating two texts. Because Space derives Ord, it is possible
--   to use min and max to determine which one to use in case of
--   a conflict.
data Space =
  Normal -- ^ A normal space that can be turned into a newline for displaying.
  | Nbsp -- ^ A non breakable space, it cannot be turned into a newline.
  | None -- ^ No space at all.

instance spaceEq :: Eq Space where
  eq Normal Normal = true
  eq Nbsp Nbsp = true
  eq None None = true
  eq _ _ = false

instance spaceOrd :: Ord Space where
  compare Normal Normal = EQ
  compare Normal _ = LT
  compare _ Normal = GT
  compare Nbsp Nbsp = EQ
  compare Nbsp _ = LT
  compare _ Nbsp = GT
  compare None None = EQ

-- * Typography definition

type MarkFmt = { before :: Space
               , after  :: Space
               , string :: String
               }

-- | A Typography is a data type that tells the caller what space
--   should be privileged before and after a text.
type Typography = { decide :: Ast.Mark -> MarkFmt
                    -- ^ For a given 'Ast.Mark', returns a tuple with the spaces
                    -- to use before and after the punctuation mark and its
                    -- output value.
                  , openDialogue  :: Boolean -> Maybe Ast.Mark
                    -- ^ Which mark to use to open a dialogue. If the parameter
                    -- is True, there were another dialogue just before.
                  , closeDialogue :: Boolean -> Maybe Ast.Mark
                  -- ^ Which mark to use to close a dialogue. If the parameter
                  -- is True, there is another dialogue just after.
                  }

-- | From a Typography, it gives the space to privilege before the
--   input Text.
beforeAtom :: Typography
           -> Ast.Atom
           -> Space
beforeAtom t (Ast.Punctuation m) = (t.decide m).before
beforeAtom t _ = Normal

-- | From a Typography, it gives the space to privilege after the
--   input Text.
afterAtom :: Typography
          -> Ast.Atom
          -> Space
afterAtom t (Ast.Punctuation m) = (t.decide m).after
afterAtom t _ = Normal

-- | Normalize the input in order to add it to a generated Text.
normalizeAtom :: Typography
              -> Ast.Atom
              -> String
normalizeAtom t (Ast.Punctuation m) = (t.decide m).string
normalizeAtom t (Ast.Word w) = w

-- * Ready-to-use Typography

-- | A proposal for the French typography. It can be used with several generation
--   approaches, as it remains very generic.
frenchTypo :: Typography
frenchTypo = { decide:        t
             , openDialogue:  prevT
             , closeDialogue: nextT
             }
  where
    t :: Ast.Mark -> MarkFmt
    t Ast.Semicolon = { before: Nbsp
                      , after:  Nbsp
                      , string: ";"
                      }
    t Ast.Colon = { before: Nbsp
                  , after:  Normal
                  , string: ":"
                  }
    t Ast.OpenQuote = { before: Normal
                      , after:  Nbsp
                      , string: "«"
                      }
    t Ast.CloseQuote = { before: Nbsp
                       , after:  Normal
                       , string: "»"
                       }
    t Ast.Question = { before: Nbsp
                     , after:  Normal
                     , string: "?"
                     }
    t Ast.Exclamation = { before: Nbsp
                        , after:  Normal
                        , string: "!"
                        }
    t Ast.LongDash = { before: Normal
                     , after:  Normal
                     , string: "—"
                     }
    t Ast.Dash = { before: None
                 , after:  None
                 , string: "–"
                 }
    t Ast.Hyphen = { before: None
                   , after:  None
                   , string: "-"
                   }
    t Ast.Comma = { before: None
                  , after:  Normal
                  , string: ","
                  }
    t Ast.Point = { before: None
                  , after:  Normal
                  , string: "."
                  }
    t Ast.Apostrophe = { before: None
                       , after:  None
                       , string: "’"
                       }
    t Ast.SuspensionPoints = { before: None
                             , after:  Normal
                             , string: "…"
                             }

    prevT true = Just Ast.LongDash
    prevT false = Just Ast.OpenQuote

    nextT true = Nothing
    nextT false = Just Ast.CloseQuote

-- | A proposal for the English typography. It can be used with several generation
--   approaches, as it remains very generic.
englishTypo :: Typography
englishTypo = { decide:        t
              , openDialogue:  (pure $ Just Ast.OpenQuote)
              , closeDialogue: (pure $ Just Ast.CloseQuote)
              }
  where
    t :: Ast.Mark -> MarkFmt
    t Ast.Semicolon = { before: None
                      , after:  Normal
                      , string: ";"
                      }
    t Ast.Colon = { before: None
                  , after:  Normal
                  , string: ":"
                  }
    t Ast.OpenQuote = { before: Normal
                      , after:  None
                      , string: "“"
                      }
    t Ast.CloseQuote = { before: None
                       , after:  Normal
                       , string: "”"
                       }
    t Ast.Question = { before: None
                     , after:  Normal
                     , string: "?"
                     }
    t Ast.Exclamation = { before: None
                        , after:  Normal
                        , string: "!"
                        }
    t Ast.LongDash = { before: Normal
                     , after:  None
                     , string: "—"
                     }
    t Ast.Dash = { before: None
                 , after:  None
                 , string: "–"
                 }
    t Ast.Hyphen = { before: None
                   , after:  None
                   , string: "-"
                   }
    t Ast.Comma = { before: None
                  , after:  Normal
                  , string: ","
                  }
    t Ast.Point = { before: None
                  , after:  Normal
                  , string: "."
                  }
    t Ast.Apostrophe = { before: None
                       , after:  None
                       , string: "'"
                       }
    t Ast.SuspensionPoints = { before: None
                             , after:  Normal
                             , string: "…"
                             }

