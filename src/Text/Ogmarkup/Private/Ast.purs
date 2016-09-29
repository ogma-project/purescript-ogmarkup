{-|
Module      : Text.Ogmarkup.Private.Ast
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental
An abstract representation of an ogmarkup document.
-}

module Text.Ogmarkup.Private.Ast where

import Prelude (class Eq, class Show)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gShow)

-- | A ogmarkup document internal representation waiting to be used in order
--   to generate an output.
type Document = List Section

-- | A Section within an ogmarkup document is a sequence of paragraphs. It
-- can be part of the story or an aside section like a letter, a song, etc.
-- We make the distinction between the two cases because we want to be able
-- to apply different style depending on the situation.
data Section =
    Story (List Paragraph)                -- ^ The story as it goes
  | Aside (Maybe String) (List Paragraph) -- ^ Something else. Maybe a letter, a flashback, etc.
  | Failing String

-- | A Paragraph is just a sequence of Component.
type Paragraph = List Component

-- | A Component is either a narrative text, a character's line of dialogue or
-- a character's inner thought.
--
-- We also embed an error Component in case we fail to parse a valid
-- component. This way, we can resume parsing when we meet a new paragraph.
data Component =
    Teller (List Format)          -- ^ A narrative description
  | Dialogue Reply (Maybe String) -- ^ A dialogue reply
  | Thought Reply (Maybe String)  -- ^ Inner dialogue of the character
  | IllFormed String              -- ^ If none of the above matched, then output
                                  --   what follows as-is, the parsing will
                                  --   be resumed at the next paragraph

-- | A character's line of dialogue. A reply may contain a descriptive part, which
--   is not part of what the character actually says or thinks. We call the
--   latter a "with say" reply untill someone gives use a better name for it.
data Reply =
  -- | A reply of the form: "Good morning."
  Simple (List Format)
  -- | A reply of the form: "Good morning," she says. "How are you?"
  | WithSay (List Format) (List Format) (List Format)

-- | A nested formatted text
data Format =
  Raw (List Atom)                -- ^ No particular emphasis is required on this sequence
  | Emph (List Format)           -- ^ Surrounded by @*@.
  | StrongEmph (List Format)     -- ^ Surrounded by @**@.
  | Quote (List Format)

-- | An Atom is the atomic component of a Ogmarkup document. It can be either a
--   punctuation mark or a word, that is a string.
--
--   Note that, by construction, 'OpenQuote' and 'CloseQuote' are not valid
--   'Mark' values here.  Indeed, they are implicit with the 'Quote'
--   constructor. This design allows the parser to enforce that an opened quote
--   needs to be closed.
data Atom =
    Word String      -- ^ A wrapped string
  | Punctuation Mark -- ^ A punctuation mark


-- | Mostly in order to deal with typographic spaces, main
--   punctuation marks are tokenized during the parsing of an
--   Ogmarkup document.
data Mark =
    Semicolon        -- ^ The character @;@
  | Colon            -- ^ The character @,@
  | Question         -- ^ The character @?@
  | Exclamation      -- ^ The character @!@
  | OpenQuote        -- ^ The character @"@
  | CloseQuote       -- ^ The character @"@
  | Dash             -- ^ The character – or the sequence @--@
  | LongDash         -- ^ The character — or the sequence @---@
  | Comma            -- ^ The character @,@
  | Point            -- ^ The character @.@
  | Hyphen           -- ^ The character @-@
  | SuspensionPoints -- ^ Two or more @.@ or the character …
  | Apostrophe       -- ^ The characters @'@ or @’@

-- Generic instances

derive instance genericSection :: Generic Section
derive instance genericComp    :: Generic Component
derive instance genericReply   :: Generic Reply
derive instance genericFormat  :: Generic Format
derive instance genericAtom    :: Generic Atom
derive instance genericMark    :: Generic Mark

-- Eq instances

derive instance eqSection :: Eq Section
derive instance eqComp    :: Eq Component
derive instance eqReply   :: Eq Reply
derive instance eqFormat  :: Eq Format
derive instance eqAtom    :: Eq Atom
derive instance eqMark    :: Eq Mark

-- Show instances

instance showSection :: Show Section where
  show = gShow
instance showComp    :: Show Component where
  show = gShow
instance showReply   :: Show Reply where
  show = gShow
instance showFormat  :: Show Format where
  show = gShow
instance showAtom    :: Show Atom where
  show = gShow
instance showMark    :: Show Mark where
  show = gShow
