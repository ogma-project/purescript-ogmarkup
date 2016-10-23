{-|
Module      : Text.Ogmarkup.Private.Parser
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

This module provides several parsers that can be used in order to
extract the 'Ast' of an Ogmarkup document.

Please consider that only 'document' should be used outside this
module.
-}

module Text.Ogmarkup.Private.Parser where

import Prelude             (Unit, unit, bind, ($), pure, (<*), (*>), (<$>), (-),
                            (>), (<=), (>=), (&&), (||), (==))
import Data.Either         (Either)
import Control.Alt         ((<|>))
import Control.Monad.Aff   (Aff, later)
import Control.Monad       (class Monad)
import Data.Monoid         (append)
import Data.Maybe          (Maybe)
import Data.List           (many, some) as L
import Data.Array          (many, some, fromFoldable) as A
import Data.Identity       (Identity(..))
import Data.List           (List(..), reverse)
import Data.String         (fromCharArray, toCharArray)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser (ParserT(..), fail, ParseError, runParserT, PState(..))
import Text.Parsing.Parser.Combinators (optional, optionMaybe, notFollowedBy, manyTill, many1Till, try, lookAhead)
import Text.Parsing.Parser.String (eof, string, anyChar, char, skipSpaces, oneOf, satisfy)
import Control.Monad.State (StateT(..), get, put, lift, evalStateT, runStateT)
import Text.Ogmarkup.Private.Ast     as Ast

-- | Keep track of the currently opened formats.
type ParserState = { -- | Already parsing text with emphasis
                     parseWithEmph        :: Boolean
                     -- | Already parsing text with strong
                     --   emphasis
                   , parseWithStrongEmph  :: Boolean
                     -- | Already parsing a quote
                   , parseWithinQuote     :: Boolean
                   }

-- | An ogmarkup parser processes 'Char' tokens and carries a 'ParserState'.
type OgmarkupParserT m a = ParserT String (StateT ParserState m) a
type OgmarkupParserPure a = ParserT String (StateT ParserState Identity) a
type OgmarkupParserAff eff a = ParserT String (StateT ParserState (Aff eff)) a

-- use to force pure computation
liftParser :: forall m a
            . Monad m
           => OgmarkupParserPure a
           -> OgmarkupParserT m a
liftParser (ParserT parser) =
  ParserT $ \ps ->
    StateT $ \st -> case runStateT (parser ps) st of Identity res -> pure res

later' :: forall eff a
        . OgmarkupParserAff eff a -> OgmarkupParserAff eff a
later' (ParserT parser) =
  ParserT \ps ->
    StateT \st -> later $ runStateT (parser ps) st

sget :: forall m
      . Monad m
     => OgmarkupParserT m ParserState
sget = lift get

sput :: forall m
      . Monad m
     => ParserState
     -> OgmarkupParserT m Unit
sput t = lift $ put t

-- | Update the 'ParserState' to guard against nested emphasis.
enterEmph :: forall m
           . Monad m
          => OgmarkupParserT m Unit
enterEmph = do st <- sget
               if st.parseWithEmph
                 then fail "guard against nested emphasis"
                 else do sput $ st { parseWithEmph = true }
                         pure unit

-- | Update the 'ParserState' to be able to parse insput with emphasis
-- again.
leaveEmph :: forall m
           . Monad m
          => OgmarkupParserT m Unit
leaveEmph = do st <- sget
               if st.parseWithEmph
                 then do sput st { parseWithEmph = false }
                         pure unit
                 else fail "cannot leave emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested strong emphasis.
enterStrongEmph :: forall m
                 . Monad m
                => OgmarkupParserT m Unit
enterStrongEmph = do st <- sget
                     if st.parseWithStrongEmph
                       then fail "guard against nested strong emphasis"
                       else do sput st { parseWithStrongEmph = true }
                               pure unit

-- | Update the 'ParserState' to be able to parse insput with strong emphasis
-- again.
leaveStrongEmph :: forall m
                 . Monad m
                => OgmarkupParserT m Unit
leaveStrongEmph = do st <- sget
                     if st.parseWithStrongEmph
                       then do sput st { parseWithStrongEmph = false }
                               pure unit
                       else fail "cannot leave strong emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested quoted insputs.
enterQuote :: forall m
            . Monad m
           => OgmarkupParserT m Unit
enterQuote = do st <- sget
                if st.parseWithinQuote
                  then fail "guard against nested quotes"
                  else do sput st { parseWithinQuote = true }
                          pure unit

-- | Update the 'ParserState' to be able to parse an insput
-- surrounded by quotes again.
leaveQuote :: forall m
            . Monad m
           => OgmarkupParserT m Unit
leaveQuote = do st <- sget
                if st.parseWithinQuote
                  then do sput st { parseWithinQuote = false }
                          pure unit
                  else fail "cannot leave quote when you did not enter"

-- | A initial ParserState instance to be used at the begining of
-- a document parsing.
initParserState :: ParserState
initParserState = { parseWithEmph: false
                  , parseWithStrongEmph: false
                  , parseWithinQuote: false
                  }

-- | A wrapper around the 'runParser' function of Megaparsec. It uses
-- 'initParserState' as an initial state.
parse :: forall m a
      . Monad m
      => OgmarkupParserT m a
      -> String
      -> m (Either ParseError a)
parse ogma content = evalStateT (runParserT (PState { input: content, position: initialPos }) ogma) initParserState

-- | Try its best to parse an ogmarkup document. When it encounters an
--   error, it pures an Ast and the remaining input.
--
--   See 'Ast.Document'.
document :: forall eff
          . OgmarkupParserAff eff Ast.Document
document = doc Nil
  where doc :: Ast.Document
             -> OgmarkupParserAff eff Ast.Document
        doc ast = do skipSpaces
                     sects <- many (try section)
                     let ast' = ast `append` sects
                     (eof *> pure ast') <|> (recover ast' `bind` doc)

        recover :: Ast.Document
                -> OgmarkupParserAff eff Ast.Document
        recover ast = do failure <- A.fromFoldable <$> many1Till anyChar (char '\n')
                         pure $ ast `append` (Cons (Ast.Failing $ fromCharArray failure) Nil)
-- | See 'Ast.Section'.
section :: forall eff
         . OgmarkupParserAff eff Ast.Section
section = aside <|> story

-- | See 'Ast.Aside'.
aside :: forall eff
       . OgmarkupParserAff eff Ast.Section
aside = do asideSeparator
           cls <- optionMaybe asideClass
           skipSpaces
           ps <- some (paragraph <* skipSpaces)
           asideSeparator
           manyTill skipSpaces (skip (char '\n') <|> eof)
           skipSpaces

           pure $ Ast.Aside cls ps
  where
    asideClass :: OgmarkupParserAff eff String
    asideClass = do cls <- some' letterChar
                    asideSeparator

                    pure cls
    letterChar = satisfy (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

-- | See 'Ast.Story'.
story :: forall eff
       . OgmarkupParserAff eff Ast.Section
story = Ast.Story <$> some (paragraph <* skipSpaces)

-- | See 'Ast.Paragraph'.
paragraph :: forall eff
           . OgmarkupParserAff eff Ast.Paragraph
paragraph = some component <* blank

-- | See 'Ast.Component'.
component :: forall eff
           . OgmarkupParserAff eff Ast.Component
component = later' $ liftParser $ try (dialogue <|> thought <|> teller) <|> illformed

-- | See 'Ast.IllFormed'.
illformed :: forall m
           . Monad m
          => OgmarkupParserT m Ast.Component
illformed = Ast.IllFormed <$> restOfParagraph

-- | Parse the rest of the current paragraph with no regards for the
-- ogmarkup syntax. This Parser is used when the document is ill-formed, to
-- find a new point of synchronization.
restOfParagraph :: forall m
                 . Monad m
                => OgmarkupParserT m String
restOfParagraph = do lookAhead anyChar
                     notFollowedBy endOfParagraph
                     str <- A.fromFoldable <$> manyTill anyChar (lookAhead $ try endOfParagraph)
                     pure $ fromCharArray str

-- | See 'Ast.Teller'.
teller :: forall m
        . Monad m
       => OgmarkupParserT m Ast.Component
teller = Ast.Teller <$> some format

-- | See 'Ast.Dialogue'.
dialogue :: forall m
          . Monad m
         => OgmarkupParserT m Ast.Component
dialogue = talk '[' ']' Ast.Dialogue

-- | See 'Ast.Thought'.
thought :: forall m
         . Monad m
        => OgmarkupParserT m Ast.Component
thought = talk '<' '>' Ast.Thought

-- | @'talk' c c' constr@ wraps a reply surrounded by @c@ and @c'@ inside
--   @constr@ (either 'Ast.Dialogue' or 'Ast.Thought').
talk :: forall m
      . Monad m
     => Char -- ^ A character to mark the begining of a reply
     -> Char -- ^ A character to mark the end of a reply
     -> (Ast.Reply -> Maybe String -> Ast.Component) -- ^ Either 'Ast.Dialogue' or 'Ast.Thought' according to the situation
     -> OgmarkupParserT m Ast.Component
talk c c' constructor = do
  rep <- reply c c'
  auth <- optionMaybe characterName
  blank

  pure $ constructor rep auth

-- | Parse the name of the character which speaks or thinks. According to
-- the ogmarkup syntax, it is surrounded by parentheses.
characterName :: forall m
               . Monad m
              => OgmarkupParserT m String
characterName = do
  char '('
  notFollowedBy (char ')')
  auth <- A.fromFoldable <$> manyTill anyChar (char ')')

  pure $ fromCharArray auth

-- | 'reply' parses a 'Ast.Reply'.
reply :: forall m
       . Monad m
      => Char
      -> Char
      -> OgmarkupParserT m Ast.Reply
reply c c' = do char c
                blank
                p1 <- some format
                x <- oneOf ['|', c']

                case x of '|' -> do blank
                                    ws <- some format
                                    char '|'
                                    blank
                                    p2 <- many format
                                    char c'

                                    pure $ Ast.WithSay p1 ws p2
                          _ -> pure $ Ast.Simple p1


-- | See 'Ast.Format'.
format :: forall m
        . Monad m
       => OgmarkupParserT m Ast.Format
format = raw <|> emph <|> strongEmph <|> quote
  where
    emph :: OgmarkupParserT m Ast.Format
    emph = do char '*'
              blank
              enterEmph
              f <- format
              fs <- manyTill format (char '*' *> blank)
              leaveEmph
              pure $ Ast.Emph (Cons f fs)

    strongEmph :: OgmarkupParserT m Ast.Format
    strongEmph = do char '+'
                    blank
                    enterStrongEmph
                    f <- format
                    fs <- manyTill format (char '+' *> blank)
                    leaveStrongEmph
                    pure $ Ast.StrongEmph (Cons f fs)

    quote :: OgmarkupParserT m Ast.Format
    quote = do openQuote
               enterQuote
               f <- format
               fs <- manyTill format closeQuote
               leaveQuote
               pure $ Ast.Quote (Cons f fs)

-- | See 'Ast.Raw'.
raw :: forall m
     . Monad m
    => OgmarkupParserT m Ast.Format
raw = Ast.Raw <$> some atom


-- | See 'Ast.Atom'.
atom :: forall m
      . Monad m
     => OgmarkupParserT m Ast.Atom
atom = (word <|> mark <|> longword) <* blank

-- | See 'Ast.Word'. This parser does not consume the following skipSpacess, so
--   the caller needs to take care of it.
word :: forall m
      . Monad m
     => OgmarkupParserT m Ast.Atom
word = do notFollowedBy endOfWord
          str <- A.fromFoldable <$> many1Till anyChar (lookAhead endOfWord)
          pure $ Ast.Word (fromCharArray str)
  where
    endOfWord :: OgmarkupParserT m Unit
    endOfWord = eof <|> whiteSpace <|> (skip $ oneOf specChar)

    specChar :: Array Char
    specChar = toCharArray "\"«»`+*[]<>|_'’.,;-–—!?:"

-- | Wrap a raw string surrounded by @`@ inside a 'Ast.Word'.
--
--   *>> parse longword "" "`test *ei*`"
--   Right (Ast.Word "test *ei*")
--
--   Therefore, @`@ can be used to insert normally reserved symbol
--   inside a generated document.
longword :: forall m
          . Monad m
         => OgmarkupParserT m Ast.Atom
longword = do char '`'
              notFollowedBy (char '`')
              str <- A.fromFoldable <$> manyTill anyChar (char '`')
              pure $ Ast.Word (fromCharArray str)

-- | See 'Ast.Punctuation'. Be aware that 'mark' does not parse the quotes
--   because they are processed 'quote'.
mark :: forall m
      . Monad m
     => OgmarkupParserT m Ast.Atom
mark = Ast.Punctuation <$> (semicolon
        <|> colon
        <|> question
        <|> exclamation
        <|> try longDash
        <|> try dash
        <|> hyphen
        <|> comma
        <|> apostrophe
        <|> try suspensionPoints
        <|> point)
  where
    parseMark :: forall t
               . OgmarkupParserT m t
              -> Ast.Mark
              -> OgmarkupParserT m Ast.Mark
    parseMark p m = p *> pure m

    semicolon        = parseMark (char ';') Ast.Semicolon
    colon            = parseMark (char ':') Ast.Colon
    question         = parseMark (char '?') Ast.Question
    exclamation      = parseMark (char '!') Ast.Exclamation
    longDash         = parseMark (string "—" <|> string "---") Ast.LongDash
    dash             = parseMark (string "–" <|> string "--") Ast.Dash
    hyphen           = parseMark (char '-') Ast.Hyphen
    comma            = parseMark (char ',') Ast.Comma
    point            = parseMark (char '.') Ast.Point
    apostrophe       = parseMark (char '\'' <|> char '’') Ast.Apostrophe
    suspensionPoints = parseMark (string ".." *> many' (char '.')) Ast.SuspensionPoints

-- | See 'Ast.OpenQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
openQuote :: forall m
           . Monad m
          => OgmarkupParserT m Unit
openQuote = do char '«' <|> char '"'
               blank

-- | See 'Ast.CloseQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
closeQuote :: forall m
            . Monad m
           => OgmarkupParserT m Unit
closeQuote = do char '»' <|> char '"'
                blank

-- | An aside section (see 'Ast.Aside') is a particular region
--   surrounded by two lines of underscores (at least three).
--   This parser consumes one such line.
asideSeparator :: forall m
                . Monad m
               => OgmarkupParserT m Unit
asideSeparator = do string "__"
                    some (char '_')
                    pure unit


-- | The end of a paragraph is the end of the document or two blank lines
-- or an aside separator, that is a line of underscores.
endOfParagraph :: forall m
                . Monad m
               => OgmarkupParserT m Unit
endOfParagraph = try betweenTwoSections
                 <|> asideSeparator
                 <|> eof
  where
    betweenTwoSections :: OgmarkupParserT m Unit
    betweenTwoSections = do count 2 $ manyTill whiteSpace (eof <|> skip (char '\n'))
                            skipSpaces


-- | This parser consumes all the white spaces until it finds either an aside
--   surrounding marker (see 'Ast.Aside'), the end of the document or
--   one blank line. The latter marks the end of the current paragraph.
blank :: forall m
       . Monad m
      => OgmarkupParserT m Unit
blank = do skip $ optional (notFollowedBy endOfParagraph *> skipSpaces)

-- | @skip p@ parses @p@ and skips the result.
skip :: forall m b
      . Monad m
     => OgmarkupParserT m b
     -> OgmarkupParserT m Unit
skip p = do p
            pure unit

many' :: forall m
       . Monad m
      => OgmarkupParserT m Char
      -> OgmarkupParserT m String
many' p = fromCharArray <$> A.many p

some' :: forall m
       . Monad m
      => OgmarkupParserT m Char
      -> OgmarkupParserT m String
some' p = fromCharArray <$> A.some p

many :: forall m a
      . Monad m
     => OgmarkupParserT m a
     -> OgmarkupParserT m (List a)
many p = (do x <- p
             rs <- many p
             pure (Cons x rs)) <|>
         (pure Nil)

some :: forall m a
      . Monad m
     => OgmarkupParserT m a
     -> OgmarkupParserT m (List a)
some p = do
  x <- p
  rs <- many p
  pure (Cons x rs)

count :: forall m a
       . Monad m
      => Int
      -> OgmarkupParserT m a
      -> OgmarkupParserT m (List a)
count n p = if n > 0
            then count' n p Nil
            else pure Nil
  where
    count' :: Int
           -> OgmarkupParserT m a
           -> List a
           -> OgmarkupParserT m (List a)
    count' 0 _ r = pure $ reverse r
    count' n p r = do x <- p
                      count' (n-1) p (Cons x r)

whiteSpace :: forall m
            . Monad m
           => OgmarkupParserT m Unit
whiteSpace = skip $ satisfy \c -> c == '\n' || c == ' ' || c == '\t' || c == '\r'
