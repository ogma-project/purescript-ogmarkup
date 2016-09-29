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
import Data.Monoid (append)
import Data.Maybe          (Maybe)
import Data.List           (many, some) as L
import Data.Array          (many, some, fromFoldable) as A
import Data.List           (List(..), reverse)
import Data.String         (fromCharArray, toCharArray)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser (ParserT, fail, ParseError, runParserT, PState(..))
import Text.Parsing.Parser.Combinators (optional, optionMaybe, notFollowedBy, manyTill, many1Till, try, lookAhead)
import Text.Parsing.Parser.String (eof, string, anyChar, char, skipSpaces, oneOf, satisfy)
import Control.Monad.State (State, get, put, lift, evalState)
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
type OgmarkupParser a = ParserT String (State ParserState) a

sget :: OgmarkupParser ParserState
sget = lift get

sput :: ParserState
     -> OgmarkupParser Unit
sput t = lift $ put t

-- | Update the 'ParserState' to guard against nested emphasis.
enterEmph :: OgmarkupParser Unit
enterEmph = do st <- sget
               if st.parseWithEmph
                 then fail "guard against nested emphasis"
                 else do sput $ st { parseWithEmph = true }
                         pure unit

-- | Update the 'ParserState' to be able to parse insput with emphasis
-- again.
leaveEmph :: OgmarkupParser Unit
leaveEmph = do st <- sget
               if st.parseWithEmph
                 then do sput st { parseWithEmph = false }
                         pure unit
                 else fail "cannot leave emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested strong emphasis.
enterStrongEmph :: OgmarkupParser Unit
enterStrongEmph = do st <- sget
                     if st.parseWithStrongEmph
                       then fail "guard against nested strong emphasis"
                       else do sput st { parseWithStrongEmph = true }
                               pure unit

-- | Update the 'ParserState' to be able to parse insput with strong emphasis
-- again.
leaveStrongEmph :: OgmarkupParser Unit
leaveStrongEmph = do st <- sget
                     if st.parseWithStrongEmph
                       then do sput st { parseWithStrongEmph = false }
                               pure unit
                       else fail "cannot leave strong emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested quoted insputs.
enterQuote :: OgmarkupParser Unit
enterQuote = do st <- sget
                if st.parseWithinQuote
                  then fail "guard against nested quotes"
                  else do sput st { parseWithinQuote = true }
                          pure unit

-- | Update the 'ParserState' to be able to parse an insput
-- surrounded by quotes again.
leaveQuote :: OgmarkupParser Unit
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
parse :: forall a
       . OgmarkupParser a
      -> String
      -> Either ParseError a
parse ogma content = evalState (runParserT (PState { input: content, position: initialPos }) ogma) initParserState

-- | Try its best to parse an ogmarkup document. When it encounters an
--   error, it pures an Ast and the remaining input.
--
--   See 'Ast.Document'.
document :: OgmarkupParser Ast.Document
document = doc Nil
  where doc :: Ast.Document
             -> OgmarkupParser Ast.Document
        doc ast = do skipSpaces
                     sects <- many (try section)
                     let ast' = ast `append` sects
                     (eof *> pure ast') <|> (recover ast' `bind` doc)

        recover :: Ast.Document
                -> OgmarkupParser Ast.Document
        recover ast = do failure <- A.fromFoldable <$> many1Till anyChar (char '\n')
                         pure $ ast `append` (Cons (Ast.Failing $ fromCharArray failure) Nil)
-- | See 'Ast.Section'.
section :: OgmarkupParser Ast.Section
section = aside <|> story

-- | See 'Ast.Aside'.
aside :: OgmarkupParser Ast.Section
aside = do asideSeparator
           cls <- optionMaybe asideClass
           skipSpaces
           ps <- some (paragraph <* skipSpaces)
           asideSeparator
           manyTill skipSpaces (skip (char '\n') <|> eof)
           skipSpaces

           pure $ Ast.Aside cls ps
  where
    asideClass :: OgmarkupParser String
    asideClass = do cls <- some' letterChar
                    asideSeparator

                    pure cls
    letterChar = satisfy (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

-- | See 'Ast.Story'.
story :: OgmarkupParser Ast.Section
story = Ast.Story <$> some (paragraph <* skipSpaces)

-- | See 'Ast.Paragraph'.
paragraph :: OgmarkupParser Ast.Paragraph
paragraph = some component <* blank

-- | See 'Ast.Component'.
component :: OgmarkupParser Ast.Component
component = try (dialogue <|> thought <|> teller) <|> illformed

-- | See 'Ast.IllFormed'.
illformed :: OgmarkupParser Ast.Component
illformed = Ast.IllFormed <$> restOfParagraph

-- | Parse the rest of the current paragraph with no regards for the
-- ogmarkup syntax. This Parser is used when the document is ill-formed, to
-- find a new point of synchronization.
restOfParagraph :: OgmarkupParser String
restOfParagraph = do lookAhead anyChar
                     notFollowedBy endOfParagraph
                     str <- A.fromFoldable <$> manyTill anyChar (lookAhead $ try endOfParagraph)
                     pure $ fromCharArray str

-- | See 'Ast.Teller'.
teller :: OgmarkupParser Ast.Component
teller = Ast.Teller <$> some format

-- | See 'Ast.Dialogue'.
dialogue :: OgmarkupParser Ast.Component
dialogue = talk '[' ']' Ast.Dialogue

-- | See 'Ast.Thought'.
thought :: OgmarkupParser Ast.Component
thought = talk '<' '>' Ast.Thought

-- | @'talk' c c' constr@ wraps a reply surrounded by @c@ and @c'@ inside
--   @constr@ (either 'Ast.Dialogue' or 'Ast.Thought').
talk :: Char -- ^ A character to mark the begining of a reply
     -> Char -- ^ A character to mark the end of a reply
     -> (Ast.Reply -> Maybe String -> Ast.Component) -- ^ Either 'Ast.Dialogue' or 'Ast.Thought' according to the situation
     -> OgmarkupParser Ast.Component
talk c c' constructor = do
  rep <- reply c c'
  auth <- optionMaybe characterName
  blank

  pure $ constructor rep auth

-- | Parse the name of the character which speaks or thinks. According to
-- the ogmarkup syntax, it is surrounded by parentheses.
characterName :: OgmarkupParser String
characterName = do
  char '('
  notFollowedBy (char ')')
  auth <- A.fromFoldable <$> manyTill anyChar (char ')')

  pure $ fromCharArray auth

-- | 'reply' parses a 'Ast.Reply'.
reply ::Char
      -> Char
      -> OgmarkupParser Ast.Reply
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
format :: OgmarkupParser Ast.Format
format = raw <|> emph <|> strongEmph <|> quote
  where
    emph :: OgmarkupParser Ast.Format
    emph = do char '*'
              blank
              enterEmph
              f <- format
              fs <- manyTill format (char '*' *> blank)
              leaveEmph
              pure $ Ast.Emph (Cons f fs)

    strongEmph :: OgmarkupParser Ast.Format
    strongEmph = do char '+'
                    blank
                    enterStrongEmph
                    f <- format
                    fs <- manyTill format (char '+' *> blank)
                    leaveStrongEmph
                    pure $ Ast.StrongEmph (Cons f fs)

    quote :: OgmarkupParser Ast.Format
    quote = do openQuote
               enterQuote
               f <- format
               fs <- manyTill format closeQuote
               leaveQuote
               pure $ Ast.Quote (Cons f fs)

-- | See 'Ast.Raw'.
raw :: OgmarkupParser Ast.Format
raw = Ast.Raw <$> some atom


-- | See 'Ast.Atom'.
atom :: OgmarkupParser Ast.Atom
atom = (word <|> mark <|> longword) <* blank

-- | See 'Ast.Word'. This parser does not consume the following skipSpacess, so
--   the caller needs to take care of it.
word :: OgmarkupParser Ast.Atom
word = do notFollowedBy endOfWord
          str <- A.fromFoldable <$> many1Till anyChar (lookAhead endOfWord)
          pure $ Ast.Word (fromCharArray str)
  where
    endOfWord :: OgmarkupParser Unit
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
longword :: OgmarkupParser Ast.Atom
longword = do char '`'
              notFollowedBy (char '`')
              str <- A.fromFoldable <$> manyTill anyChar (char '`')
              pure $ Ast.Word (fromCharArray str)

-- | See 'Ast.Punctuation'. Be aware that 'mark' does not parse the quotes
--   because they are processed 'quote'.
mark :: OgmarkupParser Ast.Atom
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
               . OgmarkupParser t
              -> Ast.Mark
              -> OgmarkupParser Ast.Mark
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
openQuote :: OgmarkupParser Unit
openQuote = do char '«' <|> char '"'
               blank

-- | See 'Ast.CloseQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
closeQuote :: OgmarkupParser Unit
closeQuote = do char '»' <|> char '"'
                blank

-- | An aside section (see 'Ast.Aside') is a particular region
--   surrounded by two lines of underscores (at least three).
--   This parser consumes one such line.
asideSeparator :: OgmarkupParser Unit
asideSeparator = do string "__"
                    some (char '_')
                    pure unit


-- | The end of a paragraph is the end of the document or two blank lines
-- or an aside separator, that is a line of underscores.
endOfParagraph :: OgmarkupParser Unit
endOfParagraph = try betweenTwoSections
                 <|> asideSeparator
                 <|> eof
  where
    betweenTwoSections :: OgmarkupParser Unit
    betweenTwoSections = do count 2 $ manyTill whiteSpace (eof <|> skip (char '\n'))
                            skipSpaces


-- | This parser consumes all the white spaces until it finds either an aside
--   surrounding marker (see 'Ast.Aside'), the end of the document or
--   one blank line. The latter marks the end of the current paragraph.
blank :: OgmarkupParser Unit
blank = do skip $ optional (notFollowedBy endOfParagraph *> skipSpaces)

-- | @skip p@ parses @p@ and skips the result.
skip :: forall b
      . OgmarkupParser b
     -> OgmarkupParser Unit
skip p = do p
            pure unit

many' :: OgmarkupParser Char
      -> OgmarkupParser String
many' p = fromCharArray <$> A.many p

some' :: OgmarkupParser Char
      -> OgmarkupParser String
some' p = fromCharArray <$> A.some p

many :: forall a
      . OgmarkupParser a
     -> OgmarkupParser (List a)
many p = (do x <- p
             rs <- many p
             pure (Cons x rs)) <|>
         (pure Nil)

some :: forall a
      . OgmarkupParser a
     -> OgmarkupParser (List a)
some p = do
  x <- p
  rs <- many p
  pure (Cons x rs)

count :: forall a
       . Int
      -> OgmarkupParser a
      -> OgmarkupParser (List a)
count n p = if n > 0
            then count' n p Nil
            else pure Nil
  where
    count' :: Int
           -> OgmarkupParser a
           -> List a
           -> OgmarkupParser (List a)
    count' 0 _ r = pure $ reverse r
    count' n p r = do x <- p
                      count' (n-1) p (Cons x r)

whiteSpace :: OgmarkupParser Unit
whiteSpace = skip $ satisfy \c -> c == '\n' || c == ' ' || c == '\t' || c == '\r'
