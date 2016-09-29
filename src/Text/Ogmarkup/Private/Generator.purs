{-|
Module      : Text.Ogmarkup.Private.Generator
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

The generation of the output from an 'Ast.Ast' is carried out by the 'Generator'
Monad.

-}

module Text.Ogmarkup.Private.Generator where

import Prelude                          (($), bind, Unit, pure, unit, max, (<$>), (&&), (<<<))

import Control.Monad                    (when)
import Data.Monoid                      (append)
import Data.Maybe                       (Maybe(..))
import Control.Monad.State              (StateT, execStateT, get, put)
import Control.Monad.Reader             (Reader, runReader, ask)
import Data.List                        (List(..))

import Text.Ogmarkup.Private.Ast        as Ast
import Text.Ogmarkup.Private.Config     (GenConf, Template)
import Text.Ogmarkup.Private.Typography (afterAtom, beforeAtom, normalizeAtom)

-- * The 'Generator' Monad

-- | The 'Generator' Monad is eventually used to generate an output from a
--   given 'Ast.Document. Internally, it keeps track of the previous processed
--   'Ast.Atom' in order to deal with atom separation.
type GenState = { string :: String
                , prev   :: Maybe Ast.Atom
                }

initState :: GenState
initState = { string: ""
            , prev:   Nothing
            }

type Generator a = StateT GenState (Reader GenConf) a

-- | Run a 'Generator' monad and get the generated output. The output
--   type has to implement the class 'Monoid' because the 'Generator' monad
--   uses the 'mempty' constant as the initial state of the output and then
--   uses 'mappend' to expand the result as it processes the generation.
runGenerator :: forall a
              . Generator a -- ^ The 'Generator' to run
             -> GenConf          -- ^ The configuration to use during the generation
             -> String           -- ^ The output
runGenerator gen conf = (runReader (execStateT gen initState) conf).string

-- * Low-level 'Generator's

-- | Apply a template to the result of a given 'Generator' before appending it
--   to the previously generated output.
apply :: forall u
       . Template          -- ^ The 'Template' to apply
      -> Generator u       -- ^ The 'Generator' to run
      -> Generator Unit
apply temp gen = do
  st <- get :: Generator GenState
  put $ st { string = "" }
  gen
  st' <- get :: Generator GenState
  put $ st' { string = st.string `append` (temp st'.string) }
            { prev   = st'.prev }

-- | Forget about the past and consider the next 'Ast.Atom' as the
--   first to be processed.
reset :: Generator Unit
reset = do
  st <- get :: Generator GenState
  put $ st { prev = (Nothing :: Maybe Ast.Atom) }

-- | Append a new sub-output to the generated output.
raw :: String           -- ^ A sub-output to append
    -> Generator Unit
raw str' = do
  st <- get :: Generator GenState
  let st' = st { string = st.string `append` str' } :: GenState
  put st'

-- * AST Processing 'Generator's

-- | Process an 'Ast.Atom' and deal with the space to use to separate it from
--   the paramter of the previous call (that is the last processed
--   'Ast.Atom').
atom :: Ast.Atom
     -> Generator Unit
atom text = do
  st <- get :: Generator GenState
  conf <- ask :: Generator GenConf

  case st.prev of
    Just prev ->
      let spc =  (conf.printSpace $ max (afterAtom conf.typography prev) (beforeAtom conf.typography text))
          str' = spc `append` normalizeAtom conf.typography text
      in put $ st { string = st.string `append` str' } { prev = Just text }
    Nothing -> put $ st { string = st.string `append` normalizeAtom conf.typography text }
                        { prev = Just text }

-- | Call 'atom' if the parameter is not 'Nothing'. Otherwise, do nothing.
maybeAtom :: Maybe Ast.Atom
          -> Generator Unit
maybeAtom (Just text) = atom text
maybeAtom Nothing = pure unit

-- | Process a sequence of 'Ast.Atom'.
atoms :: List Ast.Atom
      -> Generator Unit
atoms (Cons f rst) = do
  atom f
  atoms rst
atoms Nil = pure unit

-- | Process a 'Ast.Format'.
format :: Ast.Format
       -> Generator Unit

format (Ast.Raw as) = atoms as

format (Ast.Emph fs) = do
  conf <- ask :: Generator GenConf
  let temp = conf.emphTemplate

  apply temp (formats fs)

format (Ast.StrongEmph fs) = do
  conf <- ask :: Generator GenConf
  let temp = conf.strongEmphTemplate

  apply temp (formats fs)

format (Ast.Quote fs) = do
  atom $ Ast.Punctuation Ast.OpenQuote
  formats fs
  atom $ Ast.Punctuation Ast.CloseQuote

-- | Process a sequence of 'Ast.Format'.
formats :: List Ast.Format
        -> Generator Unit
formats (Cons f rst) = do
  format f
  formats rst
formats Nil = pure unit

-- | Process a 'Ast.Reply'.
reply :: Maybe Ast.Atom
      -> Maybe Ast.Atom
      -> Ast.Reply
      -> Generator Unit
reply begin end (Ast.Simple d) = do
  conf <- ask :: Generator GenConf
  let temp = conf.replyTemplate

  maybeAtom begin
  apply temp (formats d)
  maybeAtom end
reply begin end (Ast.WithSay d ws d') = do
  conf <- ask :: Generator GenConf
  let temp = conf.replyTemplate

  maybeAtom begin
  apply temp (formats d)

  case d' of Nil -> do
               maybeAtom end
               formats ws
             l   -> do
               formats ws
               apply temp (formats d')
               maybeAtom end

-- | Process a 'Ast.Component'.
component :: Boolean        -- ^ Was the last component a piece of dialog?
          -> Boolean        -- ^ Will the next component be a piece of dialog?
          -> Ast.Component  -- ^ The current component to process
          -> Generator Unit
component p n (Ast.Dialogue d a) = do
  conf <- ask :: Generator GenConf
  let typo = conf.typography
      auth = conf.authorNormalize
      temp = conf.dialogueTemplate

  let
    open  = typo.openDialogue
    close = typo.closeDialogue

  apply (temp $ auth a) (reply (Ast.Punctuation <$> open p) (Ast.Punctuation <$> close n) d)

component p n (Ast.Thought d a) = do
  conf <- ask :: Generator GenConf
  let auth = conf.authorNormalize
      temp = conf.thoughtTemplate

  apply (temp $ auth a) (reply Nothing Nothing d)
component p n (Ast.Teller fs) = formats fs
component p n (Ast.IllFormed ws) = do
  conf <- ask :: Generator GenConf
  let temp = conf.errorTemplate
  apply temp (raw ws)

-- | Process a 'Ast.Paragraph' and deal with sequence of 'Ast.Reply'.
paragraph :: Ast.Paragraph
          -> Generator Unit
paragraph l@(Cons h r) = do
  conf <- ask :: Generator GenConf
  let temp = conf.paragraphTemplate
      between = conf.betweenDialogue

  apply temp (recGen between false (willBeDialogue l) l)

  where
    isDialogue (Ast.Dialogue _ _) = true
    isDialogue _ = false

    willBeDialogue (Cons h (Cons n r)) = isDialogue n
    willBeDialogue _ = false

    recGen :: String
           -> Boolean
           -> Boolean
           -> List Ast.Component
           -> Generator Unit
    recGen between p n (Cons c rst) = do
      when (p && isDialogue c) $ do raw between
                                    reset
      component p n c
      recGen between (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ Nil = pure unit

paragraph Nil = pure unit

-- | Process a sequence of 'Ast.Paragraph'.
paragraphs :: List Ast.Paragraph
           -> Generator Unit
paragraphs (Cons h r) = do paragraph h
                           reset
                           paragraphs r
paragraphs Nil = pure unit

-- | Process a 'Ast.Section'.
section :: Ast.Section
        -> Generator Unit
section (Ast.Story ps) = do conf <- ask :: Generator GenConf
                            let temp = conf.storyTemplate

                            apply temp (paragraphs ps)
section (Ast.Aside cls ps) = do conf <- ask :: Generator GenConf
                                let temp = conf.asideTemplate
                                apply (temp cls) (paragraphs ps)

section (Ast.Failing f) = do
    conf <- ask :: Generator GenConf
    let temp = conf.errorTemplate
        temp2 = conf.storyTemplate
    apply (temp2 <<< temp) (raw f)

-- | Process a sequence of 'Ast.Section'.
sections :: List Ast.Section
         -> Generator Unit
sections (Cons s r) = do section s
                         sections r
sections Nil = pure unit

-- | Process a 'Ast.Document', that is a complete Ogmarkup document.
document :: Ast.Document
         -> Generator Unit
document d = do conf <- ask :: Generator GenConf
                let temp = conf.documentTemplate

                apply temp (sections d)
