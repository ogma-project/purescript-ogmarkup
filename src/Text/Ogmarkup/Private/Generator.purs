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
import Control.Monad.Aff                (Aff, later)
import Control.Monad.Aff.Class
import Control.Monad.Trans              (lift)
import Data.Monoid                      (class Monoid, append, mempty)
import Data.Maybe                       (Maybe(..))
import Control.Monad.State              (StateT, runStateT, execStateT, get, put)
import Control.Monad.Reader             (ReaderT, runReaderT, ask)
import Data.List                        (List(..))
import Data.Tuple                       (Tuple(..))

import Text.Ogmarkup.Private.Ast        as Ast
import Text.Ogmarkup.Private.Config     (GenConf(..), Template)
import Text.Ogmarkup.Private.Typography (afterAtom, beforeAtom, normalizeAtom)

-- * The 'Generator' Monad

-- | The 'Generator' Monad is eventually used to generate an output from a
--   given 'Ast.Document. Internally, it keeps track of the previous processed
--   'Ast.Atom' in order to deal with atom separation.
data GenState a = GS { string :: a
                     , prev   :: Maybe Ast.Atom
                     }

initState :: forall a
           . Monoid a
          => GenState a
initState = GS { string: mempty
               , prev:   Nothing
               }

type Generator eff a x = StateT (GenState a) (ReaderT (GenConf a) (Aff eff)) x

-- | Run a 'Generator' monad and get the generated output. The output
--   type has to implement the class 'Monoid' because the 'Generator' monad
--   uses the 'mempty' constant as the initial state of the output and then
--   uses 'mappend' to expand the result as it processes the generation.
runGenerator :: forall eff a x
              . Monoid a
             => Generator eff a x    -- ^ The 'Generator' to run
             -> GenConf a        -- ^ The configuration to use during the generation
             -> Aff eff a        -- ^ The output
runGenerator gen conf = do GS st <- runReaderT (execStateT gen initState) conf
                           pure st.string

later' :: forall eff a x. Generator eff a x -> Generator eff a x
later' gen = do st   :: GenState a <- get
                conf :: GenConf a <- ask

                Tuple res st' <- liftAff <<< later $ runReaderT (runStateT gen st) conf
                put st'
                pure res

-- * Low-level 'Generator's

-- | Apply a template to the result of a given 'Generator' before appending it
--   to the previously generated output.
apply :: forall eff a u
       . Monoid a
      => Template a        -- ^ The 'Template' to apply
      -> Generator eff a u       -- ^ The 'Generator' to run
      -> Generator eff a Unit
apply temp gen = do
  GS st :: GenState a <- get
  put $ GS $ st { string = mempty :: a }

  gen

  GS st' :: GenState a <- get
  put $ GS $ st' { string = st.string `append` (temp st'.string) }
                 { prev   = st'.prev }

-- | Forget about the past and consider the next 'Ast.Atom' as the
--   first to be processed.
reset :: forall eff a
       . Generator eff a Unit
reset = do
  GS st <- get :: Generator eff a (GenState a)
  put $ GS $ st { prev = Nothing }

-- | Append a new sub-output to the generated output.
raw :: forall eff a
     . Monoid a
    => a           -- ^ A sub-output to append
    -> Generator eff a Unit
raw str' = do
  GS st <- get
  let st' = GS $ st { string = st.string `append` str' }
  put st'

-- * AST Processing 'Generator's

-- | Process an 'Ast.Atom' and deal with the space to use to separate it from
--   the paramter of the previous call (that is the last processed
--   'Ast.Atom').
atom :: forall eff a
      . Monoid a
     => Ast.Atom
     -> Generator eff a Unit
atom text = do
  GS st <- get   :: Generator eff a (GenState a)
  GC conf <- ask :: Generator eff a (GenConf a)

  case st.prev of
    Just prev ->
      let spc =  (conf.printSpace $ max (afterAtom conf.typography prev) (beforeAtom conf.typography text))
          str' = spc `append` normalizeAtom conf.typography text
      in put $ GS $ st { string = st.string `append` str' } { prev = Just text }
    Nothing -> put $ GS $ st { string = st.string `append` normalizeAtom conf.typography text }
                             { prev = Just text }

-- | Call 'atom' if the parameter is not 'Nothing'. Otherwise, do nothing.
maybeAtom :: forall eff a
           . Monoid a
          => Maybe Ast.Atom
          -> Generator eff a Unit
maybeAtom (Just text) = atom text
maybeAtom Nothing = pure unit

-- | Process a sequence of 'Ast.Atom'.
atoms :: forall eff a
       . Monoid a
      => List Ast.Atom
      -> Generator eff a Unit
atoms (Cons f rst) = do
  atom f
  atoms rst
atoms Nil = pure unit

-- | Process a 'Ast.Format'.
format :: forall eff a
        . Monoid a
       => Ast.Format
       -> Generator eff a Unit

format (Ast.Raw as) = atoms as

format (Ast.Emph fs) = do
  GC conf :: GenConf a <- ask
  let temp = conf.emphTemplate

  apply temp (formats fs)

format (Ast.StrongEmph fs) = do
  GC conf :: GenConf a <- ask
  let temp = conf.strongEmphTemplate

  apply temp (formats fs)

format (Ast.Quote fs) = do
  atom $ Ast.Punctuation Ast.OpenQuote
  formats fs
  atom $ Ast.Punctuation Ast.CloseQuote

-- | Process a sequence of 'Ast.Format'.
formats :: forall eff a
         . Monoid a
        => List Ast.Format
        -> Generator eff a Unit
formats (Cons f rst) = later' $ do
  format f
  formats rst
formats Nil = pure unit

-- | Process a 'Ast.Reply'.
reply :: forall eff a
       . Monoid a
      => Maybe Ast.Atom
      -> Maybe Ast.Atom
      -> Ast.Reply
      -> Generator eff a Unit
reply begin end (Ast.Simple d) = do
  GC conf :: GenConf a <- ask
  let temp = conf.replyTemplate

  maybeAtom begin
  apply temp (formats d)
  maybeAtom end
reply begin end (Ast.WithSay d ws d') = do
  GC conf :: GenConf a <- ask
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
component :: forall eff a
           . Monoid a
          => Boolean        -- ^ Was the last component a piece of dialog?
          -> Boolean        -- ^ Will the next component be a piece of dialog?
          -> Ast.Component  -- ^ The current component to process
          -> Generator eff a Unit
component p n (Ast.Dialogue d a) = do
  GC conf :: GenConf a <- ask
  let typo = conf.typography
      auth = conf.authorNormalize
      temp = conf.dialogueTemplate

  let
    open  = typo.openDialogue
    close = typo.closeDialogue

  apply (temp $ auth a) (reply (Ast.Punctuation <$> open p) (Ast.Punctuation <$> close n) d)

component p n (Ast.Thought d a) = do
  GC conf :: GenConf a <- ask
  let auth = conf.authorNormalize
      temp = conf.thoughtTemplate

  apply (temp $ auth a) (reply Nothing Nothing d)
component p n (Ast.Teller fs) = formats fs
component p n (Ast.IllFormed ws) = do
  GC conf :: GenConf a <- ask
  let temp = conf.errorTemplate
      typo = conf.typography
  apply temp (raw $ typo.wrapWord ws)

-- | Process a 'Ast.Paragraph' and deal with sequence of 'Ast.Reply'.
paragraph :: forall eff a
           . Monoid a
          => Ast.Paragraph
          -> Generator eff a Unit
paragraph l@(Cons h r) = do
  GC conf :: GenConf a <- ask
  let temp = conf.paragraphTemplate
      between = conf.betweenDialogue

  apply temp (recGen between false (willBeDialogue l) l)

  where
    isDialogue (Ast.Dialogue _ _) = true
    isDialogue _ = false

    willBeDialogue (Cons h (Cons n r)) = isDialogue n
    willBeDialogue _ = false

    recGen :: a
           -> Boolean
           -> Boolean
           -> List Ast.Component
           -> Generator eff a Unit
    recGen between p n (Cons c rst) = do
      when (p && isDialogue c) $ do raw between
                                    reset
      component p n c
      recGen between (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ Nil = pure unit

paragraph Nil = pure unit

-- | Process a sequence of 'Ast.Paragraph'.
paragraphs :: forall eff a
            . Monoid a
           => List Ast.Paragraph
           -> Generator eff a Unit
paragraphs (Cons h r) = do paragraph h
                           reset
                           paragraphs r
paragraphs Nil = pure unit

-- | Process a 'Ast.Section'.
section :: forall eff a
         . Monoid a
        => Ast.Section
        -> Generator eff a Unit
section (Ast.Story ps) = do GC conf :: GenConf a <- ask
                            let temp = conf.storyTemplate

                            apply temp (paragraphs ps)
section (Ast.Aside cls ps) = do GC conf :: GenConf a <- ask
                                let temp = conf.asideTemplate
                                apply (temp cls) (paragraphs ps)

section (Ast.Failing f) = do
    GC conf :: GenConf a <- ask
    let temp = conf.errorTemplate
        temp2 = conf.storyTemplate
        typo = conf.typography
    apply (temp2 <<< temp) (raw $ typo.wrapWord f)

-- | Process a sequence of 'Ast.Section'.
sections :: forall eff a
          . Monoid a
         => List Ast.Section
         -> Generator eff a Unit
sections (Cons s r) = do section s
                         sections r
sections Nil = pure unit

-- | Process a 'Ast.Document', that is a complete Ogmarkup document.
document :: forall eff a
          . Monoid a
         => Ast.Document
         -> Generator eff a Unit
document d = do GC conf :: GenConf a <- ask
                let temp = conf.documentTemplate

                apply temp (sections d)
