{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Text.Regex.TDFA.Pipes
( Regex
, CompOption
, ExecOption
, compile
, execute
, regexec
, ProjectChar(..)
) where

import Prelude
  ( Monad(..)
  , Functor(..)
  , String
  , Either(..)
  , (.)
  , (++)
  , Maybe(..)
  , show
  , Char
  , fst
  , tail
  , map
  , id
  , either
  )
--

import Text.Regex.TDFA.NewDFA.Uncons
  ( Uncons(..)
  )
--

import Text.Regex.TDFA.NewDFA.Engine
  ( execMatch
  )
--

import Text.Regex.Base.Impl
  ( polymatch
  , polymatchM
  )
--

import Text.Regex.TDFA.Common
 ( CompOption
 , ExecOption
 )
--

import Text.Regex.TDFA.TDFA
 ( patternToRegex
 )
--

import Text.Regex.Base
 ( RegexLike(..)
 , RegexOptions
 , Extract(..)
 , RegexContext(..)
 , MatchArray
 , matchOnce
 )
--

import Text.Regex.TDFA
 ( Regex
 )
--

import Text.Regex.TDFA.ReadRegex
 ( parseRegex
 )
--

import Pipes
 ( (<-<)
 )
--

import Pipes as Pipe
 ( next
 )
--

import qualified Pipes.Prelude as Pipe
 ( take
 , drop
 , toListM
 )
--

import Pipes.Core
 ( Producer
 )
--

import Control.Lens
 ( (^?)
 , over
 , _Just
 , _1
 , _Right
 , each
 , firstOf
 )
--

import Data.Foldable
 ( Foldable
 , asum
 )
--

import Data.Array.IArray
 ( (!)
 , elems
 , amap
 )
--

import Control.Monad.Identity
 ( Identity(..)
 )
--

------------------------------------------------------------------------
-- ProjectChar
------------------------------------------------------------------------

-- | Types which can have a Char projected from them.
--
class ProjectChar a where
  -- | Project a Char from the type.
  --
  projectChar :: a -> Char
--

instance ProjectChar Char where
  projectChar = id
--

instance (ProjectChar a, ProjectChar b) =>
         ProjectChar (Either a b)
  where
  projectChar = either projectChar projectChar
--

instance (ProjectChar a) => ProjectChar (Identity a) where
  projectChar = projectChar . runIdentity
--

instance (ProjectChar a, Monad m, Foldable m, Functor m) =>
         Uncons (Producer a m r)
  where
  uncons = over (_Just . _1) projectChar
           . asum
           . fmap (^? _Right)
           . Pipe.next
--

------------------------------------------------------------------------
-- Adapting Pipes to TDFA
------------------------------------------------------------------------

instance (Monad m) => Extract (Producer a m ()) where
  before = (<-<) . Pipe.take
  after = (<-<) . Pipe.drop
  empty = return ()
  extract (off, len) = before len . after off
--

instance (ProjectChar a, Monad m, Foldable m, Functor m) =>
         RegexLike Regex (Producer a m ())
  where
  matchOnce r p = firstOf each (matchAll r p)
  matchAll r p = execMatch r 0 '\n' p
--

instance (ProjectChar a, Monad m, Foldable m, Functor m) =>
         RegexContext Regex (Producer a m ()) (Producer a m ())
  where
  match = polymatch
  matchM = polymatchM
--

------------------------------------------------------------------------
-- Exported API
------------------------------------------------------------------------

-- | Takes the output of a Producer to form a regular expression. This
--   expression is then compiled using the options provided. If
--   compilation fails an error message is returned.
--
--   If your regular expression is expressed as a String literal or
--   some other "Data.Foldable" then "Pipes.each" is useful.
--
compile :: (ProjectChar a, Monad m, Foldable m, Functor m) =>
           CompOption
           -> ExecOption
           -> Producer a m ()
           -> Either String Regex
compile compOpt execOpt regexp =
  case parseRegex (map projectChar (asum (Pipe.toListM regexp))) of
    Left  err -> Left ( "parseRegex for Text.Regex.TDFA.Pipes failed: "
                        ++ show err
                      )
    Right pat -> Right (patternToRegex pat compOpt execOpt)
--

-- | Execute a compiled regular expression on the output of a Producer.
--
execute :: (ProjectChar a, Monad m, Foldable m, Functor m) =>
           Regex -> Producer a m () -> Either a (Maybe MatchArray)
execute r p = Right (matchOnce r p)


regexec :: (ProjectChar a, Monad m, Foldable m, Functor m) =>
           Regex
           -> Producer a m ()
           -> Either String ( Maybe ( Producer a m ()
                                    , Producer a m ()
                                    , Producer a m ()
                                    , [Producer a m ()]
                                    )
                            )
regexec r bs =
  case matchOnceText r bs of
    Nothing -> Right Nothing
    Just (pre,mt,post) ->
      let main = fst (mt!0)
          rest = map fst (tail (elems mt)) -- will be []
      in Right (Just (pre,main,post,rest))
--




