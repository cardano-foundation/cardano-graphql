{-# LANGUAGE DoAndIfThenElse #-}

{-|
Module      :  Cardano.Prelude.GHC.Heap.NormalForm
Copyright   :  (c) 2013 Joachim Breitner
License     :  BSD3

This code has been adapted from the module "GHC.AssertNF" of the package
<http://hackage.haskell.org/package/ghc-heap-view ghc-heap-view>
(<https://github.com/nomeata/ghc-heap-view GitHub>) authored by
Joachim Breitner.

To avoid space leaks and unwanted evaluation behaviour, the programmer might want his data to be fully evaluated at certain positions in the code. This can be enforced, for example, by ample use of "Control.DeepSeq", but this comes at a cost.

Experienced users hence use 'Control.DeepSeq.deepseq' only to find out about the existence of space leaks and optimize their code to not create the thunks in the first place, until the code no longer shows better performance with 'deepseq'.

This module provides an alternative approach: An explicit assertion about the evaluation state. If the programmer expect a certain value to be fully evaluated at a specific point of the program (e.g. before a call to 'writeIORef'), he can state that, and as long as assertions are enabled, this statement will be checked. In the production code the assertions can be disabled, to avoid the run-time cost.

Copyright (c) 2012-2013, Joachim Breitner

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Joachim Breitner nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Cardano.Prelude.GHC.Heap.NormalForm
  ( isHeadNormalForm
  , isNormalForm
  )
where

import Cardano.Prelude.Base

import GHC.Exts.Heap

-- Everything is in normal form, unless it is a
-- thunk explicitly marked as such.
-- Indirection are also considered to be in HNF
isHeadNormalForm :: Closure -> IO Bool
isHeadNormalForm c = do
  case c of
    ThunkClosure{}    -> return False
    APClosure{}       -> return False
    SelectorClosure{} -> return False
    BCOClosure{}      -> return False
    _                 -> return True

-- | The function 'isNormalForm' checks whether its argument is fully evaluated
-- and deeply evaluated.
--
-- NOTE 1: If you want to override the behaviour of 'isNormalForm' for specific
-- types (in particular, for specific types that may be /nested/ somewhere
-- inside the @a@), consider using
-- 'Cardano.Prelude.GHC.Heap.NormalForm.Classy.noUnexpectedThunks' instead.
--
-- NOTE 2: The normal form check can be quite brittle, especially with @-O0@.
-- For example, writing something like
--
-- > let !x = ...
-- > nf <- isNormalForm x
--
-- might translate to
--
-- > nf <- isNormalForm (case ... of x -> x)
--
-- which would trivially be @False@. In general, 'isNormalForm' should probably
-- only be used with @-O1@, but even then the answer may still depend on
-- internal decisions made by ghc during compilation.
isNormalForm :: a -> IO Bool
isNormalForm x = isNormalFormBoxed (asBox x)

isNormalFormBoxed :: Box -> IO Bool
isNormalFormBoxed b = do
  c  <- getBoxedClosureData b
  nf <- isHeadNormalForm c
  if nf
    then do
      c' <- getBoxedClosureData b
      allM isNormalFormBoxed (allClosures c')
    else do
      return False

-- From Control.Monad.Loops in monad-loops, but I'd like to avoid too many
-- trivial dependencies
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x : xs) = do
  q <- p x
  if q then allM p xs else return False
