{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.14
-- |
-- Module      :  Unused.TastyQuickCheck
-- Copyright   :  2008--2021 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Unused extensions\/helpers for "Test.Tasty.QuickCheck".  This
-- will eventually be removed from the repo; I'm just leaving it
-- here for now, for lack of somewhere better to shelve it.
----------------------------------------------------------------
module Unused.TastyQuickCheck (localQuickCheckOptions) where

import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.QuickCheck  as TastyQC
import qualified Test.QuickCheck        as QC

----------------------------------------------------------------
-- QuickCheck >=2.1.0 && <2.5.0 used 'maxDiscard' instead, which
-- has a different semantics and which we set to @max 1000 (10*n)@.
-- But since the cabal file lists QuickCheck-2.10 as the minimum
-- version, must switch to the new 'maxDiscardRatio' instead.

-- | Convert most of 'QC.Args' into Tasty.  There are a few QuickCheck
-- args which are not handled:
--
--  * 'QC.maxShrinks' if tasty-quickcheck<0.10.2, because
--    'TastyQC.QuickCheckMaxShrinks' is not exported.
--    <https://github.com/UnkindPartition/tasty/issues/316>
--  * 'QC.chatty' because Tasty always ignores this setting.
--  * 'QC.replay' because of technical difficulty with inverting
--    @Test.QuickCheck.Random.mkQCGen :: Int -> QCGen@
--
-- Conversely, there are two TastyQC options which have no equivalent
-- in 'QC.Args': 'TastyQC.QuickCheckVerbose' and 'TastyQC.QuickCheckShowReplay'.
localQuickCheckOptions :: QC.Args -> Tasty.TestTree -> Tasty.TestTree
localQuickCheckOptions args
    = Tasty.localOption (TastyQC.QuickCheckTests      $ QC.maxSuccess      args)
    . Tasty.localOption (TastyQC.QuickCheckMaxSize    $ QC.maxSize         args)
    . Tasty.localOption (TastyQC.QuickCheckMaxRatio   $ QC.maxDiscardRatio args)
#if MIN_VERSION_tasty_quickcheck(0,10,2)
    . Tasty.localOption (TastyQC.QuickCheckMaxShrinks $ QC.maxShrinks      args)
#endif
{-
Tasty lacks some options that QC.Args has:
    * (QC.chatty Bool{default=True}), though 'TastyQC.optionSetToArgs'
      always sets this to False.
Tasty has some additional options that QC.Args lacks:
    * (TastyQC.QuickCheckVerbose Bool{default=False})
      chooses between QC.verboseCheckWithResult vs QC.quickCheckWithResult.
      Where,
        QC.verboseCheckWithResult a p = QC.quickCheckWithResult a (QC.verbose p)
    * (TastyQC.QuickCheckShowReplay Bool{default=False})
      says whether to print the replay seed even on successful tests
      (it's always printed on unsuccessful tests).
Tasty has some discrepancy with QC.Args:
    * (TastyQC.QuickCheckReplay (Maybe Int){default=Nothing})
      vs (QC.replay (Maybe (QCGen, Int)){default=Nothing})

      The Int of QC.replay is the value returned by QC.getSize,
      which can be ignored for the purposes of TastyQC.QuickCheckReplay
      since QC.verboseCheckWithResult doesn't use it for random
      seed stuff. (QC.verboseCheckWithResult does use it to define
      the case for QC.State.computeSize applied to (0,0) however.)

      However, there's no good way I can think to invert
        Test.QuickCheck.Random.mkQCGen :: Int -> QCGen
      Which is just a wrapper around
        System.Random.mkStdGen :: Int -> StdGen
      or
        System.Random.SplitMix.mkSMGen :: Word64 -> SMGen
      depending on, if impl(hugs): cpp-options: -DNO_SPLITMIX

      Partly because it depends on whether the @random@ library
      version is >=1.2 vs <1.2, since they use different internals
      which the QCGen type is expressly designed to paper over.
      But mainly because there is no inverse function already given.
      We could use the Read and Show instances to recover the components
      of the QCGen, however it's less clear how to put them back
      together into an Int.
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
