{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.12.14
-- |
-- Module      :  Unused.CriterionExts
-- Copyright   :  2008--2021 wren romano
-- License     :  BSD-3-Clause
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Unused extensions\/helpers for "Criterion".  This will eventually
-- be removed from the repo; I'm just leaving it here for now, for
-- lack of somewhere better to shelve it.
----------------------------------------------------------------

module Unused.CriterionExts (generatePerBatch) where

import           Control.DeepSeq                      (NFData(rnf))
import qualified Test.QuickCheck                      as QC
import qualified Criterion.Main                       as C
import qualified Criterion.Measurement.Types          as C (Benchmarkable(..))
import qualified Criterion.Measurement.Types.Internal as C (nf')
-- N.B., the @gauge@ clone of @criterion@ doesn't export @nf'@.
----------------------------------------------------------------

-- | Generate a new argument for each batch.
--
-- Unfortunately, this is only good for when the generated type has
-- roughly constant size (like the basic numeric types), because
-- this can only be used for a single benchmark rather than for a
-- collection of benchmarks like 'C.env' can.  This is an inherent
-- limitation, because the \"batch\" notion only exists at the
-- 'C.Benchmarkable' level, not at the 'C.Benchmark' level.
generatePerBatch
    :: (NFData a, NFData b) => QC.Gen a -> (a -> b) -> C.Benchmarkable
generatePerBatch gen f =
    C.Benchmarkable
        (\_ -> QC.generate gen)
        (\_ _ -> return ())
        (C.nf' rnf f)
        False

----------------------------------------------------------------
----------------------------------------------------------- fin.
