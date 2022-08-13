{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Metrics.Metrics
    ( -- * The Type Class
    MHandle (..) 
    ) where


import           System.Metrics.Counter         as C
import           System.Metrics.Distribution    as Distribution
import           System.Metrics.Gauge           as Gauge
import           System.Metrics.Label           as Label


data MHandle = MHandle
  { hSingleUserC :: C.Counter,
    hAllUsersC   :: C.Counter,
    hCreateUserC :: C.Counter,
    hLoginC      :: C.Counter
  }
