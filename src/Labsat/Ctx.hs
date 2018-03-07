{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Labsat.Ctx where

import Data.Conduit.Network (AppData, clientSettings, runGeneralTCPClient)
import Preamble

data LabsatCtx = LabsatCtx
  { _lsStatsCtx  :: StatsCtx
  -- ^ Parent context.
  , _lsAppData   :: AppData
  -- ^ AppData for runGeneralTCPClient.
  , _lsDelay     :: Maybe Int
  -- ^ Command delay (seconds).
  }

$(makeClassyConstraints ''LabsatCtx [''HasStatsCtx])

instance HasStatsCtx LabsatCtx where
  statsCtx = lsStatsCtx

instance HasCtx LabsatCtx where
  ctx = statsCtx . ctx

type MonadLabsatCtx c m =
  ( MonadStatsCtx c m
  , HasLabsatCtx c
  )

runLabsatCtx :: MonadStatsCtx c m => ByteString -> Int -> Maybe Int -> TransT LabsatCtx m a -> m a
runLabsatCtx host port timeout action =
  runGeneralTCPClient (clientSettings port host) $ \ad -> do
    e <- view statsCtx
    runTransT (LabsatCtx e ad timeout) action

