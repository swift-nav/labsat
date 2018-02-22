{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Labsat.Ctx where

import Data.Conduit.Network (AppData)
import Preamble

data TcpCtx = TcpCtx
  { _tcpStatsCtx :: StatsCtx
  , _tcpAppData :: AppData
  }

$(makeClassyConstraints ''TcpCtx [''HasStatsCtx])

instance HasStatsCtx TcpCtx where
  statsCtx = tcpStatsCtx

instance HasCtx TcpCtx where
  ctx = statsCtx . ctx

type MonadTcpCtx c m =
  ( MonadStatsCtx c m
  , HasTcpCtx c
  )

runTcpCtx :: MonadStatsCtx c m => AppData -> TransT TcpCtx m a -> m a
runTcpCtx ad action = do
  e <- view statsCtx
  runTransT (TcpCtx e ad) action

