{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Labsat where

import           Control.Concurrent.Async.Lifted (race_)
import           Control.Concurrent.Lifted       (threadDelay)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary             as B
import           Data.Conduit.Network
import           Data.Foldable                   (traverse_)
import           Data.Text.Encoding              (encodeUtf8)
import           Labsat.Ctx
import           Labsat.Parser
import           Labsat.Types
import           Preamble                        hiding (find)
import           System.IO                       hiding (print, putStrLn)


--------------------------------------------------------------------------------

-- | Bracketed opening, closing of a binary file.
--
withBinaryFile' :: (MonadIO m, MonadBaseControl IO m) => FilePath -> (Handle -> m a) -> m a
withBinaryFile' f = flip bracket (liftIO . hClose) $ do
    h <- liftIO $ openBinaryFile f AppendMode
    liftIO $ hSetBuffering h LineBuffering
    pure h

-- | Add Labsat end-of-line delimiters and send command
--
sendCmd :: MonadLabsatCtx c m => ByteString -> m ()
sendCmd s = runResourceT $ do
  ad <- view lsAppData
  yield (s <> "\r\r\n") =$= B.conduitFile "labsat.log" $$ appSink ad

-- | Strip ANSI color codes
--
colorStripper :: MonadIO m => Conduit ByteString m ByteString
colorStripper = do
  mx <- await
  case mx of
    Nothing -> pure ()
    Just bs ->
      case BS.findIndex isEscape bs of
        Nothing -> do
          yield bs
          colorStripper
        Just idx -> do
          let (prefix, escape) = BS.splitAt idx bs
          yield prefix
          case parse parseColorSeq escape of
            Fail{} -> do
              let (h,t) = BS.splitAt 1 escape
              yield h
              leftover t
            Partial _ -> leftover escape
            Done i _ -> leftover i
          colorStripper

-- | Receive command response and strip color codes
--
receiveResp :: MonadLabsatCtx c m => Parser a -> m a
receiveResp p = runResourceT $ do
  ad <- view lsAppData
  appSource ad =$= colorStripper =$= B.conduitFile "labsat.log" $$ sinkParser p

-- | Receive command response, strip color codes, and log to file
--
logResp :: MonadLabsatCtx c m => FilePath -> m ()
logResp lf = runResourceT $ do
  ad <- view lsAppData
  withBinaryFile' lf $ \lh ->
    appSource ad =$= colorStripper $$ B.sinkHandle lh

-- | Parse connection message
--
connectMsg :: (MonadLabsatCtx c m) => m ()
connectMsg = receiveResp parseFirstLabsatMsg >> pure ()

-- | Send a command and parser for its response.
--
command :: MonadLabsatCtx c m => ByteString -> Parser a -> m a
command c p = do
  delay <- view lsDelay
  sendCmd c
  traverse_ (threadDelay . (*1000000)) delay
  receiveResp $ parseCommandAck c *> p

-- | Send a command and parse for OK and the prompt
--
okCommand :: (MonadLabsatCtx c m) => ByteString -> m ByteString
okCommand = flip command okPrompt

-- Swallow first message, capture and print second one (debug)
--
debugRecv :: ByteString -> ByteString -> Int -> IO ()
debugRecv msg host port =
  runCtx $ runStatsCtx $
    runLabsatCtx host port Nothing $ do
      msg0 <- receiveResp parseUntilPrompt
      putStrLn "First message:"
      print msg0

      putStrLn "Debug message:"
      sendCmd msg
      res <- receiveResp parseUntilPrompt
      print (res <> "LABSAT_V3 >")

testCommand :: (MonadStatsCtx c m, Show a) => ByteString -> Int -> TransT LabsatCtx m a -> m ()
testCommand host port cmd =
  runLabsatCtx host port Nothing $ do
    void $ receiveResp parseFirstLabsatMsg
    res <- cmd
    print res

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | Optionally create argument string from Maybe a
--
-- | TODO fix this function so output doesn't have escaped quotes from 'show'
--
argFromMaybe :: (Show a) => ByteString -> Maybe a -> ByteString
argFromMaybe a m =
  case m of
    Nothing -> ""
    Just m' -> a ++ showToBs m'

-- Int -> ByteString
--
intToBs :: Int -> ByteString
intToBs = C.pack . show

-- Bool -> ByteString
--
boolToBs :: Bool -> ByteString
boolToBs = bool "N" "Y"

showToBs :: Show a => a -> ByteString
showToBs = C.pack . show

--------------------------------------------------------------------------------
-- Help command
--------------------------------------------------------------------------------

-- | HELP command.
--
help :: MonadLabsatCtx c m => m HelpCommands
help = command "HELP" parseHelp

help_ :: MonadLabsatCtx c m => m ()
help_ = void help

--------------------------------------------------------------------------------
-- Media command
--------------------------------------------------------------------------------

-- | MEDIA:LIST command.
--
mediaList :: MonadLabsatCtx c m => m MediaList
mediaList = command "MEDIA:LIST" parseMediaList

mediaList_ :: MonadLabsatCtx c m => m ()
mediaList_ = void mediaList

-- | MEDIA:CHDIR:\ command.
--
mediaChdirRoot :: MonadLabsatCtx c m => m ByteString
mediaChdirRoot = command "MEDIA:CHDIR:\\" parseMediaChdir

mediaChdirRoot_ :: MonadLabsatCtx c m => m ()
mediaChdirRoot_ = void mediaChdirRoot

-- | MEDIA:CHDIR:.. command.
--
mediaChdirUp :: MonadLabsatCtx c m => m ByteString
mediaChdirUp = command "MEDIA:CHDIR:.." parseMediaChdir

mediaChdirUp_ :: MonadLabsatCtx c m => m ()
mediaChdirUp_ = void mediaChdirUp

-- | MEDIA:CHDIR:<dir> command.
--
mediaChdir :: MonadLabsatCtx c m => ByteString -> m ByteString
mediaChdir d = command ("MEDIA:CHDIR:" <> d) parseMediaChdir

mediaChdir_ :: MonadLabsatCtx c m => ByteString -> m ()
mediaChdir_ = void . mediaChdir

-- | MEDIA:PROTECT:Y/N:FILE command.
--
mediaProtect :: MonadLabsatCtx c m => Bool -> ByteString -> m ByteString
mediaProtect b f = okCommand $ "MEDIA:PROTECT:" <> bool "N:" "Y:" b <> f

mediaProtect_ :: MonadLabsatCtx c m => Bool -> ByteString -> m ()
mediaProtect_ b f = void $ mediaProtect b f

-- | MEDIA:DELETE:FILE command.
--
mediaDelete :: MonadLabsatCtx c m => ByteString -> m ByteString
mediaDelete f = okCommand $ "MEDIA:DELETE:" <> f

mediaDelete_ :: MonadLabsatCtx c m => ByteString -> m ()
mediaDelete_ = void . mediaDelete

-- | MEDIA:SELECT:SD/USB/SATA command.
--
mediaSelect :: MonadLabsatCtx c m => MediaType -> m ByteString
mediaSelect s = okCommand $ "MEDIA:SELECT:" <> showToBs s

mediaSelect_ :: MonadLabsatCtx c m => MediaType -> m ()
mediaSelect_ = void . mediaSelect

--------------------------------------------------------------------------------
-- Play command
--------------------------------------------------------------------------------

-- | PLAY command.
--
play :: MonadLabsatCtx c m => ByteString -> m ByteString
play f = command ("PLAY:FILE:" <> f) (parsePlay f)

play_ :: MonadLabsatCtx c m => ByteString -> m ()
play_ = void . play

-- | PLAY command that supports FOR and FROM
--
play' :: MonadLabsatCtx c m => PlayConf -> m ByteString
play' pc = command cmd (parsePlay file')
  where
    for'  = argFromMaybe ":FOR:"  $ pc ^.pcFor
    from' = argFromMaybe ":FROM:" $ pc ^.pcFrom
    file' = pc ^. pcFile
    cmd = "PLAY:FILE:" <> file' <> for' <> from'

play'_ :: MonadLabsatCtx c m => PlayConf -> m ()
play'_ = void . play'

-- | PLAY:STOP command.
--
playStop :: MonadLabsatCtx c m => m ByteString
playStop = okCommand "PLAY:STOP"

playStop_ :: MonadLabsatCtx c m => m ()
playStop_ = void playStop

-- | PLAY:? command.
--
playStatus :: MonadLabsatCtx c m => m PlayStatus
playStatus = command "PLAY:?" parsePlayStatus

playStatus_ :: MonadLabsatCtx c m => m ()
playStatus_ = void playStatus

--------------------------------------------------------------------------------
-- Type command
--------------------------------------------------------------------------------

-- | TYPE command. Named 'info' to avoid the obvious conflict.
--
info :: MonadLabsatCtx c m => m Info
info = command "TYPE" parseInfo

info_ :: MonadLabsatCtx c m => m ()
info_ = void info

--------------------------------------------------------------------------------
-- Find command
--------------------------------------------------------------------------------

-- | FIND command.
--
find :: MonadLabsatCtx c m => m ByteString
find = okCommand "FIND"

find_ :: MonadLabsatCtx c m => m ()
find_ = void find

--------------------------------------------------------------------------------
-- Mon command
--------------------------------------------------------------------------------

-- | MON:NMEA:ON command.
--
nmeaOn :: MonadLabsatCtx c m => m ByteString
nmeaOn = okCommand "MON:NMEA:ON"

nmeaOn_ :: MonadLabsatCtx c m => m ()
nmeaOn_ = void nmeaOn

-- | MON:NMEA:OFF command.
--
nmeaOff :: MonadLabsatCtx c m => m ByteString
nmeaOff = okCommand "MON:NMEA:OFF"

nmeaOff_ :: MonadLabsatCtx c m => m ()
nmeaOff_ = void nmeaOff

-- | Capture NMEA log for 'n' seconds
--
nmeaLog :: (MonadIO m, MonadLabsatCtx c m) => Int -> FilePath -> m ()
nmeaLog n f = do
  void nmeaOn
  race_ (threadDelay $ n * 1000000) $ logResp f
  void nmeaOff
  pure ()

nmeaLog_ :: (MonadIO m, MonadLabsatCtx c m) => Int -> FilePath -> m ()
nmeaLog_ = nmeaLog

-- | MON:LOC command.
--
monLoc :: MonadLabsatCtx c m => m Location
monLoc = command "MON:LOC" parseMonLoc

monLoc_ :: MonadLabsatCtx c m => m ()
monLoc_ = void monLoc

-- | MON:SAT command.
--
monSat :: MonadLabsatCtx c m => m [ConstellationCNO]
monSat = command "MON:SAT" parseMonSat

monSat_ :: MonadLabsatCtx c m => m ()
monSat_ = void monSat

--------------------------------------------------------------------------------
-- Rec command
--------------------------------------------------------------------------------

-- | REC command.
--
rec :: MonadLabsatCtx c m => m ByteString
rec = command "REC" parseRec

rec_ :: MonadLabsatCtx c m => m ()
rec_ = void rec

-- | REC command that supports FILE and FOR.
--
rec' :: MonadLabsatCtx c m => RecordConf -> m ByteString
rec' rc = command cmd parseRec
  where
    file' = argFromMaybe ":FILE:" $ rc ^. rcFile
    for'  = argFromMaybe ":FOR:"  $ rc ^. rcFor
    cmd = "REC" <> file' <> for'

rec'_ :: MonadLabsatCtx c m => RecordConf -> m ()
rec'_  = void . rec'

-- | REC:STOP command.
--
recStop :: MonadLabsatCtx c m => m ByteString
recStop = okCommand "REC:STOP"

recStop_ :: MonadLabsatCtx c m => m ()
recStop_ = void recStop

-- | REC:? command.
--
recStatus :: MonadLabsatCtx c m => m RecordStatus
recStatus = command "REC:?" parseRecordStatus

recStatus_ :: MonadLabsatCtx c m => m ()
recStatus_ = void recStatus

--------------------------------------------------------------------------------
-- Mute command
--------------------------------------------------------------------------------

-- | MUTE command.
--
mute :: MonadLabsatCtx c m => Bool -> m ByteString
mute b = okCommand ("MUTE:" <> boolToBs b)

mute_ :: MonadLabsatCtx c m => Bool -> m ()
mute_ = void . mute

-- | MUTE command that supports individual channel control.
--
mute' :: MonadLabsatCtx c m => MuteConf -> m MuteConf
mute' mc =
  case mc ^. mcMuteAll of
    Just b  -> command ("MUTE:" <> b2c b) parseMute
    Nothing -> do
      let ch1 = fromMaybeBoolToMuteStr "CH1" $ mc ^. mcMuteCh1
          ch2 = fromMaybeBoolToMuteStr "CH2" $ mc ^. mcMuteCh2
          ch3 = fromMaybeBoolToMuteStr "CH3" $ mc ^. mcMuteCh3
      command ("MUTE:" <> ch1 <> ch2 <> ch3) parseMute
    where
      b2c = boolToBs
      fromMaybeBoolToMuteStr prefix m = case m of
        Nothing -> ""
        Just b  -> ":" ++ prefix ++ ":" ++ b2c b

mute'_ :: MonadLabsatCtx c m => MuteConf -> m ()
mute'_ = void . mute'
--------------------------------------------------------------------------------
-- Attentuation command
--------------------------------------------------------------------------------

-- | ATTN command.
--
attn :: MonadLabsatCtx c m => Int -> m AttnConf
attn i = command ("ATTN:" <> intToBs i) parseAttn

attn_ :: MonadLabsatCtx c m => Int -> m ()
attn_ = void . attn

-- | ATTN command that supports individual channel control.
--
attn' :: MonadLabsatCtx c m => AttnConf -> m AttnConf
attn' ac =
  case ac ^. acAttnAll of
    Just i  -> command ("ATTN:" <> intToBs i) parseAttn
    Nothing -> do
      let ch1 = fromMaybeIntToAttnStr "CH1" $ ac ^. acAttnCh1
          ch2 = fromMaybeIntToAttnStr "CH2" $ ac ^. acAttnCh2
          ch3 = fromMaybeIntToAttnStr "CH3" $ ac ^. acAttnCh3
      command ("ATTN:" <> ch1 <> ch2 <> ch3) parseAttn
    where
      fromMaybeIntToAttnStr prefix m = case m of
        Nothing -> ""
        Just i  -> ":" ++ prefix ++ ":" ++ intToBs i

attn'_ :: MonadLabsatCtx c m => AttnConf -> m ()
attn'_ = void . attn'

--------------------------------------------------------------------------------
-- Configuration command
--------------------------------------------------------------------------------

-- | CONF:PLAY:LOOP command.
--
confPlayLoop :: MonadLabsatCtx c m => Bool -> m ByteString
confPlayLoop b = okCommand ("CONF:PLAY:LOOP:" <> boolToBs b)

confPlayLoop_ :: MonadLabsatCtx c m => Bool -> m ()
confPlayLoop_ = void . confPlayLoop

-- | CONF:PLAY:PAUSE command.
--
confPlayPause :: MonadLabsatCtx c m => Int -> m ByteString
confPlayPause i = okCommand ("CONF:PLAY:PAUSE:" <> intToBs i)

confPlayPause_ :: MonadLabsatCtx c m => Int -> m ()
confPlayPause_ = void . confPlayPause

-- | CONF:PLAY:FOR command.
--
confPlayFor :: MonadLabsatCtx c m => Int -> m ByteString
confPlayFor i = okCommand ("CONF:PLAY:FOR:" <> intToBs i)

confPlayFor_ :: MonadLabsatCtx c m => Int -> m ()
confPlayFor_ = void . confPlayFor

-- | CONF:PLAY:FROM command.
--
confPlayFrom :: MonadLabsatCtx c m => Int -> m ByteString
confPlayFrom i = okCommand ("CONF:PLAY:FROM:" <> intToBs i)

confPlayFrom_ :: MonadLabsatCtx c m => Int -> m ()
confPlayFrom_ = void . confPlayFrom

-- | CONF:PLAY:FOR:FROM command.
--
confPlayForFrom :: MonadLabsatCtx c m => Int -> Int -> m ByteString
confPlayForFrom i j = okCommand ("CONF:PLAY:FOR:" <> intToBs i <> ":FROM:" <> intToBs j)

confPlayForFrom_ :: MonadLabsatCtx c m => Int -> Int -> m ()
confPlayForFrom_ i j = void $ confPlayForFrom i j

-- | CONF:REC:FOR command.
--
confRecFor :: MonadLabsatCtx c m => Int -> m ByteString
confRecFor i = okCommand ("CONF:REC:FOR:" <> intToBs i)

confRecFor_ :: MonadLabsatCtx c m => Int -> m ()
confRecFor_ = void . confRecFor

-- | CONF:SETUP:DISP:CONT command.
--
confContrast :: MonadLabsatCtx c m => Int -> m ByteString
confContrast i = okCommand ("CONF:SETUP:DISP:CONT:" <> intToBs i)

confContrast_ :: MonadLabsatCtx c m => Int -> m ()
confContrast_ = void . confContrast

-- | CONF:SETUP:DISP:BRIG command.
--
confBrightness :: MonadLabsatCtx c m => Int -> m ByteString
confBrightness i = okCommand ("CONF:SETUP:DISP:BRIG:" <> intToBs i)

confBrightness_ :: MonadLabsatCtx c m => Int -> m ()
confBrightness_ = void . confBrightness

-- | CONF:SETUP:PSAV command.
--
confPsav :: MonadLabsatCtx c m => Bool -> m ByteString
confPsav b = okCommand ("CONF:SETUP:PSAV:" <> boolToBs b)

confPsav_ :: MonadLabsatCtx c m => Bool -> m ()
confPsav_ = void . confPsav

-- | CONF:SETUP:SYNC command.
--
confSync :: MonadLabsatCtx c m => Bool -> m ByteString
confSync b = okCommand ("CONF:SETUP:SYNC:" <> boolToBs b)

confSync_ :: MonadLabsatCtx c m => Bool -> m ()
confSync_ = void . confSync


-- | CONF:SETUP:BEEP command.
--
confBeep :: MonadLabsatCtx c m => Bool -> m ByteString
confBeep b = okCommand ("CONF:SETUP:BEEP:" <> boolToBs b)

confBeep_ :: MonadLabsatCtx c m => Bool -> m ()
confBeep_ = void . confBeep


-- | CONF:SETUP:TIME:UTC command.
--
confTimeUTC :: MonadLabsatCtx c m => m ByteString
confTimeUTC = okCommand "CONF:SETUP:TIME:UTC:Y"

confTimeUTC_ :: MonadLabsatCtx c m => m ()
confTimeUTC_ = void confTimeUTC

-- | CONF:SETUP:TIME:MAN command.
--
confTimeManual :: MonadLabsatCtx c m
               => ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> m ByteString
confTimeManual year month day hours minutes seconds =
  okCommand ("CONF:SETUP:TIME:UTC:N:MAN:" <> intercalate ":" [year, month, day, hours, minutes, seconds])

confTimeManual_ :: MonadLabsatCtx c m
                => ByteString
                -> ByteString
                -> ByteString
                -> ByteString
                -> ByteString
                -> ByteString
                -> m ()
confTimeManual_ y m d h m' s = void $ confTimeManual y m d h m' s

-- | CONF:SETUP:DIGI command.
--
confDigi :: MonadLabsatCtx c m => CANChannel -> DigitalFunction -> m ByteString
confDigi ch df = okCommand ("CONF:SETUP:DIGI:" <> showToBs ch <> ":" <> showToBs df)

confDigi_ :: MonadLabsatCtx c m => CANChannel -> DigitalFunction -> m ()
confDigi_ ch df = void $ confDigi ch df

-- | CONF:SETUP:CAN:CH*:BAUD command.
--
confCANBaud :: MonadLabsatCtx c m => CANChannel -> Double -> m Double
confCANBaud ch val = command ("CONF:SETUP:CAN:" <> showToBs ch <> ":BAUD:" <> showToBs val) parseCANBaud

confCANBaud_ :: MonadLabsatCtx c m => CANChannel -> Double -> m ()
confCANBaud_ ch val = void $ confCANBaud ch val

-- | CONF:SETUP:CAN:SILENT command.
--
confCANSilent :: MonadLabsatCtx c m => Bool -> m ByteString
confCANSilent b = okCommand ("CONF:SETUP:CAN:SILENT:" <> boolToBs b)

confCANSilent_ :: MonadLabsatCtx c m => Bool -> m ()
confCANSilent_ = void . confCANSilent

-- | CONF:SETUP:CAN:LOGFILE command.
--
confCANLogfile :: MonadLabsatCtx c m => Bool -> m ByteString
confCANLogfile b = okCommand ("CONF:SETUP:CAN:LOGFILE:" <> boolToBs b)

confCANLogfile_ :: MonadLabsatCtx c m => Bool -> m ()
confCANLogfile_ = void . confCANLogfile

-- | CONF:SETUP:CAN:REPLAYFILE command.
--
confCANReplayfile :: MonadLabsatCtx c m => Bool -> m ByteString
confCANReplayfile b = okCommand ("CONF:SETUP:CAN:REPLAYFILE:" <> boolToBs b)

confCANReplayfile_ :: MonadLabsatCtx c m => Bool -> m ()
confCANReplayfile_ = void . confCANReplayfile

-- | CONF:SETUP:CLKREF:OCXO command.
--
confClkRefOCXO :: MonadLabsatCtx c m => m ByteString
confClkRefOCXO = okCommand "CONF:SETUP:CLKREF:OCXO"

confClkRefOCXO_ :: MonadLabsatCtx c m => m ()
confClkRefOCXO_ = void confClkRefOCXO

-- | CONF:SETUP:CLKREF:TCXO command.
--
confClkRefTCXO :: MonadLabsatCtx c m => m ByteString
confClkRefTCXO = okCommand "CONF:SETUP:CLKREF:TCXO"

confClkRefTCXO_ :: MonadLabsatCtx c m => m ()
confClkRefTCXO_ = void confClkRefTCXO

-- | CONF:SETUP:CLKREF:EXT command.
--
confClkRefEXT :: MonadLabsatCtx c m => m ByteString
confClkRefEXT = okCommand "CONF:SETUP:CLKREF:EXT"

confClkRefEXT_ :: MonadLabsatCtx c m => m ()
confClkRefEXT_ = void confClkRefEXT

-- | CONF:SETUP:CLKREF:REFOUT command.
--
confClkRefout :: MonadLabsatCtx c m => Bool -> m ByteString
confClkRefout b = okCommand ("CONF:SETUP:CLKREF:REFOUT:" <> boolToBs b)

confClkRefout_ :: MonadLabsatCtx c m => Bool -> m ()
confClkRefout_ = void . confClkRefout

-- | CONF:CONS command.
--
confConstellationPreset :: MonadLabsatCtx c m => ConstellationPresetConf -> m ConstellationPresetConf
confConstellationPreset cc = command ("CONF:CONS:" <> showToBs cc) parseConsPreset

confConstellationPreset_ :: MonadLabsatCtx c m => ConstellationPresetConf -> m ()
confConstellationPreset_ = void . confConstellationPreset

-- | CONF:CONS command.
--
confConstellationFreq :: MonadLabsatCtx c m => ConstellationFreqConf -> m ConstellationFreqConf
confConstellationFreq cc = command ("CONF:CONS:" <> showToBs cc) parseConsFreq

confConstellationFreq_ :: MonadLabsatCtx c m => ConstellationFreqConf -> m ()
confConstellationFreq_ = void . confConstellationFreq

-- | CONF:? command.
--
confQuery :: MonadLabsatCtx c m => m ByteString
confQuery = command "CONF:?" parseUntilPrompt

confQuery_ :: MonadLabsatCtx c m => m ()
confQuery_ = void confQuery

-- | Labsat Main
--
labsatMain :: MonadControl m => Text -> Int -> Maybe Int -> m ()
labsatMain ip port timeout = do
  putStrLn "Labsat!"
  print ip
  print port
  print timeout

  -- Example
  runCtx $ runStatsCtx $
    runLabsatCtx (encodeUtf8 ip) port timeout $
      connectMsg >> info >> pure ()
