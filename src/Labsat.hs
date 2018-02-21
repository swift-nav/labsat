{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Labsat where

import           Control.Concurrent.Async.Lifted   (race_)
import           Control.Concurrent.Lifted         (threadDelay)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary           as B
import           Data.Conduit.Network
import           Labsat.Ctx
import           Labsat.Parser
import           Labsat.Types
import           Preamble
import           System.IO                 hiding (print, putStrLn)


--------------------------------------------------------------------------------

-- | Bracketed opening, closing of a binary file.
--
withBinaryFile' :: (MonadIO m, MonadBaseControl IO m) => FilePath -> (Handle -> m a) -> m a
withBinaryFile' f = flip bracket (liftIO . hClose) $ do
    h <- liftIO $ openBinaryFile f AppendMode
    liftIO $ hSetBuffering h LineBuffering
    pure h

-- | Send command
--
sendit :: MonadTcpCtx c m => ByteString -> m ()
sendit s = do
  ad <- view catAppData
  yield s $$ appSink ad

-- | Add Labsat end-of-line delimiters and send command
--
sendCmd :: MonadTcpCtx c m => ByteString -> m ()
sendCmd s = sendit (s ++ "\r\r\n")

-- | Strip ANSI color codes
--
colorStripper :: MonadIO m => Conduit ByteString m ByteString
colorStripper = do
  mx <- await
  case mx of
    Nothing -> return ()
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
receiveit :: MonadTcpCtx c m => Parser a -> m a
receiveit p = runResourceT $ do
  ad <- view catAppData
  appSource ad =$= colorStripper $$ sinkParser p

-- | Receive command response, strip color codes, and log to file
--
logit :: MonadTcpCtx c m => FilePath -> m ()
logit lf = runResourceT $ do
  ad <- view catAppData
  withBinaryFile' lf $ \lh ->
    appSource ad =$= colorStripper $$ B.sinkHandle lh

-- | Send a command and parser for its response.
--
command :: (MonadTcpCtx c m) => ByteString -> Parser a -> m a
command c p = do
  sendCmd c
  receiveit $ parseCommandAck c *> p

-- | Send a command and parse for OK and the prompt
--
okCommand :: (MonadTcpCtx c m) => ByteString -> m ByteString
okCommand = flip command okPrompt

-- Swallow first message, capture and print second one (debug)
--
debugRecv :: ByteString -> ByteString -> Int -> IO ()
debugRecv msg host port =
  runCtx $ runStatsCtx $
    runGeneralTCPClient (clientSettings port host) $
      flip runTcpCtx $ do
        msg0 <- receiveit parseUntilPrompt
        putStrLn "First message:"
        print msg0

        putStrLn "Debug message:"
        sendCmd msg
        res <- receiveit parseUntilPrompt
        print (res <> "LABSAT_V3 >")

testCommand :: (MonadStatsCtx c m, Show a) => ByteString -> Int -> TransT TcpCtx m a -> m ()
testCommand host port cmd =
  runGeneralTCPClient (clientSettings port host) $
    flip runTcpCtx $ do
      msg0 <- receiveit parseFirstLabsatMsg
      print msg0
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
help :: MonadTcpCtx c m => m HelpCommands
help = command "HELP" parseHelp

--------------------------------------------------------------------------------
-- Media command
--------------------------------------------------------------------------------

-- | MEDIA:LIST command.
--
mediaList :: MonadTcpCtx c m => m MediaList
mediaList = command "MEDIA:LIST" parseMediaList

-- | MEDIA:CHDIR:\ command.
--
mediaChdirRoot :: MonadTcpCtx c m => m ByteString
mediaChdirRoot = command "MEDIA:CHDIR:\\" parseMediaChdir

-- | MEDIA:CHDIR:.. command.
--
mediaChdirUp :: MonadTcpCtx c m => m ByteString
mediaChdirUp = command "MEDIA:CHDIR:.." parseMediaChdir

-- | MEDIA:CHDIR:<dir> command.
--
mediaChdir :: MonadTcpCtx c m => ByteString -> m ByteString
mediaChdir d = command ("MEDIA:CHDIR:" <> d) parseMediaChdir

-- | MEDIA:PROTECT:Y/N:FILE command.
--
mediaProtect :: MonadTcpCtx c m => Bool -> ByteString -> m ByteString
mediaProtect b f = okCommand $ "MEDIA:PROTECT:" <> bool "N:" "Y:" b <> f

-- | MEDIA:DELETE:FILE command.
--
mediaDelete :: MonadTcpCtx c m => ByteString -> m ByteString
mediaDelete f = okCommand $ "MEDIA:DELETE:" <> f

-- | MEDIA:SELECT:SD/USB/SATA command.
--
mediaSelect :: MonadTcpCtx c m => MediaType -> m ByteString
mediaSelect s = okCommand $ "MEDIA:SELECT:" <> showToBs s

--------------------------------------------------------------------------------
-- Play command
--------------------------------------------------------------------------------

-- | PLAY command.
--
play :: MonadTcpCtx c m => ByteString -> m ByteString
play f = command ("PLAY:FILE:" <> f) (parsePlay f)

-- | PLAY command that supports FOR and FROM
--
play' :: MonadTcpCtx c m => PlayConf -> m ByteString
play' pc = command cmd (parsePlay file')
  where
    for'  = argFromMaybe ":FOR:"  $ pc ^.pcFor
    from' = argFromMaybe ":FROM:" $ pc ^.pcFrom
    file' = pc ^. pcFile
    cmd = "PLAY:FILE:" <> file' <> for' <> from'

-- | PLAY:STOP command.
--
playStop :: MonadTcpCtx c m => m ByteString
playStop = okCommand "PLAY:STOP"

-- | PLAY:? command.
--
playStatus :: MonadTcpCtx c m => m PlayStatus
playStatus = command "PLAY:?" parsePlayStatus

--------------------------------------------------------------------------------
-- Type command
--------------------------------------------------------------------------------

-- | TYPE command. Named 'info' to avoid the obvious conflict.
--
info :: MonadTcpCtx c m => m Info
info = command "TYPE" parseInfo

--------------------------------------------------------------------------------
-- Find command
--------------------------------------------------------------------------------

-- | FIND command.
--
find :: MonadTcpCtx c m => m ByteString
find = okCommand "FIND"

--------------------------------------------------------------------------------
-- Mon command
--------------------------------------------------------------------------------

-- | MON:NMEA:ON command.
--
nmeaOn :: MonadTcpCtx c m => m ByteString
nmeaOn = okCommand "MON:NMEA:ON"

-- | MON:NMEA:OFF command.
--
nmeaOff :: MonadTcpCtx c m => m ByteString
nmeaOff = okCommand "MON:NMEA:OFF"

-- | Capture NMEA log for 'n' seconds
--
nmeaLog :: (MonadIO m, MonadTcpCtx c m) => Int -> FilePath -> m ()
nmeaLog n f = do
  _ <- nmeaOn
  race_ (threadDelay $ n * 1000000) $ logit f
  _ <- nmeaOff
  return ()

-- | MON:LOC command.
--
monLoc :: MonadTcpCtx c m => m Location
monLoc = command "MON:LOC" parseMonLoc

-- | MON:SAT command.
--
monSat :: MonadTcpCtx c m => m [ConstellationCNO]
monSat = command "MON:SAT" parseMonSat

--------------------------------------------------------------------------------
-- Rec command
--------------------------------------------------------------------------------

-- | REC command.
--
rec :: MonadTcpCtx c m => m ByteString
rec = command "REC" parseRec

-- | REC command that supports FILE and FOR.
--
rec' :: MonadTcpCtx c m => RecordConf -> m ByteString
rec' rc = command cmd parseRec
  where
    file' = argFromMaybe ":FILE:" $ rc ^. rcFile
    for'  = argFromMaybe ":FOR:"  $ rc ^. rcFor
    cmd = "REC" <> file' <> for'

-- | REC:STOP command.
--
recStop :: MonadTcpCtx c m => m ByteString
recStop = okCommand "REC:STOP"

-- | REC:? command.
--
recStatus :: MonadTcpCtx c m => m RecordStatus
recStatus = command "REC:?" parseRecordStatus

--------------------------------------------------------------------------------
-- Mute command
--------------------------------------------------------------------------------

-- | MUTE command.
--
mute :: MonadTcpCtx c m => Bool -> m ByteString
mute b = okCommand ("MUTE:" <> boolToBs b)

-- | MUTE command that supports individual channel control.
--
mute' :: MonadTcpCtx c m => MuteConf -> m ByteString
mute' mc =
  case mc ^. mcMuteAll of
    Just b  -> okCommand ("MUTE:" <> b2c b)
    Nothing -> do
      let ch1 = fromMaybeBoolToMuteStr "CH1" $ mc ^. mcMuteCh1
          ch2 = fromMaybeBoolToMuteStr "CH2" $ mc ^. mcMuteCh2
          ch3 = fromMaybeBoolToMuteStr "CH3" $ mc ^. mcMuteCh3
      okCommand ("MUTE:" <> ch1 <> ch2 <> ch3)
    where
      b2c = boolToBs
      fromMaybeBoolToMuteStr prefix m = case m of
        Nothing -> ""
        Just b  -> ":" ++ prefix ++ ":" ++ b2c b

--------------------------------------------------------------------------------
-- Attentuation command
--------------------------------------------------------------------------------

-- | ATTN command.
--
attn :: MonadTcpCtx c m => Int -> m AttnConf
attn i = command ("ATTN:" <> intToBs i) parseAttn

-- | ATTN command that supports individual channel control.
--
attn' :: MonadTcpCtx c m => AttnConf -> m AttnConf
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

--------------------------------------------------------------------------------
-- Configuration command
--------------------------------------------------------------------------------

-- | CONF:PLAY:LOOP command.
--
confPlayLoop :: MonadTcpCtx c m => Bool -> m ByteString
confPlayLoop b = okCommand ("CONF:PLAY:LOOP:" <> boolToBs b)

-- | CONF:PLAY:PAUSE command.
--
confPlayPause :: MonadTcpCtx c m => Int -> m ByteString
confPlayPause i = okCommand ("CONF:PLAY:PAUSE:" <> intToBs i)

-- | CONF:PLAY:FOR command.
--
confPlayFor :: MonadTcpCtx c m => Int -> m ByteString
confPlayFor i = okCommand ("CONF:PLAY:FOR:" <> intToBs i)

-- | CONF:PLAY:FROM command.
--
confPlayFrom :: MonadTcpCtx c m => Int -> m ByteString
confPlayFrom i = okCommand ("CONF:PLAY:FROM:" <> intToBs i)

-- | CONF:PLAY:FOR:FROM command.
--
confPlayForFrom :: MonadTcpCtx c m => Int -> Int -> m ByteString
confPlayForFrom i j = okCommand ("CONF:PLAY:FOR:" <> intToBs i <> ":FROM:" <> intToBs j)

-- | CONF:REC:FOR command.
--
confRecFor :: MonadTcpCtx c m => Int -> m ByteString
confRecFor i = okCommand ("CONF:REC:FOR:" <> intToBs i)

-- | CONF:SETUP:DISP:CONT command.
--
confContrast :: MonadTcpCtx c m => Int -> m ByteString
confContrast i = okCommand ("CONF:SETUP:DISP:CONT:" <> intToBs i)

-- | CONF:SETUP:DISP:BRIG command.
--
confBrightness :: MonadTcpCtx c m => Int -> m ByteString
confBrightness i = okCommand ("CONF:SETUP:DISP:BRIG:" <> intToBs i)

-- | CONF:SETUP:PSAV command.
--
confPsav :: MonadTcpCtx c m => Bool -> m ByteString
confPsav b = okCommand ("CONF:SETUP:PSAV:" <> boolToBs b)

-- | CONF:SETUP:SYNC command.
--
confSync :: MonadTcpCtx c m => Bool -> m ByteString
confSync b = okCommand ("CONF:SETUP:SYNC:" <> boolToBs b)


-- | CONF:SETUP:BEEP command.
--
confBeep :: MonadTcpCtx c m => Bool -> m ByteString
confBeep b = okCommand ("CONF:SETUP:BEEP:" <> boolToBs b)


-- | CONF:SETUP:TIME:UTC command.
--
confTimeUTC :: MonadTcpCtx c m => m ByteString
confTimeUTC = okCommand "CONF:SETUP:TIME:UTC:Y"

-- | CONF:SETUP:TIME:MAN command.
--
confTimeManual :: MonadTcpCtx c m
               => ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> m ByteString
confTimeManual year month day hours minutes seconds =
  okCommand ("CONF:SETUP:TIME:UTC:N:MAN:" <> intercalate ":" [year, month, day, hours, minutes, seconds])

-- | CONF:SETUP:DIGI command.
--
confDigi :: MonadTcpCtx c m => CANChannel -> DigitalFunction -> m ByteString
confDigi ch df = okCommand ("CONF:SETUP:DIGI:" <> showToBs ch <> ":" <> showToBs df)

-- | CONF:SETUP:CAN:CH*:BAUD command.
--
confCANBaud :: MonadTcpCtx c m => CANChannel -> Double -> m Double
confCANBaud ch val = command ("CONF:SETUP:CAN:" <> showToBs ch <> ":BAUD:" <> showToBs val) parseCANBaud


-- | CONF:SETUP:CAN:SILENT command.
--
confCANSilent :: MonadTcpCtx c m => Bool -> m ByteString
confCANSilent b = okCommand ("CONF:SETUP:CAN:SILENT:" <> boolToBs b)

-- | CONF:SETUP:CAN:LOGFILE command.
--
confCANLogfile :: MonadTcpCtx c m => Bool -> m ByteString
confCANLogfile b = okCommand ("CONF:SETUP:CAN:LOGFILE:" <> boolToBs b)

-- | CONF:SETUP:CAN:REPLAYFILE command.
--
confCANReplayfile :: MonadTcpCtx c m => Bool -> m ByteString
confCANReplayfile b = okCommand ("CONF:SETUP:CAN:REPLAYFILE:" <> boolToBs b)

-- | CONF:SETUP:CLKREF:OCXO command.
--
confClkRefOCXO :: MonadTcpCtx c m => m ByteString
confClkRefOCXO = okCommand "CONF:SETUP:CLKREF:OCXO"

-- | CONF:SETUP:CLKREF:TCXO command.
--
confClkRefTCXO :: MonadTcpCtx c m => m ByteString
confClkRefTCXO = okCommand "CONF:SETUP:CLKREF:TCXO"

-- | CONF:SETUP:CLKREF:EXT command.
--
confClkRefEXT :: MonadTcpCtx c m => m ByteString
confClkRefEXT = okCommand "CONF:SETUP:CLKREF:EXT"

-- | CONF:SETUP:CLKREF:REFOUT command.
--
confClkRefout :: MonadTcpCtx c m => Bool -> m ByteString
confClkRefout b = okCommand ("CONF:SETUP:CLKREF:REFOUT:" <> boolToBs b)

-- | CONF:CONS command.
--
confConstellationPreset :: MonadTcpCtx c m => ConstellationPresetConf -> m ConstellationPresetConf
confConstellationPreset cc = command ("CONF:CONS:" <> showToBs cc) parseConsPreset

-- | CONF:CONS command.
--
confConstellationFreq :: MonadTcpCtx c m => ConstellationFreqConf -> m ConstellationFreqConf
confConstellationFreq cc = command ("CONF:CONS:" <> showToBs cc) parseConsFreq

-- | CONF:? command.
--
confQuery :: MonadTcpCtx c m => m ByteString
confQuery = command "CONF:?" parseUntilPrompt

a'' :: ByteString
a'' = "\255\253\SOH\255\253!\255\251\SOH\255\251\ETX\ESC[0m\r\r\n\ESC[0mLABSAT_V3 >"
a' :: ByteString
a' = "\255\253\SOH\255\253!\255\251\SOH\255\251\ETX\r\r\nLABSAT_V3 >"
b' :: ByteString
b' = "HELP\r\r\nCurrent commands are : \r\r\n\r\r\n\ESC[1mHELP\ESC[22m\r\r\n\ESC[1mTYPE\ESC[22m\r\r\n\ESC[1mFIND\ESC[22m\r\r\n\ESC[1mMON\ESC[22m\r\r\n\ESC[1mPLAY\ESC[22m\r\r\n\ESC[1mREC\ESC[22m\r\r\n\ESC[1mATTN\ESC[22m\r\r\n\ESC[1mCONF\ESC[22m\r\r\n\ESC[1mMEDIA\ESC[22m\r\r\n\ESC[1mMUTE\ESC[22m\r\r\n\r\r\n\ESC[0m\r\r\n\ESC[0mLABSAT_V3 >\r\r\n\ESC[0m\r\r\n\ESC[0mLABSAT_V3 >"
c' :: ByteString
c' = "HELP\r\r\nCurrent commands are : \r\r\n\r\r\nHELP\r\r\nTYPE\r\r\nFIND\r\r\nMON\r\r\nPLAY\r\r\nREC\r\r\nATTN\r\r\nCONF\r\r\nMEDIA\r\r\nMUTE\r\r\n\r\r\n\r\r\nLABSAT_V3 >\r\r\n\r\r\nLABSAT_V3 >\r\r\n\r\r\nLABSAT_V3 >"
d' :: ByteString
d' = "\255\253\SOH\255\253!\255\251\SOH\255\251\ETXin use with 10.11.21.166\r\r\n"
ee' :: ByteString
ee' = "MEDIA:LIST\r\r\nABC \ESC[40G 00:00:00\r\r\nASDF\r\r\nFile_001 \ESC[40G 00:05:40\r\r\nFile_002 \ESC[40G 00:21:17\r\r\nLabSat 3 Wideband Demo SSD files\r\r\n\r\r\nLABSAT_V3 >"
f' :: ByteString
f' = "File_002 \ESC[40G 00:21:17\r\r\nLabSat 3 Wideband Demo SSD files\r\r\n\r\r\nLABSAT_V3 >"
g' :: ByteString
g' = "PLAY:FILE:File_001\r\r\nFile_001\r\r\n\r\r\nLABSAT_V3 >"
j' :: ByteString
j' = "PLAY:?\r\r\nPLAY:/mnt/sata/File_001:DUR:00:00:20\r\r\n\r\r\nLABSAT_V3 >"
k' :: ByteString
k' = "PLAY:?\r\r\nPLAY:IDLE\r\r\n\r\r\nLABSAT_V3 >"

cno :: ByteString
cno = "GPS 9\r\r\n05,42,07,52,08,43,09,52,16,35,23,47,27,43,28,42,30,49\r\r\n\r\r\nGLO 10\r\r\n01,50,02,52,03,36,08,37,10,36,11,50,12,50,13,32,20,31,21,40\r\r\n\r\r\nBDS 0\r\r\n\r\r\nGAL 4\r\r\n01,44,04,42,19,44,20,36\r\r\n\r\r\n"


attn0 :: ByteString
attn0 = "OK:ATTN:CH1:-10 \r\r\nOK:ATTN:CH2:-10 \r\r\nOK:ATTN:CH3:-10 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

attn1 :: ByteString
attn1 = "OK:ATTN:CH1:0 \r\r\nOK:ATTN:CH2:0 \r\r\nOK:ATTN:CH3:0 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

attn2 :: ByteString
attn2 = "OK:ATTN:CH1:-20 \r\r\nOK:ATTN:CH2:-10 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

attn3 :: ByteString
attn3 = "OK:ATTN:CH1:0 \r\r\nOK:ATTN:CH2:0 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

attn4 :: ByteString
attn4 = "OK:ATTN:CH1:0 \r\r\nOK:ATTN:CH2:0 \r\r\nOK:ATTN:CH3:0 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

confcons :: ByteString
confcons = "QUA-1, BW-10, Available ch(3) 3, 11, 13 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

-- Careful w/ the examples above. If there are escape codes in them, the examples below will barf
--main :: IO ()
--main = do
--  print $ parse (parseFirstLabsatMsg) a'
--  print $ parse (parseCommandAck "HELP" *> parseHelp) c'
--  print $ parse (parseCommandAck "MEDIA:LIST" *> parseMediaList) ee'
--  print $ parse (parseCommandAck "PLAY:FILE:File_001" *> (parsePlay "File_001")) g'
--  print $ parse (parseCommandAck "PLAY:?" *> parseIdle) k'
--  print $ parse (parseCommandAck "PLAY:?" *> parsePlaying) j'

labsatMain :: MonadControl m => Text -> Int -> m ()
labsatMain ip port= do
  putStrLn "yay"
  print ip
  print port
