{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Labsat.Types where


import Data.ByteString()
import Data.Scientific
import Preamble


-- | HelpCommands: List of commands returned by 'HELP'
--
newtype HelpCommands =
  HelpCommands [ByteString]
  deriving (Eq, Show)


-- | Info: List of information returned by 'TYPE'
--
newtype Info =
  Info [ByteString]
  deriving (Eq, Show)


-- | PlayStatus: Playback status, Idle or Playing
--
data PlayStatus =
    Playing ByteString ByteString
  | PlayIdle
  deriving (Eq, Show)

-- | RecordStatus: Recording status, Idle or Recording
--
data RecordStatus =
    Recording ByteString ByteString
  | RecordIdle
  deriving (Eq, Show)

-- | PlayConf: Playback configuration
--
data PlayConf = PlayConf
  { -- File to playback
    _pcFile :: ByteString
    -- Playback duration
  , _pcFor  :: Maybe Int
    -- Starting position
  , _pcFrom :: Maybe Int
  } deriving (Eq, Show)

$(makeLenses ''PlayConf)

-- | RecConf: Record configuration
--
data RecordConf = RecordConf
  { -- File to record
    _rcFile :: Maybe ByteString
    -- Record duration
  , _rcFor  :: Maybe Int
  } deriving (Eq, Show)

$(makeLenses ''RecordConf)

-- | MuteConf: Mute configuration
--
data MuteConf = MuteConf
  { -- Mute all (overrides the rest)
    _mcMuteAll :: Maybe Bool
    -- Mute channel 1
  , _mcMuteCh1 :: Maybe Bool
    -- Mute channel 2
  , _mcMuteCh2 :: Maybe Bool
    -- Mute channel 3
  , _mcMuteCh3 :: Maybe Bool
  } deriving (Eq, Show)

$(makeLenses ''MuteConf)

-- | Attentuation configuration.
--
data AttnConf = AttnConf
  { -- Attentuation value for all channels
    _acAttnAll :: Maybe Int
    -- Attentuation value for channel 1
  , _acAttnCh1 :: Maybe Int
    -- Attentuation value for channel 2
  , _acAttnCh2 :: Maybe Int
    -- Attentuation value for channel 3
  , _acAttnCh3 :: Maybe Int
  } deriving (Eq, Show)

$(makeLenses ''AttnConf)

-- | Media: File or Directory
--
data Media =
    File ByteString ByteString
  | Dir ByteString
  deriving (Eq, Show)

-- | MediaList: List of files and directories
--
newtype MediaList =
  MediaList [Media]
  deriving (Eq, Show)

-- | Media type
--
data MediaType = USB | SD | SATA
  deriving (Eq, Show)

-- | Digital Function type
data DigitalFunction = OFF | CAN1 | CAN2 | RS232 | DIGI1 | DIGI2 | OnePPS
  deriving (Eq, Show)

-- | CAN Bus Channel
--
data CANChannel = CAN_CH1 | CAN_CH2

instance Show CANChannel where
  show CAN_CH1 = "CH1"
  show CAN_CH2 = "CH2"

-- | Bandwidth type
--
data Bandwidth = BW_10 | BW_30 | BW_56
  deriving (Eq)

instance Show Bandwidth where
  show BW_10 = "BW:10"
  show BW_30 = "BW:30"
  show BW_56 = "BW:56"

-- | Quantization type
--
data Quantization = QUA1 | QUA2 | QUA3
  deriving (Eq)

instance Show Quantization where
  show QUA1 = "QUA:1"
  show QUA2 = "QUA:2"
  show QUA3 = "QUA:3"

-- | Frequency presets
--
data FreqPreset =
    Beidou1
  | Beidou2
  | Beidou3
  | Freq1191_795MHz
  | Freq1233_738MHz
  | Freq1567_236MHz
  | Freq1580MHz
  | GLONASS1
  | GLONASS2
  | GPS1
  | GPS2
  | GPS5
  | Galileo6
  | LBand
  | UnknownPreset ByteString
  deriving (Eq)

instance Show FreqPreset where
  show Beidou1           = "5"
  show Beidou2           = "12"
  show Beidou3           = "8"
  show Freq1191_795MHz   = "14"
  show Freq1233_738MHz   = "10"
  show Freq1567_236MHz   = "4"
  show Freq1580MHz       = "2"
  show GLONASS1          = "1"
  show GLONASS2          = "9"
  show GPS1              = "3"
  show GPS2              = "11"
  show GPS5              = "13"
  show Galileo6          = "7"
  show LBand             = "6"
  show (UnknownPreset a) = show a

freqPresetLookup :: ByteString -> FreqPreset
freqPresetLookup bs =
  case bs of
    "5"  -> Beidou1
    "12" -> Beidou2
    "8"  -> Beidou3
    "14" -> Freq1191_795MHz
    "10" -> Freq1233_738MHz
    "4"  -> Freq1567_236MHz
    "2"  -> Freq1580MHz
    "1"  -> GLONASS1
    "9"  -> GLONASS2
    "3"  -> GPS1
    "11" -> GPS2
    "13" -> GPS5
    "7"  -> Galileo6
    "6"  -> LBand
    other -> UnknownPreset other

-- | Constellation Preset Configuration
--
data ConstellationPresetConf = ConstellationPresetConf
  { -- Signal quantization
    _cpcQuantization :: Quantization
    -- Bandwidth
  , _cpcBandwidth :: Bandwidth
    -- Frequency presets (up to three)
  , _cpcFreqPresets :: [FreqPreset]
  }

$(makeLenses ''ConstellationPresetConf)

instance Show ConstellationPresetConf where
  show cpc = joinColon [quant, bandwidth, presets]
    where joinColon = intercalate ":"
          freqs     = cpc ^. cpcFreqPresets
          quant     = show $ cpc ^. cpcQuantization
          bandwidth = show $ cpc ^. cpcBandwidth
          presets   = if null freqs
                      then ""
                      else joinColon ("SETS" : map show freqs)

-- | Constellation Frequency Configuration
--
data ConstellationFreqConf = ConstellationFreqConf
  { -- Signal quantization
    _cfcQuantization :: Quantization
    -- Bandwidth
  , _cfcBandwidth :: Bandwidth
    -- Frequencies
  , _cfcFrequencies :: [Scientific]
  } deriving (Eq)

$(makeLenses ''ConstellationFreqConf)

instance Show ConstellationFreqConf where
  show cfc = joinColon [quant, bandwidth, presets]
    where joinColon = intercalate ":"
          freqs     = cfc ^. cfcFrequencies
          quant     = show $ cfc ^. cfcQuantization
          bandwidth = show $ cfc ^. cfcBandwidth
          presets   = if null freqs
                      then ""
                      else joinColon ("FREQ" : map show freqs)

-- | Satellite navigation systems
--
data Constellation
  = BDS        -- ^ Chinese BeiDou (Compass)
  | GAL        -- ^ European Galileo
  | GLO        -- ^ Russian GLONASS
  | GPS        -- ^ US NAVSTAR GPS
  deriving (Eq, Show)


-- | Satellite CNO: Carrier-to-noise ration for a satellite
--
data SatelliteCNO =
  SatelliteCNO ByteString ByteString
  deriving (Eq, Show)


-- | CNO: Carrier-to-noise ratio for a constellation
--
data ConstellationCNO =
  ConstellationCNO Constellation ByteString [SatelliteCNO]
  deriving (Eq, Show)


-- | Location
--
data Location = Location
  {
    _time      :: Double
  , _height    :: (Double, ByteString)
  , _lattitude :: (Double, ByteString)
  , _longitude :: (Double, ByteString)
  } deriving (Eq, Show)

$(makeLenses ''Location)

