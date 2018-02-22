{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Labsat.Parser
  ( tests
  ) where

import Data.Attoparsec.ByteString
import Data.ByteString
import Labsat.Parser
import Labsat.Types
import Preamble
import Test.Tasty
import Test.Tasty.HUnit

parser :: (Eq a, Show a) => Parser a -> ByteString -> Either String a -> Assertion
parser p s r = parseOnly p s @?= r

--------------------------------------------------------------------------------
-- HELP Parsers
--------------------------------------------------------------------------------
helpOutput :: ByteString
helpOutput = "Current commands are : \r\r\n\r\r\nHELP\r\r\nTYPE\r\r\nFIND\r\r\nMON\r\r\nPLAY\r\r\nREC\r\r\nATTN\r\r\nCONF\r\r\nMEDIA\r\r\nMUTE\r\r\n\r\r\n\r\r\nLABSAT_V3 >"

helpResult :: HelpCommands
helpResult = HelpCommands ["HELP","TYPE","FIND","MON","PLAY","REC","ATTN","CONF","MEDIA","MUTE"]

testHelp :: TestTree
testHelp =
  testGroup "Test help parser"
    [ testCase "Help without colors" $ parser parseHelp helpOutput $ Right helpResult
    ]

--------------------------------------------------------------------------------
-- MEDIA Parsers
--------------------------------------------------------------------------------
mediaListOutput :: ByteString
mediaListOutput = "ABC \ESC[40G 00:00:00\r\r\nASDF\r\r\nFile_001 \ESC[40G 00:05:40\r\r\nFile_002 \ESC[40G 00:21:17\r\r\nLabSat 3 Wideband Demo SSD files\r\r\n\r\r\nLABSAT_V3 >"

mediaListResult :: MediaList
mediaListResult = MediaList [File "ABC" "00:00:00",Dir "ASDF",File "File_001" "00:05:40",File "File_002" "00:21:17",Dir "LabSat 3 Wideband Demo SSD files"]

testMedia :: TestTree
testMedia =
  testGroup "Test media parsers"
    [ testCase "Media list" $ parser parseMediaList mediaListOutput $ Right mediaListResult
    ]

--------------------------------------------------------------------------------
-- PLAY Parsers
--------------------------------------------------------------------------------
playFileOutput :: ByteString
playFileOutput = "File_001\r\r\n\r\r\nLABSAT_V3 >"

statusPlayingOutput :: ByteString
statusPlayingOutput = "PLAY:/mnt/sata/File_001:DUR:00:00:20\r\r\n\r\r\nLABSAT_V3 >"

statusPlayingResult :: PlayStatus
statusPlayingResult = Playing "File_001" "00:00:20"

statusIdleOutput :: ByteString
statusIdleOutput = "PLAY:IDLE\r\r\n\r\r\nLABSAT_V3 >"

testPlay :: TestTree
testPlay =
  testGroup "Test play parsers"
    [ testCase "Play file" $ parser (parsePlay "File_001") playFileOutput $ Right "File_001"
    , testCase "Status playing" $ parser parsePlayStatus statusPlayingOutput $ Right statusPlayingResult
    , testCase "Status idle" $ parser parsePlayStatus statusIdleOutput $ Right PlayIdle
    ]

--------------------------------------------------------------------------------
-- REC Parsers
--------------------------------------------------------------------------------
statusRecordingOutput :: ByteString
statusRecordingOutput = "REC:/mnt/sata/File_004:DUR:00:00:08\r\r\n\r\r\nLABSAT_V3 >"

statusRecordingResult :: RecordStatus
statusRecordingResult = Recording "File_004" "00:00:08"

statusRecordIdleOutput :: ByteString
statusRecordIdleOutput = "REC:IDLE\r\r\n\r\r\nLABSAT_V3 >"

testRecord :: TestTree
testRecord =
  testGroup "Test record parsers"
    [ testCase "Recordfile" $ parser parseRec playFileOutput $ Right "File_001"
    , testCase "Status playing" $ parser parseRecordStatus statusRecordingOutput $ Right statusRecordingResult
    , testCase "Status idle" $ parser parseRecordStatus statusRecordIdleOutput $ Right RecordIdle
    ]

--------------------------------------------------------------------------------
-- TYPE Parser
--------------------------------------------------------------------------------
typeOutput :: ByteString
typeOutput = "Labsat Wideband\r\nSerial 57082 \r\nFirmware 1.0.260\r\nFPGA 33\r\nIP 10.1.22.44\r\nBattery not connected\r\nTCXO-0x7b7f\r\n\r\r\n\r\r\nLABSAT_V3 >"

typeResult :: Info
typeResult = Info ["Labsat Wideband","Serial 57082 ","Firmware 1.0.260","FPGA 33","IP 10.1.22.44","Battery not connected","TCXO-0x7b7f"]

testType :: TestTree
testType =
  testGroup "Test type parser"
    [ testCase "Type command" $ parser parseInfo typeOutput $ Right typeResult
    ]

--------------------------------------------------------------------------------
-- MON Parsers
--------------------------------------------------------------------------------

monSatOutput :: ByteString
monSatOutput = "GPS 9\r\r\n05,42,07,52,08,43,09,52,16,35,23,47,27,43,28,42,30,49\r\r\n\r\r\nGLO 10\r\r\n01,50,02,52,03,36,08,37,10,36,11,50,12,50,13,32,20,31,21,40\r\r\n\r\r\nBDS 0\r\r\n\r\r\nGAL 4\r\r\n01,44,04,42,19,44,20,36\r\r\n\r\r\nLABSAT_V3 >"

monSatResult :: [ConstellationCNO]
monSatResult = [ConstellationCNO GPS "9" [SatelliteCNO "05" "42",SatelliteCNO "07" "52",SatelliteCNO "08" "43",SatelliteCNO "09" "52",SatelliteCNO "16" "35",SatelliteCNO "23" "47",SatelliteCNO "27" "43",SatelliteCNO "28" "42",SatelliteCNO "30" "49"],ConstellationCNO GLO "10" [SatelliteCNO "01" "50",SatelliteCNO "02" "52",SatelliteCNO "03" "36",SatelliteCNO "08" "37",SatelliteCNO "10" "36",SatelliteCNO "11" "50",SatelliteCNO "12" "50",SatelliteCNO "13" "32",SatelliteCNO "20" "31",SatelliteCNO "21" "40"],ConstellationCNO BDS "0" [],ConstellationCNO GAL "4" [SatelliteCNO "01" "44",SatelliteCNO "04" "42",SatelliteCNO "19" "44",SatelliteCNO "20" "36"]]

monLocOutput :: ByteString
monLocOutput = "123456.00,-5.800000,M,1234.12345,N,54321.54321,W\r\r\n\r\r\nLABSAT_V3 >"

monLocResult :: Location
monLocResult = Location {_time = 123456.0, _height = (-5.8,"M"), _lattitude = (1234.12345,"N"), _longitude = (54321.54321,"W")}

testMon :: TestTree
testMon =
  testGroup "Test MON parsers"
    [ testCase "Test MON:SAT" $ parser parseMonSat monSatOutput $ Right monSatResult
    , testCase "Test MON:LOC" $ parser parseMonLoc monLocOutput $ Right monLocResult
    ]

--------------------------------------------------------------------------------
-- ATTN Parsers
--------------------------------------------------------------------------------

attnOutput1 :: ByteString
attnOutput1 = "OK:ATTN:CH1:0 \r\r\nOK:ATTN:CH2:0 \r\r\nOK:ATTN:CH3:0 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

attnOutput2 :: ByteString
attnOutput2 = "OK:ATTN:CH1:10 \r\r\nOK:ATTN:CH3:10 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

attnResult1 :: AttnConf
attnResult1 = AttnConf {_acAttnAll = Nothing, _acAttnCh1 = Just 0, _acAttnCh2 = Just 0, _acAttnCh3 = Just 0}

attnResult2 :: AttnConf
attnResult2 = AttnConf {_acAttnAll = Nothing, _acAttnCh1 = Just 10, _acAttnCh2 = Nothing, _acAttnCh3 = Just 10}

testAttn :: TestTree
testAttn =
  testGroup "Test ATTN parsers"
    [ testCase "Test ATTN:0" $ parser parseAttn attnOutput1 $ Right attnResult1
    , testCase "Test ATTN:CH1:10:CH3:10" $ parser parseAttn attnOutput2 $ Right attnResult2
    ]

--------------------------------------------------------------------------------
-- CONF Parsers
--------------------------------------------------------------------------------

canBaudOutput :: ByteString
canBaudOutput = "baud value is 500000.000000 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

canBaudResult :: Double
canBaudResult = 500000.0

consFreqOutput :: ByteString
consFreqOutput = "TELNET_CONF \r\r\n QUA-1, BW-10, Available ch(1) 1575420000, 1207140014, 1268520019 \r\r\nOK\r\r\n\r\r\nLABSAT_V3 >"

consFreqResult :: ConstellationFreqConf
consFreqResult = ConstellationFreqConf {
                  _cfcQuantization = QUA1,
                  _cfcBandwidth = BW_10,
                  _cfcFrequencies = [1.57542e9,1.207140014e9,1.268520019e9]}

testConf :: TestTree
testConf =
  testGroup "Test CONF parsers"
  [ testCase "Test CONF:SETUP:CAN:CH1:BAUD:500000" $ parser parseCANBaud canBaudOutput $ Right canBaudResult
  , testCase "Test CONF:CONS:QUA:1:BW:10:FREQ:1575420000" $ parser parseConsFreq consFreqOutput $ Right consFreqResult
  ]

tests :: TestTree
tests =
  testGroup "LabSat parser tests"
    [ testHelp
    , testMedia
    , testPlay
    , testRecord
    , testType
    , testMon
    , testAttn
    , testConf
    ]
