{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings   #-}

module Labsat.Parser where


import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char, decimal, double, isDigit_w8, isEndOfLine, scientific, signed)
import           Labsat.Types
import           Preamble hiding (takeWhile)


--------------------------------------------------------------------------------
-- Helper Parsers
--------------------------------------------------------------------------------

prompt :: Parser ByteString
prompt = string "LABSAT_V3 >"

newline :: Parser ByteString
newline = string "\r\r\n"

ok :: Parser ByteString
ok = string "OK"

err :: Parser ByteString
err = string "ERR"

colon :: Word8 -> Bool
colon = (== 58)

isETX :: Word8 -> Bool
isETX = (==03)

etx :: Parser Word8
etx = word8 03

isEscape :: Word8 -> Bool
isEscape = (== 27)

notNewline :: Word8 -> Bool
notNewline w = w /= 10 && w /= 13

parseNotNewline :: Parser Word8
parseNotNewline = satisfy notNewline

mediaDelim :: Parser ByteString
mediaDelim = string " \ESC[40G "

takeDigits :: Parser ByteString
takeDigits = takeWhile1 isDigit_w8

-- | Parse OK followed by prompt
--
okPrompt :: Parser ByteString
okPrompt = ok <* takeNewlines <* prompt

-- | Parse comma separated lists
--
commaSep :: Parser ByteString -> Parser [ByteString]
commaSep = flip sepBy' (char ',')

-- | take [\r\n]+
--
takeNewlines :: Parser ()
takeNewlines = satisfy isEndOfLine *> skipWhile isEndOfLine

-- | Parse everything up to [\r\n]+
--
parseLabsatLine :: Parser ByteString
parseLabsatLine = takeWhile1 (not . isEndOfLine) <* takeNewlines

-- | Parse lines until next Labsat prompt
--
parseLabsatLines :: Parser [ByteString]
parseLabsatLines = manyTill parseLabsatLine prompt

-- | Parse everything until next prompt (debug)
--
parseUntilPrompt :: Parser ByteString
parseUntilPrompt = BS.pack <$> manyTill anyWord8 prompt

-- | Parse everything up to and including next prompt (debug)
--
parseThroughPrompt :: Parser ByteString
parseThroughPrompt = do
  x <- parseUntilPrompt
  return $ x <> "LABSAT_V3 >"

-- | Parse echoed command followed by newline(s)
--
parseCommandAck :: ByteString -> Parser ByteString
parseCommandAck cmd = string cmd <* takeNewlines

-- | Parse ANSI Color Escape Sequence
--
parseColorSeq :: Parser ByteString
parseColorSeq = do
  esc <- string "\ESC["
  n <- takeDigits
  m <- string "m"
  return $ BS.concat [esc, n, m]

-- | Parse Labsat header
--
takeLabsatHeader :: Parser ByteString
takeLabsatHeader = takeWhile1 (not . isETX) <* etx

-- | Parse IP address
--
-- | TODO make sure its a valid IP address
parseIP :: Parser ByteString
parseIP = do
  octet1 <- takeDigits
  _ <- char '.'
  octet2 <- takeDigits
  _ <- char '.'
  octet3 <- takeDigits
  _ <- char '.'
  octet4 <- takeDigits
  return $ intercalate "." [octet1, octet2, octet3, octet4]

-- | Parse Duration HH:MM:SS
--
parseDuration :: Parser ByteString
parseDuration = do
  hh <- takeWhile isDigit_w8
  _ <- char ':'
  mm <- takeWhile isDigit_w8
  _ <- char ':'
  ss <- takeWhile isDigit_w8
  return $ intercalate ":" [hh, mm, ss]

-- | Parse In Use Error
--
parseInUse :: Parser ByteString
parseInUse = takeLabsatHeader *> string "in use with " *> parseIP <* takeNewlines

-- | Parse First Labsat Message
--
parseFirstLabsatMsg :: Parser ByteString
parseFirstLabsatMsg = takeLabsatHeader <* takeNewlines *> prompt

--------------------------------------------------------------------------------
-- HELP Parsers
--------------------------------------------------------------------------------

parseHelp :: Parser HelpCommands
parseHelp =
  HelpCommands <$> ("Current commands are : " <* takeNewlines *> parseLabsatLines)

--------------------------------------------------------------------------------
-- MEDIA Parsers
--------------------------------------------------------------------------------

parseMediaFile :: Parser Media
parseMediaFile = File <$> (BS.pack <$> manyTill parseNotNewline mediaDelim) <*> parseDuration <* newline

parseMediaDir :: Parser Media
parseMediaDir = Dir <$> (BS.pack <$> manyTill parseNotNewline newline)

parseMedia :: Parser Media
parseMedia = parseMediaFile <|> parseMediaDir

parseMediaList :: Parser MediaList
parseMediaList = MediaList <$> manyTill parseMedia (newline *> prompt)

parseMediaChdir :: Parser ByteString
parseMediaChdir = takeWhile notNewline <* takeNewlines <* ok <* takeNewlines <* prompt

--------------------------------------------------------------------------------
-- PLAY Parsers
--------------------------------------------------------------------------------

parsePlayFile :: ByteString -> Parser ByteString
parsePlayFile = string

parsePlayIdle :: Parser PlayStatus
parsePlayIdle = do
  _ <- "PLAY:IDLE"
  return PlayIdle

parsePlaying :: Parser PlayStatus
parsePlaying =
  Playing <$> parseFile <*> parseDuration'
    where
      parseFile      = "PLAY:/mnt/sata/" *> takeWhile1 (not . colon)
      parseDuration' = ":DUR:" *> parseDuration

parsePlay :: ByteString -> Parser ByteString
parsePlay f = string f <* takeWhile notNewline <* takeNewlines <* prompt

parsePlayStatus :: Parser PlayStatus
parsePlayStatus = (parsePlayIdle <|> parsePlaying) <* takeNewlines <* prompt

--------------------------------------------------------------------------------
-- REC Parsers
--------------------------------------------------------------------------------
parseRec :: Parser ByteString
parseRec = takeWhile notNewline <* takeNewlines <* prompt

parseRecordIdle :: Parser RecordStatus
parseRecordIdle = do
  _ <- "REC:IDLE"
  return RecordIdle

parseRecording :: Parser RecordStatus
parseRecording =
  Recording <$> parseFile <*> parseDuration'
    where
      parseFile      = "REC:/mnt/sata/" *> takeWhile1 (not . colon)
      parseDuration' = ":DUR:" *> parseDuration

parseRecordStatus :: Parser RecordStatus
parseRecordStatus = (parseRecordIdle <|> parseRecording) <* takeNewlines <* prompt

--------------------------------------------------------------------------------
-- TYPE Parser
--------------------------------------------------------------------------------

parseInfo :: Parser Info
parseInfo = Info <$> parseLabsatLines

--------------------------------------------------------------------------------
-- MON Parsers
--------------------------------------------------------------------------------

-- | Constellation parser.
--
parseConstellation :: Parser Constellation
parseConstellation =
  (string "GPS" >> return GPS) <|>
  (string "GLO" >> return GLO) <|>
  (string "BDS" >> return BDS) <|>
  (string "GAL" >> return GAL)

-- | Satellite CNO parser.
--
parseSatelliteCNO :: Parser [SatelliteCNO]
parseSatelliteCNO = do
  res <- option [] (commaSep takeDigits <* takeNewlines)
  return $ uncurry SatelliteCNO <$>  extractSatelliteCNOPairs res
    where
      extractSatelliteCNOPairs :: [ByteString] -> [(ByteString, ByteString)]
      extractSatelliteCNOPairs [] = []
      extractSatelliteCNOPairs [_] = error "Corrupt Satellite CNO information"
      extractSatelliteCNOPairs (x:y:xs) = (x,y) : extractSatelliteCNOPairs xs

-- | Constellation CNO parser.
--
parseConstellationCNO :: Parser ConstellationCNO
parseConstellationCNO =
  ConstellationCNO               <$>
    parseConstellation <* " "    <*>
    takeDigits <* takeNewlines   <*>
    parseSatelliteCNO

-- | 'MON:LOC' parser
--
-- | TODO handle case when record/replay is turned off and ',0.000000,,,,,' is returned
--
parseMonLoc :: Parser Location
parseMonLoc = Location <$> double <* "," <*> parseLocation <* "," <*> parseLocation <* "," <*> parseLocation
  where
    parseLocation = (,) <$> double <* "," <*> parseDirection
    parseDirection = takeWhile1 (inClass "MNSEW")

-- | 'MON:SAT' parser
--
parseMonSat :: Parser [ConstellationCNO]
parseMonSat = manyTill parseConstellationCNO prompt

--------------------------------------------------------------------------------
-- ATTN Parsers
--------------------------------------------------------------------------------

-- | 'ATTN' parser
--
parseAttn :: Parser AttnConf
parseAttn =
  AttnConf <$>
    return Nothing <*>
    parseChannelAttn "CH1" <*>
    parseChannelAttn "CH2" <*>
    parseChannelAttn "CH3" <* ok <* takeNewlines <* prompt
    where
      parseChannelId ch = string $ "OK:ATTN:"++ch++":"
      parseChannelAttn ch = option Nothing (parseChannelId ch *> (Just <$> signed decimal) <* " " <* takeNewlines)

--------------------------------------------------------------------------------
-- CONF Parsers
--------------------------------------------------------------------------------

-- | 'CONF:SETUP:CAN:CHX:BAUD' parser
--
parseCANBaud :: Parser Double
parseCANBaud = "baud value is " *> double <* " " <* takeNewlines <* okPrompt


parseQuantization :: Parser Quantization
parseQuantization = (string "QUA-1" >> return QUA1) <|>
                    (string "QUA-2" >> return QUA2) <|>
                    (string "QUA-3" >> return QUA3)


parseBandwidth :: Parser Bandwidth
parseBandwidth = (string "BW-10" >> return BW_10) <|>
                 (string "BW-30" >> return BW_30) <|>
                 (string "BW-56" >> return BW_56)

-- | 'CONF:CONS' parsers
--
-- | WARNING: The output of CONF:CONS relies on a leading space in some cases
--
-- | TODO: cleanup
parseConsPreset :: Parser ConstellationPresetConf
parseConsPreset =
  ConstellationPresetConf <$> parseQua <*> parseBW <*> parsePresets <* takeNewlines <* okPrompt
    where parseQua   = parseQuantization <* ", "
          parseBW    = parseBandwidth <* ", "
          parsePresets = do
            _ <- string "Available ch(" <* takeDigits <* string ") "
            presets <- takeDigits `sepBy'` string ", " <* string " "
            return $ freqPresetLookup <$> presets

parseConsFreq :: Parser ConstellationFreqConf
parseConsFreq =
  ConstellationFreqConf <$> parseQua <*> parseBW <*> parseFreqs <* takeNewlines <* okPrompt
    where optHeader  = option "" ("TELNET_CONF " <* takeNewlines)
          parseQua   = optHeader *> " " *> parseQuantization <* ", "
          parseBW    = parseBandwidth <* ", "
          parseFreqs = do
            _ <- string "Available ch(" <* takeDigits <* string ") "
            scientific `sepBy'` string ", " <* string " "


