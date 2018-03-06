{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LabSat telnet interface
--
import Labsat
import Options.Generic
import Preamble

-- | Args
--
-- Program arguments.
--
data Args = Args
  {
    ip          :: Text
    -- ^ Host IP.
  , port        :: Int
    -- ^ Host port.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run Labsat Telnet Wrapper
--
main :: IO ()
main = do
  args <- getRecord "Labsat Telnet Wrapper"
  labsatMain
    (ip args)
    (port args)
