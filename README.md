# LabSat TCP Wrapper

[![Build status][travis-img]][travis]

This package provides a wrapper around the LabSat3 Wideband TCP interface. It is tested against
LabSat3 Wideband Firmware 1.0.260 FPGA 33.


## Usage

Run the `help` command:
```
λ: runCtx $ runStatsCtx $ testCommand <ip address> <port> $ help
HelpCommands ["HELP","TYPE","FIND","MON","PLAY","REC","ATTN","CONF","MEDIA","MUTE"]
```

Run the `info` command:
```
λ: runCtx $ runStatsCtx $ testCommand <ip address> <port> $ info
Info ["Labsat Wideband","Serial 57082 ","Firmware 1.0.260","FPGA 33","IP 10.1.22.44","Battery not connected","TCXO-0x7b7f"]
```

Run the play command, check status, stop playing, check status:
```
λ: runCtx $ runStatsCtx $ testCommand  <ip address> <port> $ play "File_001"
"File_001"

λ: runCtx $ runStatsCtx $ testCommand  <ip address> <port> $ playStatus
Playing "File_001" "00:00:03"

λ: runCtx $ runStatsCtx $ testCommand  <ip address> <port> $ playStop
"OK"

λ: runCtx $ runStatsCtx $ testCommand  <ip address> <port> $ playStatus
PlayIdle
```

## Dependencies

To build, install, run, and test `labsat`, the following dependencies may be required:

+ [stack][stack]


[stack]:       https://docs.haskellstack.org/en/stable/README/#how-to-install
[travis]:      https://travis-ci.com/swift-nav/labsat
[travis-img]:  https://img.shields.io/travis/swift-nav/labsat/master.svg?style=flat
