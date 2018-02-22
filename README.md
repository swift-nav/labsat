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


## Development

`labsat` has a shakefile/makefile to provide convience around building and testing:

    # build the project's libraries, executables, and tests
    $ ./Shakefile.hs build-tests-error

    # test the project
    $ ./Shakefile.hs tests-error

    # start an interpreter with the project's libraries, executables, and tests loaded
    $ ./Shakefile.hs ghci-tests

    # install the project's executables
    $ ./Shakefile.hs install

    # clean the project
    $ ./Shakefile.hs clean

    # lint the project source code
    $ ./Shakefile.hs lint


## Dependencies

To build, install, run, and test `labsat`, the following dependencies may be required:

+ [stack][stack]


## LICENSE

Copyright © 2018 Swift Navigation

Distributed under MIT License. See [LICENSE](LICENSE)

[stack]:       https://docs.haskellstack.org/en/stable/README/#how-to-install
[travis]:      https://travis-ci.com/swift-nav/labsat
[travis-img]:  https://img.shields.io/travis/swift-nav/labsat/master.svg?style=flat
