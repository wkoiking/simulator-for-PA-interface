# How to Install

1. Check that you are connected to Internet.

2. Install stack from:

<https://www.stackage.org/stack/windows-i386-installer>

3. Clone this repository by such as:

    > git clone https://github.com/wkoiking/simulator-for-PA-interface.git

4. Install with stack:

    > cd simulator-for-interface
    > stack build

# How to Use

1. Open ghci:

    > stack ghci

2. Run simulator for ATS server:

    GHCi> serverATS "3000" scenario1

3. Start other terminal and run the simulator for PA server in the similar manner:

    GHCi> serverPA "localhost" "3000"

Refer to the following for the detail of the other commands.

<./doc/index.html>
