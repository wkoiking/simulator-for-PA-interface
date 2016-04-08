# How to Install

1. Check that you are connected to Internet.

2. Install stack from:

    <https://www.stackage.org/stack/windows-i386-installer>

3. Clone this repository by such as:

    ```
    > git clone https://github.com/wkoiking/simulator-for-PA-interface.git
    ```

4. Install the simulator with stack:

    ```
    > cd simulator-for-interface
    > stack setup
    > stack build
    ```

# How to Use

1. Open ghci at this repository:

    ```
    > cd path-to-this-repository
    > stack ghci
    ```

2. Run simulator for ATS server:

    ```
    GHCi> serverATS "3000" scenario1
    ```

3. Start other terminal and run the simulator for PA server in the similar manner:

    ```
    > cd path-to-this-repository
    > stack ghci
    GHCi> serverPA "localhost" "3000"
    ```

Also you can manually send the Message by such as:

```
GHCi> h <- getHandle "localhost" "3000"        // Get a handle by specifying address and port number of PA server
GHCi> sendMsgATS2PA h OperationalATSSession    // Send Operational ATS Session Message
GHCi> sendMsgATS2PA h (DeparturePlatform IWNR (TrainInfo SixCar PL2 15 00 00 BTGD)) // Send Departure Platform Message
GHCi> sendMsgATS2PA h (ClearDisplay BTGD PL2)  // Send ClearDisplay Message
```

Refer to `~/doc/index.html` for the detail of the API.

# ToDo

* 例外の挙動を知るために適当にテストしてみる
* Hexでパケットの表示をしてあげる方法も用意してあげる -> decode, showHexを使う

* SP6仕様書を書き換えたい
    * なぜなら、現在の仕様だとNonStoppingやNonRevenueの列車が来る場合は次のRevenueの時刻が表示されない仕様だから
    * 本当はArrival TrigerとTimeToArrivalにするのが良い
    * Train stopping scheduleの削除したい
    * Dwell Timeの削除したい

# Memo

* NotInService       -> 止まるけど次のtripがNon-revenue
* Terminated         -> 今のmissionがそこで終わる（向きを変える）かつ次のtripがRevenue
* Non-Stopping       -> 止まらない
* NextEstimatedTrain -> 次のtripがRevenueかつ向きを変えない
