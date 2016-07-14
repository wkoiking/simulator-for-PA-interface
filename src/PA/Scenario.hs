module PA.Scenario where

import PA.Data
import Data.Word (Word8)


type Scenario = [(MessageATS2PA, Int)]

scenario1
    :: StationCode -- ^ Station to be displayed
    -> StationCode -- ^ Destination station for row 1
    -> StationCode -- ^ Destination station for row 2
    -> StationCode -- ^ Destination station for row 3
    -> PlatformNumber -- ^ Platform Number
    -> RollingStockProfile -- ^ Type of train
    -> Word8 -- ^ Hour of first train departure
    -> Scenario
-- ^ This scenario is for Departure Platform Message.
scenario1 stCode dst1 dst2 dst3 pl profile hh = replicate 5 (nextThreeDepartures, 30)
 where nextThreeDepartures = NextThreeDeparture
           stCode
           [TrainInfo profile pl hh 00 00 dst1
           ,TrainInfo profile pl hh 10 35 dst2
           ,TrainInfo profile pl hh 25 55 dst3
           ]

scenario2
    :: StationCode -- ^ Station to be displayed
    -> StationCode -- ^ Destination station for train 1
    -> StationCode -- ^ Destination station for train 2
    -> PlatformNumber -- ^ Platform Number
    -> RollingStockProfile -- ^ Type of train
    -> Word8 -- ^ Hour of train departure
    -> Scenario
-- ^ This scenario is for revenue train at KJMD.
scenario2 stCode dst1 dst2 pl profile hh = concat [beforeArrival, atDwell, afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform stCode (NextEstimatedTrain trainAarr 30), 30)
       atDwell = [(DeparturePlatform stCode  trainAdep, 30)]
       afterArrival = replicate 4 $ (ArrivalPlatform stCode  (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo profile pl hh 10 00 dst1
       trainAdep = TrainInfo profile pl hh 10 30 dst1
       trainBarr = TrainInfo profile pl hh 25 00 dst2

scenario3
    :: StationCode -- ^ Station to be displayed
    -> StationCode -- ^ Destination station
    -> PlatformNumber -- ^ Platform Number
    -> RollingStockProfile -- ^ Type of train
    -> Word8 -- ^ Hour of train departure
    -> Scenario
-- ^ This scenario is for non-stopping train at KJMD.
scenario3 stCode dst pl profile hh = concat [beforeArrival, afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform stCode  NonStopping, 30)
       afterArrival = replicate 4 $ (ArrivalPlatform stCode  (NextEstimatedTrain trainBarr 30), 30)
       trainBarr = TrainInfo profile pl hh 55 00 dst

scenario4
    :: StationCode
    -> StationCode -- ^ Destination station for train 1
    -> StationCode -- ^ Destination station for train 2
    -> PlatformNumber -- ^ Platform Number
    -> RollingStockProfile -- ^ Type of train
    -> Word8 -- ^ Hour of train departure
    -> Scenario
-- ^ This scenario is for non-revenue train at KJMD.
scenario4 stCode dst1 dst2 pl profile hh = concat [beforeArrival, [clearCue], afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform stCode NotInService, 30)
       clearCue = (ClearDisplay stCode pl, 40)
       afterArrival = replicate 4 $ (ArrivalPlatform stCode (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo profile pl hh 24 00 dst1
       trainBarr = TrainInfo profile pl hh 39 00 dst2

scenario5
    :: StationCode -- ^ Station to be displayed
    -> StationCode -- ^ Destination station
    -> PlatformNumber -- ^ Platform Number
    -> RollingStockProfile -- ^ Type of train
    -> Word8 -- ^ Hour of train departure
    -> Scenario
-- ^ This scenario is for terminated train at BTGD.
scenario5 stCode dst pl profile hh = concat [beforeArrival, atDwell, afterArrival] 
 where beforeArrival = replicate 3 $ (ArrivalPlatform stCode Terminated, 30)
       atDwell = [(DeparturePlatform stCode trainAdep, 30)]
       afterArrival = replicate 3 $ (ArrivalPlatform stCode Terminated, 30)
       trainAdep = TrainInfo profile pl hh 43 30 dst
