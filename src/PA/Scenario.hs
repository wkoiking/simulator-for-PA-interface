module PA.Scenario where

import PA.Data

type Scenario = [(MessageATS2PA, Int)]

scenario1 :: Scenario
-- ^ This scenario is for Departure Platform Message
scenario1 = replicate 5 (nextThreeDepartures, 30)
 where nextThreeDepartures = NextThreeDeparture
           NUEE
           [TrainInfo SixCar PL2 10 55 00 BTGD
           ,TrainInfo SixCar PL2 11 05 35 BTGD
           ,TrainInfo SixCar PL2 11 20 55 BTGD
           ]

scenario2 :: Scenario
-- ^ This scenario is for revenue train at KJMD
scenario2 = concat [beforeArrival, atDwell, afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainAarr 30), 30)
       atDwell = [(DeparturePlatform KJMD trainAdep, 30)]
       afterArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo SixCar PL2 15 00 00 BTGD
       trainAdep = TrainInfo SixCar PL2 15 00 30 BTGD
       trainBarr = TrainInfo SixCar PL2 15 15 00 BTGD

scenario3 :: Scenario
-- ^ This scenario is for non-stopping train at KJMD
scenario3 = concat [beforeArrival, afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform KJMD NonStopping, 30)
       afterArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo SixCar PL2 15 00 00 BTGD
       trainBarr = TrainInfo SixCar PL2 15 15 00 BTGD

scenario4 :: Scenario
-- ^ This scenario is for non-revenue train at KJMD
scenario4 = concat [beforeArrival, [clearCue], afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform KJMD NotInService, 30)
       clearCue = (ClearDisplay BTGD PL2, 40)
       afterArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo SixCar PL2 15 00 00 BTGD
       trainBarr = TrainInfo SixCar PL2 15 15 00 BTGD

scenario5 :: Scenario
-- ^ This scenario is for terminated train at BTGD
scenario5 = concat [beforeArrival, atDwell, afterArrival] 
 where beforeArrival = replicate 3 $ (ArrivalPlatform BTGD Terminated, 30)
       atDwell = [(DeparturePlatform BTGD trainAdep, 30)]
       afterArrival = replicate 3 $ (ArrivalPlatform BTGD Terminated, 30)
       trainAdep = TrainInfo SixCar PL2 15 00 30 JPW
