module PA where

-- ToDo
-- ��O�̋�����m�邽�߂ɓK���Ƀe�X�g���Ă݂�
-- Hex�Ńp�P�b�g�̕\�������Ă�������@���p�ӂ��Ă����� -> decode, showHex���g��

-- NotInService       -> �~�܂邯�ǎ���trip��Non-revenue
-- Terminated         -> ����mission�������ŏI���i������ς���j������trip��Revenue
-- Non-Stopping       -> �~�܂�Ȃ�
-- NextEstimatedTrain -> ����trip��Revenue��������ς��Ȃ�

-- SP6�d�l����������������
--   �Ȃ��Ȃ�A���݂̎d�l����NonStopping��NonRevenue�̗�Ԃ�����ꍇ�͎���Revenue�̎������\������Ȃ��d�l������
--   �{����Arrival Triger��TimeToArrival�ɂ���̂��ǂ�
--   Train stopping schedule�̍폜������
--   Dwell Time�̍폜������

import PA.Data
import Data.Serialize
import qualified Data.ByteString as B

-- base
import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (forever, replicateM, replicateM_)
import Control.Monad.IO.Class (liftIO)
import Numeric (showHex)
import Control.Exception (bracket)

-- conduit
import Conduit (printC, sourceHandle, sinkHandle, runResourceT, addCleanup)
import qualified Data.Conduit.List as CL
import Data.Conduit (($$), (=$), ($=), await, (=$=), yield, bracketP)
import Data.Conduit.Cereal

-- network
import Network.Socket
import Network.BSD


port = "3000"
hostname = "localhost"

main :: IO ()
main = undefined

wait :: Int -> IO ()
wait s = threadDelay $ fromIntegral $ s * 1000000

serverATS :: ServiceName -> [(MessageATS2PA, Int)] -> IO ()
serverATS port scenario = withSocketsDo $
    do addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
       let serveraddr = head addrinfos
       bracket (socket (addrFamily serveraddr) Stream defaultProtocol) sClose  $ \sock -> do
           bind sock (addrAddress serveraddr)
           listen sock 1
           putStrLn "Listening"
           loop sock
 where loop :: Socket -> IO ()
       loop mastersock = do
           (connsock, clientaddr) <- accept mastersock
           putStrLn "TCP/IP Connection established"
           let source h = sourceHandle h $= conduitGet2 get
               conduit1 h = do
                   mmsg <- await
                   case mmsg of
                       Nothing -> liftIO $ putStrLn "Nothing left, exiting"
                       Just ConnectionRequest -> do
                           liftIO $ putStrLn "Connection Request Received"
                           mapM_ yield scenario
               conduit2 h = do
                   mmsg <- await
                   case mmsg of
                       Nothing -> liftIO $ putStrLn "Scenario is Over"
                       Just (msg, delay) -> do
                           yield msg
                           liftIO $ wait delay
               sink h = conduitPut put =$ sinkHandle h
           bracket (socketToHandle connsock ReadWriteMode)
                   (\h -> do
                       putStrLn "TCP/IP Connection Lost .. Closing handle"
                       hClose h)
                   (\h -> source h $$ conduit1 h $= conduit2 h $= sink h)
           loop mastersock

serverPA :: HostName -> ServiceName -> IO ()
serverPA hostname port = bracket (getHandle hostname port) hClose (\h -> do
    sendMsgPA2ATS h ConnectionRequest
    sourceHandle h $$ conduitGet2 (get :: Get MessageATS2PA) =$ printC
    serverPA hostname port)

getHandle :: HostName -> ServiceName -> IO Handle
getHandle hostname port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    return h

closeHandle :: IO ()
closeHandle = undefined

sendMsgPA2ATS :: Handle -> MessagePA2ATS -> IO ()
sendMsgPA2ATS h msg = withSocketsDo $ do
    B.hPutStr h (encode $ msg)
    hFlush h

sendMsgATS2PA :: Handle -> MessagePA2ATS -> IO ()
sendMsgATS2PA h msg = withSocketsDo $ do
    B.hPutStr h (encode $ msg)
    hFlush h

scenarioNextThreeDepartures :: [(MessageATS2PA, Int)]
scenarioNextThreeDepartures = replicate 5 (nextThreeDepartures, 30)
 where nextThreeDepartures = NextThreeDeparture
           NUEE
           [TrainInfo SixCar PL2 10 55 00 BTGD
           ,TrainInfo SixCar PL2 11 05 35 BTGD
           ,TrainInfo SixCar PL2 11 20 55 BTGD
           ]

scenarioServiceTrain :: [(MessageATS2PA, Int)]
scenarioServiceTrain = concat [beforeArrival, atDwell, afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainAarr 30), 30)
       atDwell = [(DeparturePlatform KJMD trainAdep, 30)]
       afterArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo SixCar PL2 15 00 00 BTGD
       trainAdep = TrainInfo SixCar PL2 15 00 30 BTGD
       trainBarr = TrainInfo SixCar PL2 15 15 00 BTGD

scenarioNonStoppingTrain :: [(MessageATS2PA, Int)]
scenarioNonStoppingTrain = concat [beforeArrival, afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform KJMD NonStopping, 30)
       afterArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo SixCar PL2 15 00 00 BTGD
       trainBarr = TrainInfo SixCar PL2 15 15 00 BTGD

scenarioNotInServiceTrain :: [(MessageATS2PA, Int)]
scenarioNotInServiceTrain = concat [beforeArrival, [clearCue], afterArrival] 
 where beforeArrival = replicate 4 $ (ArrivalPlatform KJMD NotInService, 30)
       clearCue = (ClearDisplay BTGD PL2, 40)
       afterArrival = replicate 4 $ (ArrivalPlatform KJMD (NextEstimatedTrain trainBarr 30), 30)
       trainAarr = TrainInfo SixCar PL2 15 00 00 BTGD
       trainBarr = TrainInfo SixCar PL2 15 15 00 BTGD

scenarioTerminatedTrain :: [(MessageATS2PA, Int)]
scenarioTerminatedTrain = concat [beforeArrival, atDwell, afterArrival] 
 where beforeArrival = replicate 3 $ (ArrivalPlatform BTGD Terminated, 30)
       atDwell = [(DeparturePlatform BTGD trainAdep, 30)]
       afterArrival = replicate 3 $ (ArrivalPlatform BTGD Terminated, 30)
       trainAdep = TrainInfo SixCar PL2 15 00 30 JPW

scenarioTest :: [(MessageATS2PA, Int)]
scenarioTest = concat [deps, n3deps] 
 where deps = replicate 5 $ (DeparturePlatform BTGD trainAdep, 1)
       n3deps = replicate 5 $ (next3deps, 1)
       trainAdep = TrainInfo SixCar PL2 15 00 30 JPW
       next3deps = NextThreeDeparture
           NUEE
           [TrainInfo SixCar PL2 10 55 00 BTGD
           ,TrainInfo SixCar PL2 11 05 35 BTGD
           ,TrainInfo SixCar PL2 11 20 55 BTGD
           ]
