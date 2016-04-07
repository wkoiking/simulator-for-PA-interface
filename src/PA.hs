-- | Main module for interface simulation of ATS and PA

module PA where

import PA.Data
import PA.Scenario
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

-- | Start simulator for ATS server with scenario.
-- Currently following scenarios are available 'scenario1', 'scenario2', 'scenario3', 'scenario4' and 'scenario5'.
serverATS
    :: ServiceName -- ^ Port number
    -> Scenario -- ^ Scenario
    -> IO ()
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

-- | Start simulator for PA server.
serverPA
    :: HostName -- ^ IP Address of ATS server to connect
    -> ServiceName -- ^ Port number of ATS server to connect
    -> IO ()
serverPA hostname port = bracket (getHandle hostname port) hClose (\h -> do
    sendMsgPA2ATS h ConnectionRequest
    sourceHandle h $$ conduitGet2 (get :: Get MessageATS2PA) =$ printC
    serverPA hostname port)

-- | Get a handle for TCP/IP communicatoin for manually sending the message with 'sendMsgPA2ATS' or 'sendMsgATS2PA'.
getHandle
    :: HostName -- ^ IP Address of the target server to communicate
    -> ServiceName -- ^ Port number of the target server to communicate
    -> IO Handle
getHandle hostname port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    return h

-- | To close handle.
closeHandle :: Handle -> IO ()
closeHandle = hClose

-- | To manually send the PA to ATS message
sendMsgPA2ATS
    :: Handle -- ^ Handle of the TCP/IP communication
    -> MessagePA2ATS -- ^ Message to be sent
    -> IO ()
sendMsgPA2ATS h msg = withSocketsDo $ do
    B.hPutStr h (encode $ msg)
    hFlush h

-- | To manually send the ATS to PA message.
sendMsgATS2PA
    :: Handle -- ^ Handle of the TCP/IP communication
    -> MessagePA2ATS -- ^ Message to be sent
    -> IO ()
sendMsgATS2PA h msg = withSocketsDo $ do
    B.hPutStr h (encode $ msg)
    hFlush h

-- | To insert delay for the thread.
wait
    :: Int -- ^ seconds
    -> IO ()
wait s = threadDelay $ fromIntegral $ s * 1000000
