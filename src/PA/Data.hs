module PA.Data where

import Data.Serialize

-- bytes
-- import qualified Data.Bytes.Serial as S
-- import qualified Data.Bytes.Get as G
-- import qualified Data.Bytes.Put as P

-- bits
import qualified Data.Bits.Coded as B
import qualified Data.Bits.Coding as B

-- base
import Data.Word
import Data.Bits
import Control.Monad (replicateM_, replicateM)
import qualified Data.ByteString as BS (singleton, head)

data MessageATS2PA 
    = OperationalATSSession
    | NextThreeDeparture -- for revenue train
        StationCode
        [TrainInfo]
    | DeparturePlatform  -- for revenue train
        StationCode
        TrainInfo
    | ArrivalPlatform
        StationCode
        ArrivalTrigger
    | ClearDisplay
        StationCode
        PlatformNumber deriving (Show)

data MessagePA2ATS = ConnectionRequest deriving (Show)

data ArrivalTrigger
    = NonStopping
    | NotInService
    | Terminated
    | NextEstimatedTrain
        TrainInfo
        Word8 deriving (Show) -- dwell time (not used)

data TrainInfo
    = TrainInfo
        RollingStockProfile
        PlatformNumber
        Word8
        Word8
        Word8
        StationCode deriving (Show)

data RollingStockProfile
    = FourCar
    | SixCar
    | EightCar deriving (Show)

data PlatformNumber
    = PL1
    | PL2
    | PL3 deriving (Show)

data StationCode
    = JPW
    | DBMR
    | DSHP
    | PALM
    | SABR
    | IGDA
    | SKVR
    | VTVR
    | MIRK
    | RKPM
    | IIT
    | HKS
    | PSPK
    | CDLI
    | GKEI
    | NUEE
    | KJMD
    | OKNS
    | IWNR
    | JANR
    | OVA
    | JLA
    | KIKJ
    | OKBS
    | BTGD
    | KIKD
    | JSTB deriving (Show)

instance Serialize MessageATS2PA where
    put OperationalATSSession = do
        putWord8 2 -- SrcID
        putWord8 1 -- MessageID
    put (NextThreeDeparture stNum trInfos) = do
        putWord8 2 -- SrcID
        putWord8 2 -- MessageID
        put stNum
        putWord8 (fromIntegral (length trInfos) :: Word8)
        mapM_ (\x -> put x >> replicateM_ 5 (putWord8 0xFF)) trInfos
    put (DeparturePlatform stNum trInfo) = do
        putWord8 2 -- SrcID
        putWord8 3 -- MessageID
        put stNum
        put trInfo
        replicateM_ 5 (putWord8 0xFF)
    put (ArrivalPlatform stNum arrTriger) = do
        putWord8 2 -- SrcID
        putWord8 4 -- MessageID
        put stNum
        put arrTriger
    put (ClearDisplay stNum plNum) = do
        putWord8 2 -- SrcID
        putWord8 5 -- MessageID
        put stNum
        B.runEncode $ do
            B.putBitsFrom 3 (zeroBits :: Word8)
            B.encode plNum :: B.Coding PutM ()
    get = do
        2 <- getWord8
        msgID <- getWord8
        case msgID of
            1  -> return OperationalATSSession
            2  -> do
                stCode <- get
                departures <- getWord8
                let n = fromIntegral departures
                ts <- replicateM n $ do
                    t <- get
                    skip 5
                    return t
                skip ((3 - n) * 10)
                return $ NextThreeDeparture stCode ts
            3  -> do
                stCode <- get
                trInfo <- get
                skip 5
                return $ DeparturePlatform stCode trInfo
            4  -> do
                stCode <- get
                arr <- get
                return $ ArrivalPlatform stCode arr
            5  -> do
                stCode <- get
                plNum <- B.runDecode $ do
                    B.getBitsFrom 3 (zeroBits :: Word8)
                    B.decode :: B.Coding Get PlatformNumber
                return $ ClearDisplay stCode plNum
            _  -> fail "oh my god!"

instance Serialize MessagePA2ATS where
    put ConnectionRequest = do
        putWord8 2 -- SrcID
        putWord8 11 -- MessageID
    get = do
        2 <- getWord8
        msgID <- getWord8
        case msgID of
            11 -> return ConnectionRequest
            _  -> fail "oh my god!"

instance Serialize ArrivalTrigger where
    put NonStopping = do
        putWord8 0 -- Non Stopping
        replicateM_ 6 (putWord8 zeroBits)
    put NotInService = do
        putWord8 1 -- Not In Service
        replicateM_ 6 (putWord8 zeroBits)
    put Terminated = do
        putWord8 2 -- Terminated
        replicateM_ 6 (putWord8 zeroBits)
    put (NextEstimatedTrain trInfo dwell) = do
        putWord8 3 -- Next Estimated Train
        put trInfo
        putWord8 dwell
    get = do
        arrivalTrigger <- getWord8
        case arrivalTrigger of
            0 -> do
                replicateM_ 6 (skip 1)
                return NonStopping
            1 -> do
                replicateM_ 6 (skip 1)
                return NotInService
            2 -> do
                replicateM_ 6 (skip 1)
                return Terminated
            3 -> do
                trInfo <- get
                dwell <- getWord8
                return $ NextEstimatedTrain trInfo dwell

instance Serialize TrainInfo where
    put (TrainInfo profile pl hh mm ss dst) = do
        B.runEncode $ do
            B.encode profile :: B.Coding PutM ()
            B.encode pl :: B.Coding PutM ()
        putWord8 hh
        putWord8 mm
        putWord8 ss
        put dst
    get = do
        (profile, pl) <- B.runDecode $ do
            profile <- B.decode :: B.Coding Get RollingStockProfile
            pl <- B.decode :: B.Coding Get PlatformNumber
            return (profile, pl)
        hh <- getWord8
        mm <- getWord8
        ss <- getWord8
        sc <- get
        return $ TrainInfo profile pl hh mm ss sc

instance B.Coded PlatformNumber where
    encode PL1 = B.putBitsFrom 3 (1 :: Word8)
    encode PL2 = B.putBitsFrom 3 (2 :: Word8)
    encode PL3 = B.putBitsFrom 3 (3 :: Word8)
    decode = do
        w <- B.getBitsFrom 3 (zeroBits :: Word8)
        case w of
           1 -> return PL1
           2 -> return PL2
           3 -> return PL3
           _ -> fail "oh my god!"

instance B.Coded RollingStockProfile where
    encode FourCar = B.putBitsFrom 3 (1 :: Word8)
    encode SixCar = B.putBitsFrom 3 (2 :: Word8)
    encode EightCar = B.putBitsFrom 3 (2 :: Word8)
    decode = do
        w <- B.getBitsFrom 3 (zeroBits :: Word8)
        case w of
           1 -> return FourCar
           2 -> return SixCar
           3 -> return EightCar
           _ -> fail "oh my god!"

instance Serialize StationCode where
    put = putWord8 . enc
     where enc :: StationCode -> Word8
           enc JPW  = 31
           enc DBMR = 32
           enc DSHP = 33
           enc PALM = 34
           enc SABR = 35
           enc IGDA = 36
           enc SKVR = 37
           enc VTVR = 38
           enc MIRK = 39
           enc RKPM = 40
           enc IIT  = 41
           enc HKS  = 42
           enc PSPK = 43
           enc CDLI = 44
           enc GKEI = 45
           enc NUEE = 46
           enc KJMD = 47
           enc OKNS = 48
           enc IWNR = 49
           enc JANR = 50
           enc OVA  = 51
           enc JLA  = 52
           enc KIKJ = 53
           enc OKBS = 54
           enc BTGD = 55
           enc KIKD = 56
           enc JSTB = 57
    get = do
       w <- getWord8
       return $ dec w
     where dec :: Word8 -> StationCode
           dec 31 = JPW
           dec 32 = DBMR
           dec 33 = DSHP
           dec 34 = PALM
           dec 35 = SABR
           dec 36 = IGDA
           dec 37 = SKVR
           dec 38 = VTVR
           dec 39 = MIRK
           dec 40 = RKPM
           dec 41 = IIT
           dec 42 = HKS
           dec 43 = PSPK
           dec 44 = CDLI
           dec 45 = GKEI
           dec 46 = NUEE
           dec 47 = KJMD
           dec 48 = OKNS
           dec 49 = IWNR
           dec 50 = JANR
           dec 51 = OVA
           dec 52 = JLA
           dec 53 = KIKJ
           dec 54 = OKBS
           dec 55 = BTGD
           dec 56 = KIKD
           dec 57 = JSTB
