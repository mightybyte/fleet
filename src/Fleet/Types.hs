{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Fleet.Types where

import           Control.Arrow
import           Control.Lens
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.HashMap.Lazy as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

newtype LocalPath = LocalPath { unLocalPath :: String }
  deriving (Eq,Ord,Show,Read,IsString)

newtype RemotePath = RemotePath { unRemotePath :: String }
  deriving (Eq,Ord,Show,Read,IsString)

data Tunnel = Tunnel
    { _tunnelHost              :: String
    , _tunnelUser              :: String
    , _tunnelIdentityFile      :: Maybe String
    , _tunnelStartingLocalPort :: Int
    } deriving (Eq,Ord,Show,Read)

makeLenses ''Tunnel

type MachineName = String

data Machine = Machine
    { _machineName             :: MachineName
    , _machineHost             :: String
    , _machineLocalForwardPort :: Maybe Int

    -- Not supported yet.  Need to add config file support
    , _machineUser             :: Maybe String
    , _machineIdentityFile     :: Maybe String
    } deriving (Eq,Ord,Show,Read)

makeLenses ''Machine

data Cluster = Cluster
    { _clusterTunnel     :: Maybe Tunnel
    , _clusterMachineMap :: Map MachineName Machine
    } deriving (Eq,Ord,Show,Read)

makeLenses ''Cluster

clusterMachines :: Cluster -> [Machine]
clusterMachines = M.elems . _clusterMachineMap


setTunnelPorts :: Int -> Cluster -> Cluster
setTunnelPorts startingPort c@Cluster{..} =
    case _clusterTunnel of
      Nothing -> c
      Just _ -> Cluster _clusterTunnel $
                  M.fromList $ map (\m -> (_machineName m, m)) ms
  where
    ms = zipWith f [startingPort..] (clusterMachines c)
    f p m = m { _machineLocalForwardPort = Just p }

getCluster :: C.Config -> Text -> IO Cluster
getCluster cfg nm = do
    let c = C.subconfig nm cfg
        t = C.subconfig "tunnel" c
        ms = C.subconfig "machines" c
    mtunnel <- getTunnel t
    ms <- getMachines ms (nm <> "." <> "machines")
    return $ maybe id (setTunnelPorts . _tunnelStartingLocalPort) mtunnel
           $ Cluster mtunnel $ M.fromList $ map (\m -> (_machineName m, m)) ms

getTunnel :: C.Config -> IO (Maybe Tunnel)
getTunnel cfg = do
    mIp <- C.lookup cfg "ip"
    mUser <- C.lookup cfg "user"
    mIdentityFile <- C.lookup cfg "identityFile"
    startport <- C.lookupDefault 52500 cfg "startingLocalPort"
    return $ Tunnel <$> mIp
                    <*> mUser
                    <*> pure mIdentityFile
                    <*> pure startport

getMachines :: C.Config -> Text -> IO [Machine]
getMachines cfg prefix = do
    m <- C.getMap cfg
    let ps = map (first $ T.drop (T.length prefix + 1)) $
             filter (T.isPrefixOf prefix . fst) $ HM.toList m
    catMaybes <$> mapM (getMachine cfg . fst) ps

getMachine :: C.Config -> Text -> IO (Maybe Machine)
getMachine cfg nm = do
    mHost <- C.lookup cfg nm
    let m = Machine <$> pure (T.unpack nm)
                    <*> fmap T.unpack mHost
                    <*> pure Nothing
                    <*> pure Nothing
                    <*> pure Nothing
    return m

getSections :: C.Config -> IO [Text]
getSections cfg = do
    m <- C.getMap cfg
    return $ S.toList $ S.fromList $ catMaybes $ map getSection $ HM.toList m
  where
    getSection p = case T.breakOn "." (fst p) of
                     (_,"") -> Nothing
                     (s,_) -> Just s
