{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Fleet
  ( Fleet
  , FleetEnv
  , feCluster
  , runFleet
  , loadCluster

  , sshConfig

  , withSshConfig
  , getFile
  , getFromAll
  , putFile
  , putToAll
  , remoteCmd
  , cmdAll
  , remoteScript
  , scriptAll

  , diff
  , diffMany
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Lens hiding ((<.>))
import           Control.Monad.Reader
import qualified Data.Configurator as C
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           System.Directory
import           System.Exit
import           System.FilePath
import           Text.Printf
import           Turtle (proc)
------------------------------------------------------------------------------
import           Fleet.Types
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data TunnelOpts = TunnelOpts
    { toptsToPort       :: Int
    }

instance Default TunnelOpts where
    def = TunnelOpts 22

------------------------------------------------------------------------------
data FleetEnv = FleetEnv
    { feCluster       :: Cluster
    , feSshConfigFile :: FilePath
    }

------------------------------------------------------------------------------
newtype Fleet a = Fleet { unFleet :: ReaderT FleetEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FleetEnv)

------------------------------------------------------------------------------
-- | Loads a cluster configuration file.
loadCluster
    :: FilePath
    -- ^ Path to the fleet config file
    -> Text
    -- ^ Name of the cluster you want to use as it appears in the config file
    -> IO Cluster
loadCluster configFile clusterName = do
    cfg <- C.load [C.Required configFile]
    getCluster cfg clusterName

------------------------------------------------------------------------------
-- | Runs a Fleet monad computation.  Reads a cluster config file and sets up
-- a tunnel for the specified cluster name.  Then runs the computation using
-- that tunnel.
runFleet
    :: FilePath
    -- ^ Path to the fleet config file
    -> Text
    -- ^ Name of the cluster you want to use as it appears in the config file
    -> Fleet a
    -- ^ Fleet monad computation to run
    -> IO a
runFleet configFile clusterName flt = do
    c <- loadCluster configFile clusterName
    runFleet' c flt

------------------------------------------------------------------------------
runFleet' :: Cluster -> Fleet a -> IO a
runFleet' c flt = do
    mtid <- openTunnel def c
    tempdir <- getTemporaryDirectory
    t <- getCurrentTime
    let sshConfigFile = tempdir </> "ssh-config-" <>
          formatTime defaultTimeLocale "%0y%m%d%H%M%S" t <.> "config"
    -- We write an ssh config file because that makes the ssh and scp
    -- command lines simpler.
    writeFile sshConfigFile $ sshConfig c "tunnel" 22
    res <- runReaderT (unFleet flt) $ FleetEnv c sshConfigFile
    maybe (return ()) killThread mtid
    removeFile sshConfigFile
    return res

------------------------------------------------------------------------------
-- | Opens a tunnel for a cluster.
openTunnel :: TunnelOpts -> Cluster -> IO (Maybe ThreadId)
openTunnel TunnelOpts{..} c@Cluster{..} = do
    case _clusterTunnel of
      Nothing -> return Nothing
      Just t -> do
          let forwardingArgs = concat $ map (mkForwardArg toptsToPort)
                                 (clusterMachines c)
              machine = concat [_tunnelUser t, "@", _tunnelHost t]
              mkIdentityFile f = ["-i", f]
              args = map T.pack $ concat
                       [ "-M" : maybe [] mkIdentityFile (_tunnelIdentityFile t)
                       , forwardingArgs
                       , [machine]
                       ]
          putStrLn $ "Calling ssh with args: " ++ T.unpack (T.unwords args)
          fmap Just $ forkIO $ void $ proc "ssh" args (return "")

------------------------------------------------------------------------------
mkForwardArg :: Int -> Machine -> [String]
mkForwardArg toPort m = ["-L", pstr]
  where
    pstr = intercalate ":"
             [ show $ fromMaybe 666 $ _machineLocalForwardPort m
             , _machineHost m
             , show toPort
             ]

------------------------------------------------------------------------------
-- | Creates the contents of an SSH config file with predifined hosts using
-- the cluster's tunnel.
sshConfig
    :: Cluster
    -- ^ Cluster to create a a config file for
    -> String
    -- ^ Name for the host that sets up the tunnels
    -> Int
    -- ^ Forward to this port on all the machines
    -> String
sshConfig c nm toPort = intercalate "\n" $
    case _clusterTunnel c of
      Nothing -> blocks
      Just t -> tunnelBlock t : blocks
  where
    blocks = map machineConfigBlock $ clusterMachines c
    tunnelBlock t = unlines $ catMaybes $
        [ Just $ unwords ["Host", nm]
        , req ["HostName", _tunnelHost t]
        , req ["User", _tunnelUser t]
        , opt (\f -> ["IdentityFile", f]) _tunnelIdentityFile
        ] ++ map (fmap indent . fwd) (clusterMachines c)
          ++ [Just ""]
      where
        opt :: (a -> [String]) -> (Tunnel -> Maybe a) -> Maybe String
        opt f g = maybe Nothing (\a -> Just $ indent $ unwords (f a)) $ g t
        req s = Just $ indent $ unwords s
        fwd m = (\p -> unwords ["LocalForward", show p, _machineHost m <> ":" <> show toPort])
            <$> _machineLocalForwardPort m

------------------------------------------------------------------------------
indent :: (IsString m, Monoid m) => m -> m
indent s = "  " <> s

------------------------------------------------------------------------------
machineConfigBlock :: Machine -> String
machineConfigBlock m = unlines $ catMaybes
    [ Just $ unwords ["Host", _machineName m]
    , Just $ "  " ++ "HostName localhost"
    , opt (\u -> ["User", u]) _machineUser
    , opt (\p -> ["Port", show p]) _machineLocalForwardPort
    , opt (\f -> ["IdentityFile", f]) _machineIdentityFile
    ]
  where
    opt :: (a -> [String]) -> (Machine -> Maybe a) -> Maybe String
    opt f g = maybe Nothing (\a -> Just $ "  " ++ unwords (f a)) $ g m

------------------------------------------------------------------------------
-- | Executes the specified command (usually \"ssh\" or \"scp\") with a -F
-- argumnet setting the config file to the one being used by the current Fleet
-- computation.
withSshConfig :: Text -> [Text] -> Fleet ExitCode
withSshConfig cmd args = do
    sshCfg <- asks feSshConfigFile
    liftIO $ proc cmd ("-F" : T.pack sshCfg : args) (return "")

------------------------------------------------------------------------------
-- | Copies a file from a remote machine in the cluster to the local machine.
getFile :: RemotePath -> LocalPath -> MachineName -> Fleet ()
getFile rp lp m = do
    let args = [m <> ":" <> unRemotePath rp, unLocalPath lp]
    void $ withSshConfig "scp" (map T.pack args)

------------------------------------------------------------------------------
-- | Compares two files without outputting their diff.
diff :: MonadIO io => Text -> Text -> io ExitCode
diff file1 file2 = proc "diff" ["-q", file1, file2] (return "")


------------------------------------------------------------------------------
-- | Compares a list of files.
diffMany :: MonadIO m => [Text] -> m Bool
diffMany [] = return True
diffMany files = do
    results <- mapM (uncurry diff) pairs
    return $ all (==ExitSuccess) results
  where
    pairs = zip files (tail files)

------------------------------------------------------------------------------
-- | Copies a local file from a list of remote machines.  Local files have the
-- same filename as on the remote machine with the machine name concatenated
-- onto the end.
getFromAll :: RemotePath -> [MachineName] -> Fleet ()
getFromAll f = mapM_ (\s -> getFile f (LocalPath $ unRemotePath f <> "." <> s) s)

------------------------------------------------------------------------------
-- | Copies a local file to a list of remote machines
putFile :: LocalPath -> RemotePath -> MachineName -> Fleet ()
putFile lp rp m = do
    let args = [unLocalPath lp, m<>":"<>unRemotePath rp]
    void $ withSshConfig "scp" (map T.pack args)

------------------------------------------------------------------------------
-- | Copies a local file to a list of remote machines
putToAll
    :: LocalPath
    -- ^ Full path (relative or absolute) to the file to be copied
    -> RemotePath
    -- ^ Directory on the remote machine to copy to.  (Can be "")
    -> [MachineName]
    -- ^ List of machines to send to
    -> Fleet ()
putToAll lp@(LocalPath f) (RemotePath d) =
    mapM_ (\s -> putFile lp rp s)
  where
    rp = RemotePath $ d </> takeFileName f

------------------------------------------------------------------------------
-- | Executes a command on a remote machine.
remoteCmd :: Text -> MachineName -> Fleet ()
remoteCmd cmd m = do
    void $ withSshConfig "ssh" [T.pack m, cmd]

------------------------------------------------------------------------------
-- | Executes a command on a list of remote machines.
cmdAll :: Text -> [MachineName] -> Fleet ()
cmdAll cmd = mapM_ (remoteCmd cmd)

------------------------------------------------------------------------------
-- | Executes a script on a remote machine.
remoteScript :: LocalPath -> MachineName -> Fleet ()
remoteScript script m = do
    t <- liftIO getCurrentTime
    let rp = T.pack $ printf "/tmp/tunnel-script-%s"
          (formatTime defaultTimeLocale "%0y%m%d%H%M%S" t)
    putFile script (RemotePath $ T.unpack rp) m
    let cmd = T.unwords ["chmod +x", rp, "&&", rp]
    void $ withSshConfig "ssh" [T.pack m, cmd]

------------------------------------------------------------------------------
-- | Executes a script on a list of remote machines.
scriptAll :: LocalPath -> [MachineName] -> Fleet ()
scriptAll lp = mapM_ (remoteScript lp)

services :: [MachineName]
services =
    [ "contest_0"
    , "image_0"
    , "image_1"
    , "media_0"
    , "media_1"
    , "person_0"
    , "person_1"
    , "search_0"
    , "search_1"
    , "shop_0"
    , "shop_1"
    , "vfile_0"
    , "vfile_1"
    ]

go = do
    c <- loadCluster "machines.cfg" "production"
    let identFile = "/Users/mightybyte/.ssh/refresh.pem"
    let c2 = c & clusterMachineMap . each .  machineUser .~ Just "ubuntu"
               & clusterMachineMap . each .  machineIdentityFile .~ Just identFile
    mapM_ print $ clusterMachines c
    putStrLn "--------"
    mapM_ print $ clusterMachines c2
    mtid <- openTunnel def c
    threadDelay 5000000
    runFleet' c2 $ do
      getFromAll "production.cfg" services
    maybe (return ()) killThread mtid
