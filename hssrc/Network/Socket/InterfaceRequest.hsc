{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}
module Network.Socket.InterfaceRequest
where

import Foreign.Storable
import Foreign.C.Types
import Network.Socket
import Network.Socket.IOCtl
import Data.Bits
import Data.Foldable
import Foreign.Ptr
import Foreign.C.String
import Control.Applicative
import Foreign.Marshal

#include <sys/ioctl.h>
#include <netinet/in.h>
#include <linux/if.h>

data InterfaceRequest a = InterfaceRequest { irIface :: String, irValue :: a }

data InterfaceFlags = InterfaceFlags
  { ifaceUp           :: Bool -- | IFF_UP            Interface is running.
  , ifaceBroadcast    :: Bool -- | IFF_BROADCAST     Valid broadcast address set.
  , ifaceDebug        :: Bool -- | IFF_DEBUG         Internal debugging flag.
  , ifaceLoopback     :: Bool -- | IFF_LOOPBACK      Interface is a loopback interface.

  , ifacePointToPoint :: Bool -- | IFF_POINTOPOINT   Interface is a point-to-point link.
  , ifaceRunning      :: Bool -- | IFF_RUNNING       Resources allocated.
  , ifaceNoArp        :: Bool -- | IFF_NOARP         No arp protocol, L2 destination address not set.

  , ifacePromiscuous  :: Bool -- | IFF_PROMISC       Interface is in promiscuous mode.
  , ifaceNoTrailers   :: Bool -- | IFF_NOTRAILERS    Avoid use of trailers.
  , ifaceAllMulticast :: Bool -- | IFF_ALLMULTI      Receive all multicast packets.
  , ifaceMasterLB     :: Bool -- | IFF_MASTER        Master of a load balancing bundle.
  , ifaceSlaveLB      :: Bool -- | IFF_SLAVE         Slave of a load balancing bundle.
  , ifaceMulticast    :: Bool -- | IFF_MULTICAST     Supports multicast
  , ifacePortSel      :: Bool -- | IFF_PORTSEL       Is able to select media type via ifmap.
  , ifaceAutoMedia    :: Bool -- | IFF_AUTOMEDIA     Auto media selection active.
  , ifaceDynamic      :: Bool -- | IFF_DYNAMIC       The addresses are lost when the interface goes down.

  -- How these three can end up in a 16bit short is beyond me…
  , ifaceLowerUp      :: Bool -- | IFF_LOWER_UP      Driver signals L1 up (since Linux 2.6.17)
  , ifaceDormant      :: Bool -- | IFF_DORMANT       Driver signals dormant (since Linux 2.6.17)
  , ifaceEcho         :: Bool -- | IFF_ECHO          Echo sent packets (since Linux 2.6.25)
  }

ifaceDefaultFlags :: InterfaceFlags
ifaceDefaultFlags =  InterfaceFlags
  { ifaceUp           = False
  , ifaceBroadcast    = False
  , ifaceDebug        = False
  , ifaceLoopback     = False

  , ifacePointToPoint = False
  , ifaceRunning      = False
  , ifaceNoArp        = False

  , ifacePromiscuous  = False
  , ifaceNoTrailers   = False
  , ifaceAllMulticast = False
  , ifaceMasterLB     = False
  , ifaceSlaveLB      = False
  , ifaceMulticast    = False
  , ifacePortSel      = False
  , ifaceAutoMedia    = False
  , ifaceDynamic      = False

  , ifaceLowerUp      = False
  , ifaceDormant      = False
  , ifaceEcho         = False
  }

instance Storable InterfaceFlags where
  sizeOf _ = sizeOf (undefined :: CShort)
  alignment _ = alignment (undefined :: CShort)
  peek p = fmap (\i -> InterfaceFlags
                       { ifaceUp           = (i .&. (#const IFF_UP         ) /= 0)
                       , ifaceBroadcast    = (i .&. (#const IFF_BROADCAST  ) /= 0)
                       , ifaceDebug        = (i .&. (#const IFF_DEBUG      ) /= 0)
                       , ifaceLoopback     = (i .&. (#const IFF_LOOPBACK   ) /= 0)

                       , ifacePointToPoint = (i .&. (#const IFF_POINTOPOINT) /= 0)
                       , ifaceRunning      = (i .&. (#const IFF_RUNNING    ) /= 0)
                       , ifaceNoArp        = (i .&. (#const IFF_NOARP      ) /= 0)

                       , ifacePromiscuous  = (i .&. (#const IFF_PROMISC    ) /= 0)
                       , ifaceNoTrailers   = (i .&. (#const IFF_NOTRAILERS ) /= 0)
                       , ifaceAllMulticast = (i .&. (#const IFF_ALLMULTI   ) /= 0)
                       , ifaceMasterLB     = (i .&. (#const IFF_MASTER     ) /= 0)
                       , ifaceSlaveLB      = (i .&. (#const IFF_SLAVE      ) /= 0)
                       , ifaceMulticast    = (i .&. (#const IFF_MULTICAST  ) /= 0)
                       , ifacePortSel      = (i .&. (#const IFF_PORTSEL    ) /= 0)
                       , ifaceAutoMedia    = (i .&. (#const IFF_AUTOMEDIA  ) /= 0)
                       , ifaceDynamic      = (i .&. (#const IFF_DYNAMIC    ) /= 0)

                       , ifaceLowerUp      = (i .&. (#const IFF_LOWER_UP   ) /= 0)
                       , ifaceDormant      = (i .&. (#const IFF_DORMANT    ) /= 0)
                       , ifaceEcho         = (i .&. (#const IFF_ECHO       ) /= 0)
                       })
                (peek ((castPtr p) :: Ptr CShort))
  poke p (InterfaceFlags{..}) =
    poke ((castPtr p) :: Ptr CShort) $ foldl' (.|.) 0
      [ if ifaceUp           then (#const IFF_UP         ) else 0
      , if ifaceBroadcast    then (#const IFF_BROADCAST  ) else 0
      , if ifaceDebug        then (#const IFF_DEBUG      ) else 0
      , if ifaceLoopback     then (#const IFF_LOOPBACK   ) else 0

      , if ifacePointToPoint then (#const IFF_POINTOPOINT) else 0
      , if ifaceRunning      then (#const IFF_RUNNING    ) else 0
      , if ifaceNoArp        then (#const IFF_NOARP      ) else 0

      , if ifacePromiscuous  then (#const IFF_PROMISC    ) else 0
      , if ifaceNoTrailers   then (#const IFF_NOTRAILERS ) else 0
      , if ifaceAllMulticast then (#const IFF_ALLMULTI   ) else 0
      , if ifaceMasterLB     then (#const IFF_MASTER     ) else 0
      , if ifaceSlaveLB      then (#const IFF_SLAVE      ) else 0
      , if ifaceMulticast    then (#const IFF_MULTICAST  ) else 0
      , if ifacePortSel      then (#const IFF_PORTSEL    ) else 0
      , if ifaceAutoMedia    then (#const IFF_AUTOMEDIA  ) else 0
      , if ifaceDynamic      then (#const IFF_DYNAMIC    ) else 0

      , if ifaceLowerUp      then (#const IFF_LOWER_UP   ) else 0
      , if ifaceDormant      then (#const IFF_DORMANT    ) else 0
      , if ifaceEcho         then (#const IFF_ECHO       ) else 0
      ]

#include <linux/if_tun.h>

data InterfaceTunTapFlags = InterfaceTunTapFlags
  { ifaceTunTapTun          :: Bool -- | IFF_TUN           TUN device (no Ethernet headers)
  , ifaceTunTapTap          :: Bool -- | IFF_TAP           TAP device
  , ifaceTunTapNoPi         :: Bool -- | IFF_NO_PI         Do not provide packet information
  }

ifaceTunTapDefaultFlags :: InterfaceTunTapFlags
ifaceTunTapDefaultFlags =  InterfaceTunTapFlags
  { ifaceTunTapTun          = False
  , ifaceTunTapTap          = False
  , ifaceTunTapNoPi         = False
  }

instance Storable InterfaceTunTapFlags where
  sizeOf _ = sizeOf (undefined :: CShort)
  alignment _ = alignment (undefined :: CShort)
  peek p = fmap (\i -> InterfaceTunTapFlags
                       { ifaceTunTapTun  = (i .&. (#const IFF_TUN        ) /= 0)
                       , ifaceTunTapTap  = (i .&. (#const IFF_TAP        ) /= 0)
                       , ifaceTunTapNoPi = (i .&. (#const IFF_NO_PI      ) /= 0)
                       })
                (peek ((castPtr p) :: Ptr CShort))
  poke p (InterfaceTunTapFlags{..}) =
    poke ((castPtr p) :: Ptr CShort) $ foldl' (.|.) 0
      [ if ifaceTunTapTun          then (#const IFF_TUN        ) else 0
      , if ifaceTunTapTap          then (#const IFF_TAP        ) else 0
      , if ifaceTunTapNoPi         then (#const IFF_NO_PI      ) else 0
      ]

instance Storable a => Storable (InterfaceRequest a) where
  sizeOf _ = #size struct ifreq
  alignment _ = alignment (undefined :: CInt)
  peek p = InterfaceRequest <$> (peekCString (castPtr p)) <*> peek (plusPtr p 16)
  poke p (InterfaceRequest n f) = do
    let pchar = (castPtr p) :: CString
    withCStringLen n $ \(s, l) -> do
      let l' = min l 15
      copyBytes pchar s l'
      pokeByteOff pchar l' (0 :: CChar)
    pokeByteOff (castPtr p) 16 (0 :: CULLong)
    pokeByteOff (castPtr p) 24 (0 :: CULLong)
    poke (plusPtr p 16) f

data GetInterfaceFlags = GetInterfaceFlags
data SetInterfaceFlags = SetInterfaceFlags

data GetInterfaceTunTapFlags = GetInterfaceTunTapFlags
data SetInterfaceTunTapFlags = SetInterfaceTunTapFlags

data SetInterfaceMTU = SetInterfaceMTU
data SetNoCSum = SetNoCSum

instance IOControl GetInterfaceFlags (InterfaceRequest InterfaceFlags) where
  ioctlReq _ = #const SIOCSIFFLAGS

instance IOControl SetInterfaceFlags (InterfaceRequest InterfaceFlags) where
  ioctlReq _ = #const SIOCSIFFLAGS

instance IOControl GetInterfaceTunTapFlags (InterfaceRequest InterfaceTunTapFlags) where
  ioctlReq _ = #const TUNGETIFF

instance IOControl SetInterfaceTunTapFlags (InterfaceRequest InterfaceTunTapFlags) where
  ioctlReq _ = #const TUNSETIFF

instance IOControl SetInterfaceMTU (InterfaceRequest CInt) where
  ioctlReq _ = #const SIOCSIFMTU

getInterfaceFlags :: Socket -> String -> IO InterfaceFlags
getInterfaceFlags s n =
  irValue <$> ioctlsocket s GetInterfaceFlags (InterfaceRequest n ifaceDefaultFlags)

setInterfaceFlags :: Socket -> String -> InterfaceFlags -> IO ()
setInterfaceFlags s n iflags =
  ioctlsocket_ s SetInterfaceFlags (InterfaceRequest n iflags)

getInterfaceTunTapFlags :: Socket -> String -> IO InterfaceTunTapFlags
getInterfaceTunTapFlags s n =
  irValue <$> ioctlsocket s GetInterfaceTunTapFlags (InterfaceRequest n ifaceTunTapDefaultFlags)

setInterfaceTunTapFlags :: Socket -> String -> InterfaceTunTapFlags -> IO ()
setInterfaceTunTapFlags s n iflags =
  ioctlsocket_ s SetInterfaceTunTapFlags (InterfaceRequest n iflags)

setInterfaceMTU :: Socket -> String -> Int -> IO ()
setInterfaceMTU s n mtu =
  ioctlsocket_ s SetInterfaceMTU (InterfaceRequest n ((fromIntegral mtu) :: CInt))
