{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Network.Socket.InterfaceRequest
where

import Foreign.Storable
import Foreign.C.Types
import Network.Socket
import Network.Socket.IOCtl
import Data.Bits
import Foreign.Ptr
import Foreign.C.String
import Control.Applicative
import Foreign.Marshal

#include <netinet/in.h>
#include <linux/if.h>

data InterfaceRequest a = InterfaceRequest { irIface :: String, irValue :: a }
data InterfaceFlags = InterfaceFlags {
  ifaceUp :: Bool,
  ifaceRunning :: Bool
                                     }
ifaceDefaultFlags = InterfaceFlags True True

instance Storable InterfaceFlags where
  sizeOf _ = sizeOf (undefined :: CShort)
  alignment _ = alignment (undefined :: CShort)
  peek p = fmap (\i -> InterfaceFlags (i .&. 0x1 /= 0) (i .&. 0x40 /= 0))
                (peek ((castPtr p) :: Ptr CShort))
  poke p (InterfaceFlags ifaceUp ifaceRunning) = 
    poke ((castPtr p) :: Ptr CShort) $
      (if ifaceUp then 0x1 else 0) .|.
      (if ifaceRunning then 0x40 else 0)

instance Storable a => Storable (InterfaceRequest a) where
  sizeOf _ = #size struct ifreq
  alignment _ = alignment (undefined :: CInt)
  peek p = InterfaceRequest <$> (peekCString (castPtr p)) <*>
                                   peek (plusPtr p 16)
  poke p (InterfaceRequest n f) = do
    let pchar = (castPtr p) :: CString
    withCStringLen n $ \(s, l) -> do
      let l' = min l 15
      copyBytes pchar s l'
      pokeByteOff pchar l' (0 :: CChar)
    pokeByteOff (castPtr p) 16 (0 :: CULLong)
    pokeByteOff (castPtr p) 24 (0 :: CULLong)
    poke (plusPtr p 16) f

data SetInterfaceFlags = SetInterfaceFlags
data SetInterfaceMTU = SetInterfaceMTU
data SetNoCSum = SetNoCSum

instance IOControl SetInterfaceFlags (InterfaceRequest InterfaceFlags) where
  ioctlReq _ = 0x8914
instance IOControl SetInterfaceMTU (InterfaceRequest CInt) where
  ioctlReq _ = 0x8922

setInterfaceFlags :: Socket -> String -> InterfaceFlags -> IO ()
setInterfaceFlags s n iflags =
  ioctlsocket_ s SetInterfaceFlags (InterfaceRequest n iflags)

setInterfaceMTU :: Socket -> String -> Int -> IO ()
setInterfaceMTU s n mtu =
  ioctlsocket_ s SetInterfaceMTU (InterfaceRequest n ((fromIntegral mtu) :: CInt))
