--
-- Copyright (c) 2012 Citrix Systems, Inc.
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
--

{-# LANGUAGE ConstrainedClassMethods, OverloadedStrings, PatternGuards #-}
module Rpc.Types
       ( ObjectPath
       , BusName
       , InterfaceName
       , MemberName
       , ErrorName
       , strObjectPath
       , strBusName
       , strInterfaceName
       , strMemberName
       , strErrorName
       , mkObjectPath
       , mkObjectPath_
       , mkBusName
       , mkBusName_
       , mkInterfaceName
       , mkInterfaceName_
       , mkMemberName
       , mkMemberName_
       , mkErrorName
       , mkErrorName_
       , RemoteObject(..)
       , Proxy(..)
       , RpcCall (..)
       , RpcSignal (..)
       , RpcInterface (..)
       , RpcMethod (..)
       , RpcProperty (..)
       , RpcParam (..)
       , PropertyAccess (..)
       , callInterface, callMember
       , signalInterface, signalMember
       , paramName
       , mtName
       , propName
         
       , D.DBusValue
       , strDBusValue
       , Variant -- for compatiblity, TODO: scrape me
       ) where

import Data.List
import Data.Char
import Data.String
import Data.Maybe
import Data.Typeable hiding (Proxy)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text)
import qualified Control.Exception as E
import qualified Network.DBus as D
import qualified Network.DBus.Actions as D
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

data ObjectPath = ObjectPath { strObjectPath :: String } deriving (Eq,Ord)
data BusName = BusName { strBusName :: String } deriving (Eq,Ord)
data InterfaceName =  InterfaceName { strInterfaceName :: String } deriving (Eq,Ord)
data MemberName = MemberName { strMemberName :: String } deriving (Eq,Ord)
data ErrorName = ErrorName { strErrorName :: String } deriving (Eq,Ord)

mkObjectPath :: String -> Maybe ObjectPath
mkObjectPath x
  | take 1 x == "/"
  , null (filter invalid_ch x) = Just (ObjectPath x)
  | otherwise = Nothing
    where
      invalid_ch = not . valid_ch
      valid_ch '_' = True
      valid_ch '/' = True
      valid_ch c = isAlphaNum c
mkObjectPath_ x = fromMaybe (error $ "invalid object path: " ++ x) (mkObjectPath x)

mkBusName :: String -> Maybe BusName
mkBusName x
  | null x = Nothing
  | otherwise = Just (BusName x)
mkBusName_ x = fromMaybe (error $ "invalid bus name: " ++ x) (mkBusName x)

mkInterfaceName :: String -> Maybe InterfaceName
mkInterfaceName x
  | null x = Nothing
  | otherwise = Just (InterfaceName x)
mkInterfaceName_ x = fromMaybe (error $ "invalid interface name: " ++ x) (mkInterfaceName x)

mkMemberName :: String -> Maybe MemberName
mkMemberName x
  | null x = Nothing
  | otherwise = Just (MemberName x)
mkMemberName_ x = fromMaybe (error $ "invalid member name: " ++ x) (mkMemberName x)

mkErrorName :: String -> Maybe ErrorName
mkErrorName x
  | null x = Nothing
  | otherwise = Just (ErrorName x)
mkErrorName_ x = fromMaybe (error $ "invalid error name: " ++ x) (mkErrorName x)

instance IsString ObjectPath where fromString = mkObjectPath_
instance IsString BusName where fromString = mkBusName_
instance IsString InterfaceName where fromString = mkInterfaceName_
instance IsString MemberName where fromString = mkMemberName_
instance IsString ErrorName where fromString = mkErrorName_

data RemoteObject = RemoteObject BusName ObjectPath
data Proxy = Proxy RemoteObject InterfaceName

instance Show ObjectPath where show (ObjectPath p) = p
instance Show BusName where show (BusName n) = n
instance Show InterfaceName where show (InterfaceName n) = n
instance Show MemberName where show (MemberName n) = n
instance Show ErrorName where show (ErrorName n) = n
instance Show Proxy where
    show (Proxy remoteObj intf) =
        let RemoteObject service path = remoteObj in
        show path ++ " at " ++ show service ++ " : " ++ show intf

-- compat
type Variant = D.DBusValue

data RpcInterface m = RpcInterface !InterfaceName ![RpcMethod m] ![RpcProperty]

data RpcCall =
     RpcCall { callDest :: !BusName
             , callPath :: !ObjectPath
             , callInterfaceT :: !InterfaceName
             , callMemberT :: !MemberName
             , callArgs    :: ![D.DBusValue]
             }

callInterface = strInterfaceName . callInterfaceT
callMember = strMemberName . callMemberT

data RpcSignal =
     RpcSignal { signalPath :: !ObjectPath
               , signalInterfaceT :: !InterfaceName
               , signalMemberT :: !MemberName
               , signalArgs :: ![D.DBusValue]
               }

signalInterface = strInterfaceName . signalInterfaceT
signalMember = strMemberName . signalMemberT

instance Show RpcCall where
    show (RpcCall dest path intf member args) =
        show dest ++ show path ++ ":" ++ show intf ++ "." ++ show member ++ "(" ++ argsStr ++ ")"
      where
        argsStr = foldl (++) "" $ intersperse "," (map show args)


data RpcParam =
     RpcParam { paramNameT :: !Text
              , paramType  :: !D.SignatureElem }
     deriving (Eq, Show)

paramName = TL.unpack . paramNameT

data RpcMethod m =
     RpcMethod { mtSigIn   :: ![RpcParam]
               , mtSigOut  :: ![RpcParam]
               , mtNameT   :: !MemberName
               , mtInvoke  :: [D.DBusValue] -> m [D.DBusValue]
               }

mtName = strMemberName . mtNameT

instance Show (RpcMethod m) where
    show m = show (mtName m)

data RpcProperty =
     RpcProperty { propType   :: !D.SignatureElem
                 , propNameT  :: !MemberName
                 , propAccess :: !PropertyAccess }
     deriving (Eq, Show)

propName = strMemberName . propNameT

data PropertyAccess = Read | Write | ReadWrite deriving (Eq, Show)

strDBusValue :: D.DBusValue -> String
strDBusValue (D.DBusByte x) = show x
strDBusValue (D.DBusBoolean x) = show x
strDBusValue (D.DBusInt16 x) = show x
strDBusValue (D.DBusUInt16 x) = show x
strDBusValue (D.DBusInt32 x) = show x
strDBusValue (D.DBusUInt32 x) = show x
strDBusValue (D.DBusInt64 x) = show x
strDBusValue (D.DBusUInt64 x) = show x
strDBusValue (D.DBusDouble x) = show x
strDBusValue (D.DBusByteArray bs) = "[" ++ intercalate "," (map show . B.unpack $ bs) ++ "]"
strDBusValue (D.DBusString (D.PackedString x)) = UTF8.toString x
strDBusValue (D.DBusObjectPath x) = show x
strDBusValue (D.DBusSignature x) = UTF8.toString (D.serializeSignature x)
strDBusValue (D.DBusArray _ xs) = "[" ++ intercalate "," (map strDBusValue xs) ++ "]"
strDBusValue (D.DBusStruct _ xs) = "[" ++ intercalate "," (map strDBusValue xs) ++ "]"
strDBusValue (D.DBusDict k v) = strDBusValue k ++ "=" ++ strDBusValue v
strDBusValue (D.DBusUnixFD x) = show x
strDBusValue (D.DBusVariant x) = strDBusValue x
