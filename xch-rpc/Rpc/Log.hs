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

{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
module Rpc.Log where

import Control.Monad
import Control.Monad.Trans
import System.Posix.Syslog
import Foreign.C

class Log m where
    debug :: String -> m ()
    warn  :: String -> m ()
    fatal :: String -> m ()
    info  :: String -> m ()

instance (MonadIO m) => Log m where
    debug s = liftIO $ withCStringLen s (\p -> syslog (Just User) Debug p)
    warn s = liftIO $ withCStringLen s (\p -> syslog (Just User) Warning p)
    fatal s = liftIO $ withCStringLen s (\p -> syslog (Just User) Error p)
    info s = liftIO $ withCStringLen s (\p -> syslog (Just User) Info p)
