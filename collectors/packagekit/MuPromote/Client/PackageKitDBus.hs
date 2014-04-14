{-# LANGUAGE OverloadedStrings #-}
module MuPromote.Client.PackageKitDBus (
  dbusInitSt,
  getPackages
  ) where

{- *
  This module defines the binding to the DBus interface of Packagekit.
  This first version is completely without error handling.

  *-}

import Control.Applicative

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Control.Monad.Reader
import Control.Monad.IO.Class

import Data.Maybe(fromJust)

import qualified Data.Map as M

import DBus
import DBus.Client

-- Only for convenient initialization of client connection and signal handler.
import System.IO.Unsafe(unsafePerformIO)

-- *** Types and initializers ***

-- | A convenience monad for wrapping dbus access, sporting an implicit Client
-- connection and a slightly more structured signal handler.
data DBusMSt = DBusMSt { client :: Client,
                        -- | Signal handler channel, hidden in an MVar to
                        -- guarantee thread exclusive access
                         sigChan :: MVar (Chan Signal)}

type DBusM = ReaderT DBusMSt IO

-- | Connect to DBus system bus and wrap the dbus signal reception facility.
dbusInitSt = do
  client <- connectSystem
  ch <- newChan
  listen client matchAny (writeChan ch)
  mvCh <- newMVar ch
  return $ DBusMSt client mvCh

-- *** PKit/DBus async signal protocol abtraction  ***

varStr = fromJust . fromVariant :: Variant -> String

-- convenience wrapper on call_ in the DBusM monad.
callD_ :: MethodCall -> DBusM MethodReturn
callD_ method = asks client >>= \c -> liftIO $ call_ c method

-- abstract a pk async signal response, inside a transaction context
asyncResp tId memb fun =
  receive (
   let get sigCh = do
             sig <- liftIO $ readChan sigCh
             case sig of
               x | signalPath x == tId && signalMember x == memb -> do
                 let res = fun (signalBody x)
                 rest <- get sigCh
                 return $ res : rest
               x | signalPath x == tId && signalMember x == "Finished" ->
                 let status = signalBody sig |> head |> varStr :: String
                 in if status == "success" then
                   return []
                    else error ("bad Finished = " ++ status)
               _ -> -- do
                 -- putStrLn $ "ignored signal : " ++ show sig
                 get sigCh -- ignore unknown signals
              in get)
  where
    receive :: (Chan Signal -> IO a) -> DBusM a
    receive f = do
      sigChanMv <- asks sigChan
      ch <- liftIO $ takeMVar sigChanMv
      res <- liftIO $ f ch
      liftIO $ putMVar sigChanMv ch
      return res

-- Backwards function application
infixl 1 |>
a |> f = f a


--- *** DBus method wrappers ***

-- convenience-wrapper to construct methodCall's for packagekit

pkMethod meth = (methodCall "/org/freedesktop/PackageKit" "org.freedesktop.PackageKit"
          meth )
          {methodCallDestination = Just "org.freedesktop.PackageKit"}

pkTransMethod tid meth = (pkMethod meth)
  { methodCallPath = tid,
    methodCallInterface = Just "org.freedesktop.PackageKit.Transaction"}

getTidMethod = pkMethod "GetTid" {- named GetTransaction from 0.8.2 -}

getPackagesMethod tId = (pkTransMethod tId "GetPackages")
          { methodCallBody = [toVariant ("installed" :: String)]}

getDetailsMethod tId pkgs = (pkTransMethod tId "GetDetails")
          { methodCallBody = [toVariant pkgs]}

-- *** Actual haskell-friendly API : ***

-- Create a new transaction. It's a synchronous operation
createTransaction :: DBusM ObjectPath
createTransaction = do
  rep <- callD_ getTidMethod
  let [Just tId] = map fromVariant $ methodReturnBody rep :: [Maybe String]
  return (objectPath_ tId)

-- | Fetch the list of installed packages, according to the running instance of
-- packagekitd.
getPackages :: DBusM (M.Map String String)
getPackages = do
  tId <- createTransaction
  rep <- callD_ $ getPackagesMethod tId
  pkgs <- asyncResp tId "Package" (varStr . (!!1)) :: DBusM [String]

  tId <- createTransaction
  rep <- callD_ $ getDetailsMethod tId pkgs
  urls <- asyncResp tId "Details" (\bd -> (varStr $ head bd, varStr $ bd !! 4))
  return $ M.fromList urls

runD a = do
  st <- dbusInitSt
  runReaderT a st

-- *** Debug Stuff: ***

-- exploratory dbus access. Performs call in a PackageKit transaction context.
callTr meth body= do
  st <- dbusInitSt
  lock <- newEmptyMVar
  res <- runReaderT (do
    trans <- createTransaction
    client <- asks client
    liftIO $ listen client matchAny{matchPath = Just trans} (\s ->
      do
        takeMVar lock
        print (signalMember s, signalBody s)
        putMVar lock ())
    callD_ (pkTransMethod trans meth){methodCallBody = body}
    ) st
  print res
  putMVar lock ()

