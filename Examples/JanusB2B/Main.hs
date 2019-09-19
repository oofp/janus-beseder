{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  Main where

import            Protolude      
import            Beseder.Janus.JanusCallProv              
import            Beseder.Janus.JanusCallProvImpl              
import            Comm.Janus.JanusConnector
import            System.Environment (getArgs)
import            Data.Text
import            JanusB2BApp
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Control (STransApp (..))
import            Beseder.Misc.Misc
import            Beseder.Utils
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Log.Handler.Simple (fileHandler)

main :: IO ()
main = do
  putStrLn ("Welcome to Janus sample app "::Text)
  args <- getArgs
  case args of 
    [srvIP, portTxt, sipAOR, sipDisplayName, sipPwd, sipProxy, videoDest, sipDest] ->
      let srvPortMaybe :: Maybe Int
          srvPortMaybe = readMaybe portTxt
      in case srvPortMaybe of
        Just srvPort -> 
          let regReq = JanusRegisterReqPs
                        { userName = pack sipAOR
                        , displayName = pack sipDisplayName
                        , password = pack sipPwd
                        , proxy = pack sipProxy
                        }
          in b2bApp (pack srvIP) srvPort regReq (pack videoDest) (pack sipDest)
        Nothing ->
          putStrLn ("Failed to parse sip port (must be Int)"::Text)
    _ -> do putStrLn ("Parametrs: srvIP srvPort sipAOR sipDisplayName sipPwd sipProxy videoDest sipDest"::Text)        
            putStrLn ("For example: JanusSample janusserver.com 8188 sip:user@sipprovider.com John 123456 sip:sipprovider.com Bob Alice@sipprovider.com"::Text)        

b2bApp :: Text -> Int -> JanusRegisterReqPs -> Text -> Text -> IO ()
b2bApp srvIP port sipRegPs videoDest videoUser = do
  setupLog
  conHandle <- initConnector srvIP port   
  waitForConnectivity conHandle
  let sipRes = sipCallRes conHandle  sipRegPs
      videoRes = videoCallRes conHandle videoUser
      transApp = MkApp $ b2bTrans sipRes videoRes videoDest    
  runAsyncApp transApp

setupLog::IO()
setupLog=do
  let rootLog = "JanusCon"

  updateGlobalLogger rootLog (setLevel DEBUG)
  h <- fileHandler "janusSampleLog.log" DEBUG >>= \lh -> return $
              setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  updateGlobalLogger rootLog (addHandler h)  