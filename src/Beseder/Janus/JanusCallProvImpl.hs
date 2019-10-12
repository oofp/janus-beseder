{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}

module  Beseder.Janus.JanusCallProvImpl 
  ( sipCallRes
  , videoCallRes
  , JanusRegisterReqPs (..) 
  ) where

import            Beseder.Base.Common
import            Beseder.Utils
import            Beseder.Janus.JanusCallProv
import            Protolude
import            Control.Concurrent.STM.TVar
import            qualified GHC.Show (Show (..))
import            Comm.Janus.JanusConnector
--import            Beseder.Base.Control
--import            Beseder.Misc.Misc
--import            Beseder.Base.Base

data JanusCallPars = JanusCallPars
  { connectionHandle :: ConHandle
  , pluginType :: PluginType
  , callRegMsgPar :: JanusClientMsg
  , makeCallMsgPar :: Text -> SDPOffer -> JanusClientMsg
  , rejectCallMsgPar :: JanusClientMsg
  }

instance Show JanusCallPars where
  show pars = show (pluginType pars)

type JanusCallRes  = CallRes JanusCallPars

--janusCallRes ::  JanusCallPars -> JanusCallRes
--janusCallRes = CallRes 

data CallData = CallData 
  { serverHandler :: ServerHandler
  , makeCallMsgFunc :: Text -> SDPOffer -> JanusClientMsg
  , rejectCallMsgFunc :: JanusClientMsg
  }

--Add Channel (with all the info to CallState, including CallerID)
data CallState m 
  = CallIdleState CallData (Maybe (V (IdleTransitionNextStates m JanusCallPars) -> m Bool)) 
  | CallDialingState CallData (Maybe (V (DialingTransitionNextStates m JanusCallPars) -> m Bool))
  | CallConnectedState CallData (Maybe (V (ConnectedTransitionNextStates m JanusCallPars) -> m Bool))
  | CallOfferedState CallData SDPOffer (Maybe (V (OfferedTransitionNextStates m JanusCallPars) -> m Bool))
  | CallAnsweredState CallData SDPAnswer (Maybe (V (AnsweredTransitionNextStates m JanusCallPars) -> m Bool))
  | CallAnsweringState CallData (Maybe (V (AnsweringTransitionNextStates m JanusCallPars) -> m Bool))
  | CallDisconnectedState CallData
  | CallReleasedState
  | NoCallState

data JanusCallException = JanusCallException Text deriving (Show,Eq)
instance Exception JanusCallException

instance (TaskPoster m) => CallProv m JanusCallPars where
  data CallIdleData m JanusCallPars = MkCallIdle (TVar (CallState m))
  data CallDialingData m JanusCallPars = MkCallDialing (TVar (CallState m))
  data CallConnectedData m JanusCallPars =  MkCallConnected (TVar (CallState m)) 
  data CallOfferedData m JanusCallPars =  MkCallOffered (TVar (CallState m))
  data CallAnsweredData m JanusCallPars =  MkCallAnswered (TVar (CallState m))
  data CallAnsweringData m JanusCallPars =  MkCallAnswering (TVar (CallState m))
  data CallDisconnectedData m JanusCallPars = MkCallDisconnected (TVar (CallState m))
  data CallReleasedData m JanusCallPars = MkCallReleased 

  createCall pars = do
    taskPoster <- getTaskPoster
    liftIO $ do
      callStateVar <- newTVarIO NoCallState
      srvHandlerMaybe <- createHandler (connectionHandle pars) (pluginType pars) (janusCallback taskPoster callStateVar)
      case srvHandlerMaybe of
        Nothing -> throwIO (JanusCallException ("Failed to create plugin"::Text))
        Just srvHnd -> do 
          sendJanusRequest (callRegMsgPar pars) srvHnd
          atomically $ writeTVar callStateVar (CallIdleState (CallData srvHnd (makeCallMsgPar pars) (rejectCallMsgPar pars)) Nothing)
          return $ MkCallIdle callStateVar 
  
  makeCall MakeCall {..} (MkCallIdle callStateVar) = do 
    callData <- getCallDataVar callStateVar
    let makeCallMsg :: JanusClientMsg
        makeCallMsg = (makeCallMsgFunc callData) makeCallDest makeCallSDP
    liftIO $ sendJanusRequest makeCallMsg (serverHandler callData) 
    liftIO $ atomically $ writeTVar callStateVar (CallDialingState callData Nothing)
    return $ toVariant (MkCallDialing callStateVar)

  dropDialingCall DropCall (MkCallDialing callStateVar) = dropCallInternal callStateVar
  dropConnectedCall DropCall (MkCallConnected callStateVar) = dropCallInternal callStateVar
  dropOfferedCall DropCall (MkCallOffered callStateVar) = rejectCallInternal callStateVar
  dropAnsweringCall DropCall (MkCallAnswering callStateVar) = dropCallInternal callStateVar
  dropAnsweredCall DropCall (MkCallAnswered callStateVar) = dropCallInternal callStateVar

  releaseIdleCall ReleaseCall (MkCallIdle callStateVar) = releaseCallInternal callStateVar
  releaseDialingCall ReleaseCall (MkCallDialing callStateVar) = dropAndReleaseCallInternal callStateVar
  releaseConnectedCall ReleaseCall (MkCallConnected callStateVar) = dropAndReleaseCallInternal callStateVar
  releaseOfferedCall ReleaseCall (MkCallOffered callStateVar) = dropAndReleaseCallInternal callStateVar
  releaseAnsweringCall ReleaseCall (MkCallAnswering callStateVar) = dropAndReleaseCallInternal callStateVar
  releaseAnsweredCall ReleaseCall (MkCallAnswered callStateVar) = dropAndReleaseCallInternal callStateVar
  releaseDisconnectedCall ReleaseCall (MkCallDisconnected callStateVar) = releaseCallInternal callStateVar

  answerCall (AnswerCall (SDPAnswer sdpText)) (MkCallOffered callStateVar) = do
    callData <- getCallDataVar callStateVar
    liftIO $ sendJanusRequest (JanusAcceptReq sdpText)  (serverHandler callData)
    liftIO $ atomically $ writeTVar callStateVar (CallAnsweringState callData Nothing)
    return $ toVariant (MkCallAnswering callStateVar)

  resetDisconnectedCall ResetCall (MkCallDisconnected callStateVar) = do
    callData <- getCallDataVar callStateVar
    liftIO $ atomically $ writeTVar callStateVar (CallIdleState callData Nothing )
    return $ MkCallIdle callStateVar
      
  clearReleasedCall _ = return ()

  getOfferedCallSDP (MkCallOffered callStateVar) = do
    callState <- liftIO $ readTVarIO callStateVar
    case callState of 
      CallOfferedState _ sdpOffer _ -> return sdpOffer
      _ -> throwIO $ JanusCallException "getOfferedCallSDP at not-offered state"


  getAnsweredCallSDP (MkCallAnswered callStateVar) = do
    callState <- liftIO $ readTVarIO callStateVar
    case callState of 
      CallAnsweredState _ sdpAnswer _ -> return sdpAnswer 
      _ -> throwIO $ JanusCallException "getAnsweredCallSDP at not-offered state"


  idleTransition (MkCallIdle callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallIdleState callData _ ->  CallIdleState callData (Just cb)
        unexpState -> unexpState
      ) 
    return True

  dialingTransition (MkCallDialing callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallDialingState callData _ ->  CallDialingState callData (Just cb)
        unexpState -> unexpState
      ) 
    return True

  answeringTransition (MkCallAnswering callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallAnsweringState callData _ ->  CallAnsweringState callData (Just cb)
        unexpState -> unexpState
      ) 
    return True

  offeredTransition (MkCallOffered callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallOfferedState callData sdp _ ->  CallOfferedState callData sdp (Just cb)
        unexpState -> unexpState
      ) 
    return True

  answeredTransition (MkCallAnswered callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallAnsweredState callData sdp _ ->  CallAnsweredState callData sdp (Just cb)
        unexpState -> unexpState
      ) 
    return True
  
  connectedTransition (MkCallConnected callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallConnectedState callData _ ->  CallConnectedState callData (Just cb)
        unexpState -> unexpState
      ) 
    return True

{-    
getServerHandlerVar :: (MonadIO m) => TVar (CallState m) -> m ServerHandler
getServerHandlerVar callStateVar =  do
  callState <- liftIO $ readTVarIO callStateVar
  case getCallData callState of
    Just callData -> return (serverHandler callData)
    Nothing -> throwIO $ JanusCallException "No callData available"
-}

getCallDataVar :: (MonadIO m) => TVar (CallState m) -> m CallData
getCallDataVar callStateVar = do
  callState <- liftIO $ readTVarIO callStateVar
  case getCallData callState of
    Just callData -> return callData
    Nothing -> throwIO $ JanusCallException "No callData available"
    
getCallData :: CallState m -> Maybe CallData 
getCallData (CallIdleState callData _) = Just callData 
getCallData (CallDialingState callData _) = Just callData 
getCallData (CallOfferedState callData _ _) = Just callData 
getCallData (CallAnsweredState callData _ _) = Just callData 
getCallData (CallConnectedState callData _) = Just callData 
getCallData (CallAnsweringState callData _) = Just callData 
getCallData (CallDisconnectedState callData) = Just callData 
getCallData CallReleasedState = Nothing
getCallData NoCallState = Nothing   

dropCallInternal :: MonadIO m => TVar (CallState m) -> m (CallDisconnectedData m JanusCallPars)
dropCallInternal callStateVar = do
  callData <- getCallDataVar callStateVar
  liftIO $ sendJanusRequest JanusHangupReq (serverHandler callData)  
  liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
  return $ MkCallDisconnected callStateVar

rejectCallInternal :: MonadIO m => TVar (CallState m) -> m (CallDisconnectedData m JanusCallPars)
rejectCallInternal callStateVar = do
  callData <- getCallDataVar callStateVar
  liftIO $ sendJanusRequest (rejectCallMsgFunc callData) (serverHandler callData)  
  liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
  return $ MkCallDisconnected callStateVar
  
releaseCallInternal :: MonadIO m => TVar (CallState m) -> m (CallReleasedData m JanusCallPars)
releaseCallInternal callStateVar = do
  callData <- getCallDataVar callStateVar
  liftIO $ sendJanusRequest JanusDetachHandle (serverHandler callData)  
  liftIO $ atomically $ writeTVar callStateVar CallReleasedState
  return $ MkCallReleased
  
dropAndReleaseCallInternal :: MonadIO m => TVar (CallState m) -> m (CallReleasedData m JanusCallPars)
dropAndReleaseCallInternal callStateVar = do
  void $ dropCallInternal callStateVar
  releaseCallInternal callStateVar

janusCallback :: MonadIO m => (m Bool -> IO ()) -> TVar (CallState m) -> JanusServerMsg -> IO ()
janusCallback taskPoster callStateVar serverMsg = 
  taskPoster (serverMsgHandler callStateVar serverMsg)

-- invoke in context of processing thread  
serverMsgHandler :: MonadIO m => TVar (CallState m) -> JanusServerMsg -> m Bool
serverMsgHandler callStateVar serverMsg = do 
  liftIO $ putStrLn (("serverMsgHandler:"::Text) <> show serverMsg) 
  callState <- liftIO $ readTVarIO callStateVar
  case (callState , serverMsg) of 
    (CallIdleState callData (Just cb), JanusIncomingCall (Just offer)) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallOfferedState callData (SDPOffer offer) Nothing)
        cb (toVariant $ MkCallOffered callStateVar) 
    (CallOfferedState callData _ (Just cb), JanusHangupEvent) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
        cb (toVariant $ MkCallDisconnected callStateVar) 
    (CallDialingState callData (Just cb), JanusHangupEvent) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
        cb (toVariant $ MkCallDisconnected callStateVar) 
    (CallAnsweredState callData _ (Just cb), JanusHangupEvent) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
        cb (toVariant $ MkCallDisconnected callStateVar) 
    (CallAnsweringState callData (Just cb), JanusHangupEvent) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
        cb (toVariant $ MkCallDisconnected callStateVar) 
    (CallConnectedState callData (Just cb), JanusHangupEvent) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallDisconnectedState callData)
        cb (toVariant $ MkCallDisconnected callStateVar) 
    (CallAnsweredState callData _ (Just cb), JanusWebRtcUp) ->
      do 
        -- liftIO $ putStrLn ("********* Switch to connected state from answered state"::Text)
        liftIO $ atomically $ writeTVar callStateVar (CallConnectedState callData Nothing)
        cb (toVariant $ MkCallConnected callStateVar) 
    (CallAnsweringState callData (Just cb), JanusWebRtcUp) ->
      do 
        -- liftIO $ putStrLn ("********* Switch to connected state from answering state"::Text)
        liftIO $ atomically $ writeTVar callStateVar (CallConnectedState callData Nothing)
        cb (toVariant $ MkCallConnected callStateVar) 
    (CallDialingState callData (Just cb), JanusCallProgressEvent  Accepted (Just answer)) ->
      do 
        liftIO $ atomically $ writeTVar callStateVar (CallAnsweredState callData (SDPAnswer answer) Nothing)
        cb (toVariant $ MkCallAnswered callStateVar) 
    (_,JanusWebRtcUp) -> do 
      liftIO $ putStrLn ("********* Disregarding WebRtcUp !!!! "::Text)
      return True    
    (_,_) -> return True    

sipCallRes :: ConHandle -> JanusRegisterReqPs -> JanusCallRes
sipCallRes conHandle regReqPs = 
  CallRes $
    JanusCallPars 
      { connectionHandle = conHandle
      , pluginType = Sip
      , callRegMsgPar = JanusRegisterReq regReqPs
      , makeCallMsgPar = (\dest (SDPOffer offerTxt) -> JanusCallReq (JanusCallReqPs dest offerTxt))     
      , rejectCallMsgPar = JanusDeclineReq 
      }

videoCallRes :: ConHandle -> Text -> JanusCallRes
videoCallRes conHandle userName = 
  CallRes $
    JanusCallPars 
      { connectionHandle = conHandle
      , pluginType = VideoCall 
      , callRegMsgPar = JanusVideoCallRegReq (JanusVideoCallRegReqPs userName)      
      , makeCallMsgPar = (\dest (SDPOffer offerTxt) -> JanusVideoCallReq (JanusVideoCallReqPs dest offerTxt))     
      , rejectCallMsgPar = JanusHangupReq 
      }
      
      