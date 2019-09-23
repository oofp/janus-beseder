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
{-# LANGUAGE LambdaCase   #-}

module  Beseder.Janus.JanusCallProv where

import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Utils
import            Protolude

--import            Comm.Janus.JanusConnector
--import            Beseder.Base.Control
--import            Beseder.Misc.Misc

newtype CallRes pars = CallRes pars deriving Show

data MakeCall = MakeCall 
  { makeCallDest :: Text
  , makeCallSDP :: SDPOffer
  } deriving (Show, Eq)

data DropCall = DropCall deriving (Show, Eq)
data ResetCall = ResetCall deriving (Show, Eq)
data ReleaseCall = ReleaseCall deriving (Show, Eq)
instance GetInstance DropCall where getInstance = DropCall
instance GetInstance ReleaseCall where getInstance = ReleaseCall
instance GetInstance ResetCall where getInstance = ResetCall

newtype SDPOffer = SDPOffer Text deriving (Show,Eq)
newtype SDPAnswer = SDPAnswer Text deriving (Show,Eq)

data AnswerCall = AnswerCall 
  { sdpAnswer :: SDPAnswer 
  } deriving (Show, Eq)

class CallProv (m :: * -> *) pars where
  data CallIdleData m pars
  data CallOfferedData m pars
  data CallAnsweringData m pars
  data CallDialingData m pars
  data CallAnsweredData m pars
  data CallConnectedData m pars 
  data CallDisconnectedData m pars
  data CallReleasedData m pars

  createCall :: pars -> m (CallIdleData m pars)
  answerCall :: AnswerCall -> CallOfferedData m pars -> m (V '[CallAnsweringData m pars, CallDisconnectedData m pars])  
  makeCall :: MakeCall -> CallIdleData m pars -> m (V '[CallDialingData m pars, CallDisconnectedData m pars])  
  dropDialingCall :: DropCall -> CallDialingData m pars -> m (CallDisconnectedData m pars)
  dropConnectedCall :: DropCall -> CallConnectedData m pars -> m (CallDisconnectedData m pars)
  dropOfferedCall :: DropCall -> CallOfferedData m pars -> m (CallDisconnectedData m pars)
  dropAnsweringCall :: DropCall -> CallAnsweringData m pars -> m (CallDisconnectedData m pars)
  dropAnsweredCall :: DropCall -> CallAnsweredData m pars -> m (CallDisconnectedData m pars)
  resetDisconnectedCall :: ResetCall -> CallDisconnectedData m pars -> m (CallIdleData m pars)
  releaseIdleCall :: ReleaseCall -> CallIdleData m pars -> m (CallReleasedData m pars)
  releaseDialingCall :: ReleaseCall -> CallDialingData m pars -> m (CallReleasedData m pars)
  releaseConnectedCall :: ReleaseCall -> CallConnectedData m pars -> m (CallReleasedData m pars)
  releaseOfferedCall :: ReleaseCall -> CallOfferedData m pars -> m (CallReleasedData m pars)
  releaseAnsweringCall :: ReleaseCall -> CallAnsweringData m pars -> m (CallReleasedData m pars)
  releaseAnsweredCall :: ReleaseCall -> CallAnsweredData m pars -> m (CallReleasedData m pars)
  releaseDisconnectedCall :: ReleaseCall -> CallDisconnectedData m pars -> m (CallReleasedData m pars)

  clearReleasedCall ::  CallReleasedData m pars -> m ()
  
  idleTransition :: CallTrans m (CallIdleData m pars) (IdleTransitionNextStates m pars)
  dialingTransition :: CallTrans m (CallDialingData m pars) (DialingTransitionNextStates m pars)
  connectedTransition ::  CallTrans m (CallConnectedData m pars) '[CallDisconnectedData m pars]
  offeredTransition ::  CallTrans m (CallOfferedData m pars) '[CallDisconnectedData m pars]
  answeringTransition ::  CallTrans m (CallAnsweringData m pars) (AnsweringTransitionNextStates m pars)
  answeredTransition ::  CallTrans m (CallAnsweredData m pars) (AnsweredTransitionNextStates m pars)

  getOfferedCallSDP :: CallOfferedData m pars -> m SDPOffer
  getAnsweredCallSDP :: CallAnsweredData m pars -> m SDPAnswer

type IdleTransitionNextStates m pars = 
 '[ CallOfferedData m pars 
  ]   
  
type DialingTransitionNextStates m pars = 
 '[ CallAnsweredData m pars 
  , CallDisconnectedData m pars
  ]   

type OfferedTransitionNextStates m pars = 
 '[ CallDisconnectedData m pars
  ]   

type AnsweringTransitionNextStates m pars = 
 '[ CallConnectedData m pars 
  , CallDisconnectedData m pars
  ]   
     
type AnsweredTransitionNextStates m pars = 
 '[ CallConnectedData m pars 
  , CallDisconnectedData m pars
  ]   
       
type ConnectedTransitionNextStates m pars = 
 '[ CallDisconnectedData m pars
  ]   
    
type CallTrans m fromState toStates = fromState -> (V toStates -> m Bool) -> m Bool   

type CallIdle m pars name = St (CallIdleData m pars) name
type CallDialing m pars name = St (CallDialingData m pars) name
type CallConnected m pars name = St (CallConnectedData m pars) name
type CallDisconnected m pars name = St (CallDisconnectedData m pars) name
type CallOffered m pars name = St (CallOfferedData m pars) name
type CallAnswering m pars name = St (CallAnsweringData m pars) name
type CallAnswered m pars name = St (CallAnsweredData m pars) name
type CallReleased m pars name = St (CallReleasedData m pars) name

type family IsCallConnectedFam a :: Bool where
  IsCallConnectedFam (CallConnected m pars name) = 'True
  IsCallConnectedFam _ = 'False
data IsCallConnected :: Type -> Exp Bool 
type instance Eval (IsCallConnected a) = IsCallConnectedFam a

type family IsCallOfferedFam a :: Bool where
  IsCallOfferedFam (CallOffered m pars name) = 'True
  IsCallOfferedFam _ = 'False
data IsCallOffered :: Type -> Exp Bool 
type instance Eval (IsCallOffered a) = IsCallOfferedFam a

type family IsCallAnsweredFam a :: Bool where
  IsCallAnsweredFam (CallAnswered m pars name) = 'True
  IsCallAnsweredFam _ = 'False
data IsCallAnswered :: Type -> Exp Bool 
type instance Eval (IsCallAnswered a) = IsCallAnsweredFam a

type family IsCallAliveFam a :: Bool where
  IsCallAliveFam (CallConnected m pars name) = 'True
  IsCallAliveFam (CallOffered m pars name) = 'True
  IsCallAliveFam (CallAnswered m pars name) = 'True
  IsCallAliveFam (CallAnswering m pars name) = 'True
  IsCallAliveFam (CallDialing m pars name) = 'True
  IsCallAliveFam _ = 'False
data IsCallAlive :: Type -> Exp Bool 
type instance Eval (IsCallAlive a) = IsCallAliveFam a

type family IsCallDisconnectedFam a :: Bool where
  IsCallDisconnectedFam (CallDisconnected m pars name) = 'True
  IsCallDisconnectedFam _ = 'False
data IsCallDisconnected :: Type -> Exp Bool 
type instance Eval (IsCallDisconnected a) = IsCallDisconnectedFam a

type instance StateTrans (CallIdle m pars name) = 'Dynamic
type instance StateTrans (CallDialing m pars name) = 'Dynamic
type instance StateTrans (CallConnected m pars name) = 'Dynamic
type instance StateTrans (CallOffered m pars name) = 'Dynamic
type instance StateTrans (CallAnswered m pars name) = 'Dynamic
type instance StateTrans (CallAnswering m pars name) = 'Dynamic
type instance StateTrans (CallReleased m pars name) = 'Static
type instance StateTrans (CallDisconnected m pars name) = 'Static

type instance TermRequest (CallIdle m pars name) = ReleaseCall
type instance TermRequest (CallDialing m pars name) = ReleaseCall
type instance TermRequest (CallOffered m pars name) = ReleaseCall
type instance TermRequest (CallAnswering m pars name) = ReleaseCall
type instance TermRequest (CallAnswered m pars name) = ReleaseCall
type instance TermRequest (CallConnected m pars name) = ReleaseCall
type instance TermRequest (CallDisconnected m pars name) = ReleaseCall

instance (CallProv m pars, MonadIO m) => TermState m (CallReleased m pars name) where
  terminate (St callReleasedData) = clearReleasedCall callReleasedData

instance (CallProv m pars, MonadIO m) => TermState m (CallDisconnected m pars name) where
  terminate (St callDisconnectedData) = releaseDisconnectedCall ReleaseCall callDisconnectedData >>= clearReleasedCall 
  
instance (CallProv m pars, MonadIO m) => Transition m (CallDialing m pars name) where
  type NextStates (CallDialing m pars name) = ListOfNamed St name (DialingTransitionNextStates m pars) 
  next st@(St callState) cb = dialingTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallIdle m pars name) where
  type NextStates (CallIdle m pars name) = ListOfNamed St name (IdleTransitionNextStates m pars) 
  next st@(St callState) cb = idleTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallConnected m pars name) where
  type NextStates (CallConnected m pars name) = ListOfNamed St name (ConnectedTransitionNextStates m pars) 
  next st@(St callState) cb = connectedTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallOffered m pars name) where
  type NextStates (CallOffered m pars name) = ListOfNamed St name (OfferedTransitionNextStates m pars) 
  next st@(St callState) cb = offeredTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallAnswering m pars name) where
  type NextStates (CallAnswering m pars name) = ListOfNamed St name (AnsweringTransitionNextStates m pars) 
  next st@(St callState) cb = answeringTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallAnswered m pars name) where
  type NextStates (CallAnswered m pars name) = ListOfNamed St name (AnsweredTransitionNextStates m pars) 
  next st@(St callState) cb = answeredTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   
    
instance (CallProv m pars, MonadIO m) => MkRes m (CallRes pars) where
  type ResSt m (CallRes pars) = CallIdleData m pars 
  mkRes (CallRes pars) = createCall pars

instance (CallProv m pars, MonadIO m) => Request m MakeCall (CallIdle m pars name) where
  type ReqResult MakeCall (CallIdle m pars name) = '[CallDialing m pars name,CallDisconnected m pars name] 
  request makeCallReq st@(St callData) = fmap (toVarOfSt (nameFromSt st)) (makeCall makeCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m DropCall (CallDialing m pars name) where
  type ReqResult DropCall (CallDialing m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropDialingCall dropCallReq callData)
  
instance (CallProv m pars, MonadIO m) => Request m DropCall (CallConnected m pars name) where
  type ReqResult DropCall (CallConnected m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropConnectedCall dropCallReq callData)
      
instance (CallProv m pars, MonadIO m) => Request m DropCall (CallOffered m pars name) where
  type ReqResult DropCall (CallOffered m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropOfferedCall dropCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m DropCall (CallAnswered m pars name) where
  type ReqResult DropCall (CallAnswered m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropAnsweredCall dropCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m DropCall (CallAnswering m pars name) where
  type ReqResult DropCall (CallAnswering m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropAnsweringCall dropCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m AnswerCall (CallOffered m pars name) where
  type ReqResult AnswerCall (CallOffered m pars name) = '[CallAnswering m pars name, CallDisconnected m pars name] 
  request answerCallReq st@(St callData) =  fmap (toVarOfSt (nameFromSt st)) (answerCall answerCallReq callData) 

instance (CallProv m pars, MonadIO m) => Request m ReleaseCall (CallAnswering m pars name) where
  type ReqResult ReleaseCall (CallAnswering m pars name) = '[CallReleased m pars name] 
  request releaseCallReq (St callData) =  fmap (variantFromValue . St) (releaseAnsweringCall releaseCallReq callData)
  
instance (CallProv m pars, MonadIO m) => Request m ReleaseCall (CallIdle m pars name) where
  type ReqResult ReleaseCall (CallIdle m pars name) = '[CallReleased m pars name] 
  request releaseCallReq (St callData) =  fmap (variantFromValue . St) (releaseIdleCall releaseCallReq callData)
    
instance (CallProv m pars, MonadIO m) => Request m ReleaseCall (CallAnswered m pars name) where
  type ReqResult ReleaseCall (CallAnswered m pars name) = '[CallReleased m pars name] 
  request releaseCallReq (St callData) =  fmap (variantFromValue . St) (releaseAnsweredCall releaseCallReq callData)
      
instance (CallProv m pars, MonadIO m) => Request m ReleaseCall (CallConnected m pars name) where
  type ReqResult ReleaseCall (CallConnected m pars name) = '[CallReleased m pars name] 
  request releaseCallReq (St callData) =  fmap (variantFromValue . St) (releaseConnectedCall releaseCallReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m ReleaseCall (CallDisconnected m pars name) where
  type ReqResult ReleaseCall (CallDisconnected m pars name) = '[CallReleased m pars name] 
  request releaseCallReq (St callData) =  fmap (variantFromValue . St) (releaseDisconnectedCall releaseCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m ReleaseCall (CallDialing m pars name) where
  type ReqResult ReleaseCall (CallDialing m pars name) = '[CallReleased m pars name] 
  request releaseCallReq (St callData) =  fmap (variantFromValue . St) (releaseDialingCall releaseCallReq callData)
    
instance (CallProv m pars, MonadIO m) => Request m ResetCall (CallDisconnected m pars name) where
  type ReqResult ResetCall (CallDisconnected m pars name) = '[CallIdle m pars name] 
  request resetCallReq (St callData) =  fmap (variantFromValue . St) (resetDisconnectedCall resetCallReq callData)
      
getSDPOffer :: CallProv m pars => CallOffered m pars name -> m SDPOffer
getSDPOffer (St callData) = getOfferedCallSDP callData 

getSDPAnswer :: CallProv m pars => CallAnswered m pars name -> m SDPAnswer
getSDPAnswer (St callData) = getAnsweredCallSDP callData
