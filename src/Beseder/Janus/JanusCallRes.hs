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
{-# LANGUAGE TemplateHaskell       #-}

module  Beseder.Janus.JanusCallRes where

import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Resources.ResourceDef
import            Protolude

--newtype CallRes pars = CallRes pars deriving Show

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

class Monad m => CallRes m res where
  data CallIdle m res
  data CallOffered m res
  data CallAnswering m res
  data CallDialing m res
  data CallAnswered m res
  data CallConnected m res 
  data CallDisconnected m res
  data CallReleased m res

  data  ResPar m res 

  createCall :: MkResDef m (ResPar m res) (CallIdle m res)
  answerCall :: RequestDef m AnswerCall (CallOffered m res) '[CallAnswering m res, CallDisconnected m res]
  makeCall :: RequestDef m MakeCall (CallIdle m res) '[CallDialing m res, CallDisconnected m res]
  dropDialingCall :: RequestDef m DropCall (CallDialing m res) '[CallDisconnected m res]
  dropConnectedCall :: RequestDef m DropCall (CallConnected m res) '[CallDisconnected m res]
  dropOfferedCall :: RequestDef m DropCall (CallOffered m res) '[CallDisconnected m res]
  dropAnsweringCall :: RequestDef m DropCall (CallAnswering m res) '[CallDisconnected m res]
  dropAnsweredCall :: RequestDef m DropCall (CallAnswered m res) '[CallDisconnected m res]
  resetDisconnectedCall :: RequestDef m ResetCall (CallDisconnected m res) '[CallIdle m res]
  releaseIdleCall :: RequestDef m ReleaseCall (CallIdle m res) '[CallReleased m res]
  releaseDialingCall :: RequestDef m ReleaseCall (CallDialing m res) '[CallReleased m res]
  releaseConnectedCall :: RequestDef m ReleaseCall (CallConnected m res) '[CallReleased m res]
  releaseOfferedCall :: RequestDef m ReleaseCall (CallOffered m res) '[CallReleased m res]
  releaseAnsweringCall :: RequestDef m ReleaseCall (CallAnswering m res) '[CallReleased m res]
  releaseAnsweredCall :: RequestDef m ReleaseCall (CallAnswered m res) '[CallReleased m res]
  releaseDisconnectedCall :: RequestDef m ReleaseCall (CallDisconnected m res) '[CallReleased m res]

  clearReleasedCall ::  TermDef m (CallReleased m res)
  
  idleTransition :: TransitionDef m (CallIdle m res) '[CallOffered m res]
  dialingTransition :: TransitionDef m (CallDialing m res) '[ CallAnswered m res , CallDisconnected m res]   
  connectedTransition ::  TransitionDef m (CallConnected m res) '[CallDisconnected m res]
  offeredTransition ::  TransitionDef m (CallOffered m res) '[CallDisconnected m res]
  answeringTransition ::  TransitionDef m (CallAnswering m res) '[CallConnected m res , CallDisconnected m res]
  answeredTransition ::  TransitionDef m (CallAnswered m res) '[CallConnected m res , CallDisconnected m res]

  _offeredCallSDP :: CallOffered m res-> m SDPOffer
  _answeredCallSDP :: CallAnswered m res -> m SDPAnswer

buildRes ''CallRes


type family IsCallAliveFam a :: Bool where
  IsCallAliveFam (StCallConnected m res name) = 'True
  IsCallAliveFam (StCallOffered m res name) = 'True
  IsCallAliveFam (StCallAnswered m res name) = 'True
  IsCallAliveFam (StCallAnswering m res name) = 'True
  IsCallAliveFam (StCallDialing m res name) = 'True
  IsCallAliveFam _ = 'False
data IsCallAlive :: Type -> Exp Bool 
type instance Eval (IsCallAlive a) = IsCallAliveFam a

      
getSDPOffer :: CallRes m res => StCallOffered m res name -> m SDPOffer
getSDPOffer (St callData) = _offeredCallSDP callData 

getSDPAnswer :: CallRes m res => StCallAnswered m res name -> m SDPAnswer
getSDPAnswer (St callData) = _answeredCallSDP callData
