{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedLabels      #-}

module JanusB2BTimerSkip2App where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Resources.Timer
import            Beseder.Base.Control
import            Beseder.Misc.Misc
import            Control.Monad.Cont (ContT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO, wait,
                                               (>>), (>>=), forever, until,try,on, gets, First)

callHandler :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
  CallRes par2 -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[CallIdle TaskQ par1 "call1"] '(('[CallIdle TaskQ par1 "call1"]),'[]) _ () -- '('[CallIdle TaskQ par1 "call1"],('[])) ()
callHandler callRes2 dest timeoutSec1 timeoutSec2 = do
  skipTo @("call1" :? IsCallOffered) 
  sdpOffer <- opRes #call1 getSDPOffer
  try @("call1" :? IsCallAlive) $ do 
    newRes #call2 callRes2
    invoke #call2 (MakeCall dest sdpOffer) 
    newRes #timer1 TimerRes
    invoke #timer1 (StartTimer timeoutSec1)
    newRes #timer2 TimerRes
    try @("call2" :? IsCallAlive 
          :&& (Not ("timer1" :? IsTimerTriggered))
          :&& (Not ("timer2" :? IsTimerTriggered))) $ do
      skipTo @("call2" :? IsCallAnswered)
      invoke #timer1 StopTimer
      sdpAnswr <- opRes #call2 getSDPAnswer
      invoke #call1 (AnswerCall sdpAnswr)
      skipTo @("call2" :? IsCallConnected :&& "call1" :? IsCallConnected) 
      invoke #timer2 (StartTimer timeoutSec2)
      _t1 :: _ <- whatNext -- [(call1/Connected, call2/Connected, t1/Stopped, t2/Armed)]
      wait
  on @(By "timer1") (clear #timer1)  
  on @(By "timer2") (clear #timer2)  
  on @(By "call2") (clear #call2)  
  on @("call1" :? IsCallAlive) (invoke #call1 DropCall)
  invoke #call1 ResetCall

b2bTrans :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
    CallRes par1 -> CallRes par2 -> Text -> Int -> Int ->
      STransApp (ContT Bool) TaskQ NoSplitter '[()] '(('[()]),'[]) ()      
b2bTrans callRes1 callRes2 dest timeoutCalling timeoutConnected = MkApp $ do 
  newRes #call1 callRes1
  while $ do
    callHandler callRes2 dest timeoutCalling timeoutConnected
    return True
  clear #call1  
