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

module JanusB2BTimerApp where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Resources.Timer
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Control
import            Beseder.Misc.Misc
import            Control.Monad.Cont (ContT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, gets, First)


b2bTrans :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
    CallRes par1 -> CallRes par2 -> Text -> Int -> STransApp (ContT Bool) TaskQ NoSplitter '[()] _  ()      
b2bTrans callRes1 callRes2 dest timeoutSec = MkApp $ do 
  newRes #call1 callRes1
  while $ do
    reach @("call1" :? IsCallOffered) nextEv
    sdpOffer <- opRes #call1 getSDPOffer
    try @("call1" :? IsCallAlive) $ do 
      newRes #call2 callRes2
      invoke #call2 (MakeCall dest sdpOffer) 
      try @("call2" :? IsCallAlive) $ do
        try @(Not ("timer" :? IsTimerTriggered)) $ do
          newRes #timer TimerRes
          invoke #timer (StartTimer timeoutSec)
          nextEv
        on @("timer" :? IsTimerTriggered) 
          (invoke #call2 DropCall)
        clear #timer
        sdpAnswr <- opRes #call2 getSDPAnswer
        invoke #call1 (AnswerCall sdpAnswr)
        reach @("call2" :? IsCallConnected :&& "call1" :? IsCallConnected) 
          (nextEv >> nextEv)
        _t1 :: _ <- whatNext
        nextEv
    on @(By "timer") (clear #timer)  
    on @(By "call2") (clear #call2)  
    on @("call1" :? IsCallAlive) (invoke #call1 DropCall)
    invoke #call1 ResetCall
    return True  
  clear #call1  


