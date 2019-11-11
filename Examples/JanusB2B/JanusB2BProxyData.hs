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
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module JanusB2BProxyData where

import            Beseder.Janus.JanusCallProv
import            Beseder.Resources.Timer
import            Beseder.Base.Common
import            Beseder.Base.ControlData
import            Beseder.Misc.Misc
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO, wait,
                                               (>>), (>>=), forever, until,try,on, gets, First)

handleIncomingCall :: forall src dest. (_) => 
    Named src -> Named dest -> Text -> Int -> Int -> STransData TaskQ NoSplitter _  () -- '[(CallOffered TaskQ par1 src, CallIdle TaskQ par2 dest)] _ _ _ ()
handleIncomingCall srcN destN destAddress timeoutSec1 timeoutSec2 = do
    sdpOffer <- opRes srcN getSDPOffer -- (CallOffered m pars src -> m SDPOffer)) 
    try @(src :? IsCallAlive) $ do 
      invoke destN (MakeCall destAddress sdpOffer)
      newRes #timer1 TimerRes
      invoke #timer1 (StartTimer timeoutSec1)
      newRes #timer2 TimerRes
      try @(dest :? IsCallAlive 
            :&& (Not ("timer1" :? IsTimerTriggered))
            :&& (Not ("timer2" :? IsTimerTriggered))) $ do 
        skipTo @(dest :? IsCallAnswered)
        invoke #timer1 StopTimer
        sdpAnswr <- opRes destN getSDPAnswer
        invoke srcN (AnswerCall sdpAnswr)
        skipTo @(src :? IsCallConnected :&& dest :? IsCallConnected) 
        invoke #timer2 (StartTimer timeoutSec2)
        skipAll
    on @(By "timer1") (clear #timer1)  
    on @(By "timer2") (clear #timer2)  


-- evalHandleIncomingCall = evalSTransData' handleIncomingCall' (Proxy @(IncomingCallState))

--interpretHandleIncomingCall :: STrans (ContT Bool) TaskQ NoSplitter IncomingCallState _ _ _ ()
--interpretHandleIncomingCall = interpret handleIncomingCall'
