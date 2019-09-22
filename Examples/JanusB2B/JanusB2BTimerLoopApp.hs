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

module JanusB2BTimerLoopApp where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Control
import            Beseder.Misc.Misc
import            Beseder.Utils
import            Data.String
import            Beseder.Resources.Timer
import            Control.Monad.Cont (ContT)
import            Control.Monad.Identity (IdentityT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, gets, First)


b2bTrans :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
    CallRes par1 -> CallRes par2 -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ ()      
b2bTrans callRes1 callRes2 dest timeout1 timeout2 = do 
  newRes #call1 callRes1
  while (callHandler callRes2 dest timeout1 timeout2 >> return True) 

callHandler :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
                  CallRes par2 -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[CallIdle TaskQ par1 "call1"] _ _ ()
callHandler callRes2 dest timeout1 timeout2 = do    -- [(call1/Idle)]
  reach @("call1" :? IsCallOffered) nextEv          -- [(call1/Offered)]
  sdpOffer <- opRes #call1 getSDPOffer                                               
  try @("call1" :? IsCallAlive) $ do                -- break out when call1 disconnects
    newRes #call2 callRes2                          -- [(call1/Offered, call2/Idle)] 
    invoke #call2 (MakeCall dest sdpOffer)          -- [(call1/Offered, call2/Dialing]
    newRes #timerCalling TimerRes                   -- [(call1/Offered, call2/Dialing,t1/NotArmed)]
    invoke #timerCalling (StartTimer timeout1)      -- [(call1/Offered, call2/Dialing,t1/Armed)]
    newRes #timerConnected TimerRes                 -- [(call1/Offered, call2/Dialing,t1/Armed,t2/NotArmed)]
    try @("call2" :? IsCallAlive                    -- break out when call1 disconnects, or one of the timers goes off
      :&& (Not ("timerCalling" :? IsTimerTriggered))
      :&& (Not ("timerConnected" :? IsTimerTriggered))) $ do
        handleEvents $ do
          on @("call1" :? IsCallOffered :&& "call2" :? IsCallAnswered) $ do
            invoke #timerCalling StopTimer
            sdpAnswr <- opRes #call2 getSDPAnswer
            invoke #call1 (AnswerCall sdpAnswr)
          on @("call1" :? IsCallConnected :&& "call2" :? IsCallConnected) $ do
            invoke #timerConnected (StartTimer timeout2)
  clear #call2
  clear #timerCalling
  clear #timerConnected
  on @("call1" :? IsCallAlive) (invoke #call1 DropCall)
  invoke #call1 ResetCall                           -- [(call1/Idle)]

{-
[ (call1/Offered,call2/Answered, t1/Armed, t2/NotArmed)
, (call1/Answered,call2/Answered, t1/Stopped, t2/NotArmed)
, (call1/Connected, call2/Answered, t1/Stopped, t2/NotArmed)
, (call1/Answering, call2/Connected, t1/Stopped, t2/NotArmed)
, (call1/Connected, call2/Connected, t1/Stopped, t2/Armed)
]
-}

{-
!!! Renamer/typechecker [JanusB2BTimerLoopApp]: finished in 1012109.38 milliseconds, allocated 223199.108 megabytes
*** Desugar [JanusB2BTimerLoopApp]:
Result size of Desugar (before optimization)
  = {terms: 7,253,
     types: 710,734,
     coercions: 993,934,058,
     joins: 0/2,447}
Result size of Desugar (after optimization)
  = {terms: 4,269,
     types: 453,490,
     coercions: 961,107,451,
     joins: 0/962}
!!! Desugar [JanusB2BTimerLoopApp]: finished in 4448500.00 milliseconds, allocated 750768.610 megabytes
-}  

{-
    * Found type wildcard `_'
        standing for '( '[St
                            (CallIdleData
                               (ReaderT
                                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel IO)
                               par1)
                            "call1"],
                        '[]) :: ([*], [*])
-}
{-
      '[(St
          (CallOfferedData
              (ReaderT
                beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                IO)
              par1)
          "call1",
        (St
            (CallDialingData
              (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO)
              par2)
            "call2",
          (St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerArmedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerCalling",
          St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerNotArmedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerConnected"))),
        (St
          (CallAnsweringData
              (ReaderT
                beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                IO)
              par1)
          "call1",
        (St
            (CallAnsweredData
              (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO)
              par2)
            "call2",
          (St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerStoppedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerCalling",
          St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerNotArmedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerConnected"))),
        (St
          (CallConnectedData
              (ReaderT
                beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                IO)
              par1)
          "call1",
        (St
            (CallAnsweredData
              (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO)
              par2)
            "call2",
          (St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerStoppedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerCalling",
          St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerNotArmedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerConnected"))),
        (St
          (CallAnsweringData
              (ReaderT
                beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                IO)
              par1)
          "call1",
        (St
            (CallConnectedData
              (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO)
              par2)
            "call2",
          (St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerStoppedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerCalling",
          St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerNotArmedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerConnected"))),
        (St
          (CallConnectedData
              (ReaderT
                beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                IO)
              par1)
          "call1",
        (St
            (CallConnectedData
              (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO)
              par2)
            "call2",
          (St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerStoppedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerCalling",
          St
            (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerArmedEvData
                (ReaderT
                  beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                  IO))
            "timerConnected")))])-}