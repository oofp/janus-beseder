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
import            Data.String
import            Control.Monad.Cont (ContT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO, wait,
                                               (>>), (>>=), forever, until,try,on, gets, First)

callHandler :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
  CallRes par2 -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[CallIdle TaskQ par1 "call1"] '[CallIdle TaskQ par1 "call1"] '[] _ () -- '('[CallIdle TaskQ par1 "call1"],('[])) ()
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
      liftIO $ putStrLn ("****** Answer incoming call"::Text)
      skipTo @("call2" :? IsCallConnected :&& "call1" :? IsCallConnected) 
      invoke #timer2 (StartTimer timeoutSec2)
      _t1 :: _ <- whatNext -- [(call1/Connected, call2/Connected, t1/Stopped, t2/Armed)]
      liftIO $ putStrLn ("****** Media path established; 2nd timer started"::Text)
      skipAll
  on @(By "timer1") (clear #timer1)  
  on @(By "timer2") (clear #timer2)  
  on @(By "call2") (clear #call2)  
  on @("call1" :? IsCallAlive) (invoke #call1 DropCall)
  invoke #call1 ResetCall

b2bTrans :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
    CallRes par1 -> CallRes par2 -> Text -> Int -> Int ->
      STransApp (ContT Bool) TaskQ NoSplitter '[()] '[()] '[] ()      
b2bTrans callRes1 callRes2 dest timeoutCalling timeoutConnected = MkApp $ do 
  newRes #call1 callRes1
  while $ do
    callHandler callRes2 dest timeoutCalling timeoutConnected
    return True
  clear #call1  

{-
[1 of 2] Compiling JanusB2BTimerSkip2App
Stable BCO: []
Ready for upsweep
  [NONREC
      ModSummary {
         ms_hs_date = 2019-09-22 16:43:24.1192539 UTC
         ms_mod = JanusB2BTimerSkip2App,
         ms_textual_imps = [(Nothing, Protolude),
                            (Nothing, Control.Monad.Cont), (Nothing, Beseder.Misc.Misc),
                            (Nothing, Beseder.Base.Control),
                            (Nothing, Beseder.Resources.Timer),
                            (Nothing, Beseder.Janus.JanusCallProvImpl),
                            (Nothing, Beseder.Janus.JanusCallProv)]
         ms_srcimps = []
      },
   NONREC
      ModSummary {
         ms_hs_date = 2019-09-22 16:29:20.8239466 UTC
         ms_mod = Main,
         ms_textual_imps = [(Nothing, System.Log.Handler.Simple),
                            (Nothing, System.Log.Formatter), (Nothing, System.Log.Handler),
                            (Nothing, System.Log.Logger), (Nothing, Beseder.Utils),
                            (Nothing, Beseder.Misc.Misc), (Nothing, Beseder.Base.Control),
                            (Nothing, Beseder.Base.Common), (Nothing, Beseder.Base.Base),
                            (Nothing, JanusB2BTimerSkip2App), (Nothing, Data.Text),
                            (Nothing, System.Environment),
                            (Nothing, Comm.Janus.JanusConnector),
                            (Nothing, Beseder.Janus.JanusCallProvImpl),
                            (Nothing, Beseder.Janus.JanusCallProv), (Nothing, Protolude)]
         ms_srcimps = []
      }]
*** Deleting temp files:
compile: input file Examples/JanusB2B\JanusB2BTimerSkip2App.hs
*** Checking old interface for JanusB2BTimerSkip2App (use -ddump-hi-diffs for more details):
*** Parser [JanusB2BTimerSkip2App]:
!!! Parser [JanusB2BTimerSkip2App]: finished in 0.00 milliseconds, allocated 3.503 megabytes
*** Renamer/typechecker [JanusB2BTimerSkip2App]:
!!! Renamer/typechecker [JanusB2BTimerSkip2App]: finished in 1120734.38 milliseconds, allocated 230429.871 megabytes
*** Desugar [JanusB2BTimerSkip2App]:
Result size of Desugar (before optimization)
  = {terms: 8,260,
     types: 679,680,
     coercions: 1,015,238,031,
     joins: 0/2,790}
Result size of Desugar (after optimization)
  = {terms: 4,788,
     types: 435,266,
     coercions: 967,535,107,
     joins: 0/1,061}
!!! Desugar [JanusB2BTimerSkip2App]: finished in 3394328.12 milliseconds, allocated 1103685.042 megabytes

Examples\JanusB2B\JanusB2BTimerSkip2App.hs:22:1: warning: [-Wunused-imports]
    The import of `Beseder.Janus.JanusCallProvImpl' is redundant
      except perhaps to import instances from `Beseder.Janus.JanusCallProvImpl'
    To import instances alone, use: import Beseder.Janus.JanusCallProvImpl()
   |
22 | import            Beseder.Janus.JanusCallProvImpl
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Examples\JanusB2B\JanusB2BTimerSkip2App.hs:31:149: warning: [-Wpartial-type-signatures]
    * Found type wildcard `_'
        standing for `ComposeFunc
                        (EmbedFunc
                           (Dynamics :&& Not ("call1" :? IsCallOffered))
                           (BindFunc
                              IDFunc
                              (BindFunc
                                 IDFunc
                                 (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                    'One GetNextAllFunc))))
                        (BindFunc
                           IDFunc
                           (ComposeFunc
                              (EmbedFunc
                                 ("call1" :? IsCallAlive)
                                 (ComposeFunc
                                    (NewResFunc (CallRes par2) "call2" TaskQ)
                                    (ComposeFunc
                                       (InvokeAllFunc MakeCall "call2")
                                       (ComposeFunc
                                          (NewResFunc TimerRes "timer1" TaskQ)
                                          (ComposeFunc
                                             (InvokeAllFunc StartTimer "timer1")
                                             (ComposeFunc
                                                (NewResFunc TimerRes "timer2" TaskQ)
                                                (EmbedFunc
                                                   (("call2" :? IsCallAlive)
                                                    :&& (Not ("timer1" :? IsTimerTriggered)
                                                         :&& Not ("timer2" :? IsTimerTriggered)))
                                                   (ComposeFunc
                                                      (EmbedFunc
                                                         (Dynamics
                                                          :&& Not ("call2" :? IsCallAnswered))
                                                         (BindFunc
                                                            IDFunc
                                                            (BindFunc
                                                               IDFunc
                                                               (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                  'One GetNextAllFunc))))
                                                      (ComposeFunc
                                                         (InvokeAllFunc StopTimer "timer1")
                                                         (BindFunc
                                                            IDFunc
                                                            (ComposeFunc
                                                               (InvokeAllFunc AnswerCall "call1")
                                                               (ComposeFunc
                                                                  (EmbedFunc
                                                                     (Dynamics
                                                                      :&& Not
                                                                            (("call2"
                                                                              :? IsCallConnected)
                                                                             :&& ("call1"
                                                                                  :? IsCallConnected)))
                                                                     (BindFunc
                                                                        IDFunc
                                                                        (BindFunc
                                                                           IDFunc
                                                                           (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                              ('Succ 'One)
                                                                              GetNextAllFunc))))
                                                                  (ComposeFunc
                                                                     (InvokeAllFunc
                                                                        StartTimer "timer2")
                                                                     (BindFunc
                                                                        IDFunc
                                                                        (BindFunc
                                                                           IDFunc
                                                                           (BindFunc
                                                                              IDFunc
                                                                              (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                 'One
                                                                                 GetNextAllFunc)))))))))))))))))
                              (ComposeFunc
                                 (CaptureFunc (By "timer1") (ClearAllFunc "timer1"))
                                 (ComposeFunc
                                    (CaptureFunc (By "timer2") (ClearAllFunc "timer2"))
                                    (ComposeFunc
                                       (CaptureFunc (By "call2") (ClearAllFunc "call2"))
                                       (ComposeFunc
                                          (CaptureFunc
                                             ("call1" :? IsCallAlive)
                                             (InvokeAllFunc DropCall "call1"))
                                          (InvokeAllFunc ResetCall "call1"))))))) :: *
                                                                                     -> [*]
                                                                                     -> ([*], [*])
                                                                                     -> *'
      Where: `par2' is a rigid type variable bound by
               the inferred type of
                 callHandler :: (CallProv TaskQ par1, CallProv TaskQ par2,
                                 Show par1, Show par2, Show (CallRes par2)) =>
                                CallRes par2
                                -> Text
                                -> Int
                                -> Int
                                -> STrans
                                     (ContT Bool)
                                     TaskQ
                                     NoSplitter
                                     '[CallIdle TaskQ par1 "call1"]
                                     '( '[CallIdle TaskQ par1 "call1"], '[])
                                     (ComposeFunc
                                        (EmbedFunc
                                           (Dynamics :&& Not ("call1" :? IsCallOffered))
                                           (BindFunc
                                              IDFunc
                                              (BindFunc
                                                 IDFunc
                                                 (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                    'One GetNextAllFunc))))
                                        (BindFunc
                                           IDFunc
                                           (ComposeFunc
                                              (EmbedFunc
                                                 ("call1" :? IsCallAlive)
                                                 (ComposeFunc
                                                    (NewResFunc (CallRes par2) "call2" TaskQ)
                                                    (ComposeFunc
                                                       (InvokeAllFunc MakeCall "call2")
                                                       (ComposeFunc
                                                          (NewResFunc TimerRes "timer1" TaskQ)
                                                          (ComposeFunc
                                                             (InvokeAllFunc StartTimer "timer1")
                                                             (ComposeFunc
                                                                (NewResFunc TimerRes "timer2" TaskQ)
                                                                (EmbedFunc
                                                                   (("call2" :? IsCallAlive)
                                                                    :&& (Not
                                                                           ("timer1"
                                                                            :? IsTimerTriggered)
                                                                         :&& Not
                                                                               ("timer2"
                                                                                :? IsTimerTriggered)))
                                                                   (ComposeFunc
                                                                      (EmbedFunc
                                                                         (Dynamics
                                                                          :&& Not
                                                                                ("call2"
                                                                                 :? IsCallAnswered))
                                                                         (BindFunc
                                                                            IDFunc
                                                                            (BindFunc
                                                                               IDFunc
                                                                               (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                  'One
                                                                                  GetNextAllFunc))))
                                                                      (ComposeFunc
                                                                         (InvokeAllFunc
                                                                            StopTimer "timer1")
                                                                         (BindFunc
                                                                            IDFunc
                                                                            (ComposeFunc
                                                                               (InvokeAllFunc
                                                                                  AnswerCall
                                                                                  "call1")
                                                                               (ComposeFunc
                                                                                  (EmbedFunc
                                                                                     (Dynamics
                                                                                      :&& Not
                                                                                            (("call2"
                                                                                              :? IsCallConnected)
                                                                                             :&& ("call1"
                                                                                                  :? IsCallConnected)))
                                                                                     (BindFunc
                                                                                        IDFunc
                                                                                        (BindFunc
                                                                                           IDFunc
                                                                                           (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                              ('Succ
                                                                                                 'One)
                                                                                              GetNextAllFunc))))
                                                                                  (ComposeFunc
                                                                                     (InvokeAllFunc
                                                                                        StartTimer
                                                                                        "timer2")
                                                                                     (BindFunc
                                                                                        IDFunc
                                                                                        (BindFunc
                                                                                           IDFunc
                                                                                           (BindFunc
                                                                                              IDFunc
                                                                                              (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                                 'One
                                                                                                 GetNextAllFunc)))))))))))))))))
                                              (ComposeFunc
                                                 (CaptureFunc (By "timer1") (ClearAllFunc "timer1"))
                                                 (ComposeFunc
                                                    (CaptureFunc
                                                       (By "timer2") (ClearAllFunc "timer2"))
                                                    (ComposeFunc
                                                       (CaptureFunc
                                                          (By "call2") (ClearAllFunc "call2"))
                                                       (ComposeFunc
                                                          (CaptureFunc
                                                             ("call1" :? IsCallAlive)
                                                             (InvokeAllFunc DropCall "call1"))
                                                          (InvokeAllFunc ResetCall "call1"))))))))
                                     ()
               at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:(32,1)-(56,25)
    * In the type signature:
        callHandler :: (CallProv TaskQ par1,
                        CallProv TaskQ par2,
                        Show par1,
                        Show par2) =>
                       CallRes par2
                       -> Text
                          -> Int
                             -> Int
                                -> STrans (ContT Bool) TaskQ NoSplitter '[CallIdle TaskQ par1 "call1"] '(('[CallIdle TaskQ par1 "call1"]),
                                                                                                         '[]) _ ()
   |
31 |   CallRes par2 -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[CallIdle TaskQ par1 "call1"] '(('[CallIdle TaskQ par1 "call1"]),'[]) _ () -- '('[CallIdle TaskQ par1 "call1"],('[])) ()
   |                                                                                                                                                     ^

Examples\JanusB2B\JanusB2BTimerSkip2App.hs:50:14: warning: [-Wpartial-type-signatures]
    * Found type wildcard `_'
        standing for `Proxy
                        '[(beseder-0.1.1.0:Beseder.Base.Internal.Core.St
                             (CallConnectedData
                                (ReaderT
                                   beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                                   IO)
                                par1)
                             "call1",
                           (beseder-0.1.1.0:Beseder.Base.Internal.Core.St
                              (CallConnectedData
                                 (ReaderT
                                    beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                                    IO)
                                 par2)
                              "call2",
                            (beseder-0.1.1.0:Beseder.Base.Internal.Core.St
                               (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerStoppedEvData
                                  (ReaderT
                                     beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                                     IO))
                               "timer1",
                             beseder-0.1.1.0:Beseder.Base.Internal.Core.St
                               (beseder-0.1.1.0:Beseder.Resources.Timer.TimerRes.TimerArmedEvData
                                  (ReaderT
                                     beseder-0.1.1.0:Beseder.Misc.TaskPosterImpl.TaskQ.QueueChannel
                                     IO))
                               "timer2")))]'
      Where: `par1', `par2' are rigid type variables bound by
               the inferred type of
                 callHandler :: (CallProv TaskQ par1, CallProv TaskQ par2,
                                 Show par1, Show par2, Show (CallRes par2)) =>
                                CallRes par2
                                -> Text
                                -> Int
                                -> Int
                                -> STrans
                                     (ContT Bool)
                                     TaskQ
                                     NoSplitter
                                     '[CallIdle TaskQ par1 "call1"]
                                     '( '[CallIdle TaskQ par1 "call1"], '[])
                                     (ComposeFunc
                                        (EmbedFunc
                                           (Dynamics :&& Not ("call1" :? IsCallOffered))
                                           (BindFunc
                                              IDFunc
                                              (BindFunc
                                                 IDFunc
                                                 (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                    'One GetNextAllFunc))))
                                        (BindFunc
                                           IDFunc
                                           (ComposeFunc
                                              (EmbedFunc
                                                 ("call1" :? IsCallAlive)
                                                 (ComposeFunc
                                                    (NewResFunc (CallRes par2) "call2" TaskQ)
                                                    (ComposeFunc
                                                       (InvokeAllFunc MakeCall "call2")
                                                       (ComposeFunc
                                                          (NewResFunc TimerRes "timer1" TaskQ)
                                                          (ComposeFunc
                                                             (InvokeAllFunc StartTimer "timer1")
                                                             (ComposeFunc
                                                                (NewResFunc TimerRes "timer2" TaskQ)
                                                                (EmbedFunc
                                                                   (("call2" :? IsCallAlive)
                                                                    :&& (Not
                                                                           ("timer1"
                                                                            :? IsTimerTriggered)
                                                                         :&& Not
                                                                               ("timer2"
                                                                                :? IsTimerTriggered)))
                                                                   (ComposeFunc
                                                                      (EmbedFunc
                                                                         (Dynamics
                                                                          :&& Not
                                                                                ("call2"
                                                                                 :? IsCallAnswered))
                                                                         (BindFunc
                                                                            IDFunc
                                                                            (BindFunc
                                                                               IDFunc
                                                                               (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                  'One
                                                                                  GetNextAllFunc))))
                                                                      (ComposeFunc
                                                                         (InvokeAllFunc
                                                                            StopTimer "timer1")
                                                                         (BindFunc
                                                                            IDFunc
                                                                            (ComposeFunc
                                                                               (InvokeAllFunc
                                                                                  AnswerCall
                                                                                  "call1")
                                                                               (ComposeFunc
                                                                                  (EmbedFunc
                                                                                     (Dynamics
                                                                                      :&& Not
                                                                                            (("call2"
                                                                                              :? IsCallConnected)
                                                                                             :&& ("call1"
                                                                                                  :? IsCallConnected)))
                                                                                     (BindFunc
                                                                                        IDFunc
                                                                                        (BindFunc
                                                                                           IDFunc
                                                                                           (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                              ('Succ
                                                                                                 'One)
                                                                                              GetNextAllFunc))))
                                                                                  (ComposeFunc
                                                                                     (InvokeAllFunc
                                                                                        StartTimer
                                                                                        "timer2")
                                                                                     (BindFunc
                                                                                        IDFunc
                                                                                        (BindFunc
                                                                                           IDFunc
                                                                                           (BindFunc
                                                                                              IDFunc
                                                                                              (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                                 'One
                                                                                                 GetNextAllFunc)))))))))))))))))
                                              (ComposeFunc
                                                 (CaptureFunc (By "timer1") (ClearAllFunc "timer1"))
                                                 (ComposeFunc
                                                    (CaptureFunc
                                                       (By "timer2") (ClearAllFunc "timer2"))
                                                    (ComposeFunc
                                                       (CaptureFunc
                                                          (By "call2") (ClearAllFunc "call2"))
                                                       (ComposeFunc
                                                          (CaptureFunc
                                                             ("call1" :? IsCallAlive)
                                                             (InvokeAllFunc DropCall "call1"))
                                                          (InvokeAllFunc ResetCall "call1"))))))))
                                     ()
               at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:(32,1)-(56,25)
    * In a pattern type signature: _
      In the pattern: _t1 :: _
      In a stmt of a 'do' block: _t1 :: _ <- whatNext
    * Relevant bindings include
        sdpAnswr :: SDPAnswer
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:46:7)
        sdpOffer :: SDPOffer
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:34:3)
        timeoutSec2 :: Int
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:32:39)
        timeoutSec1 :: Int
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:32:27)
        dest :: Text
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:32:22)
        callRes2 :: CallRes par2
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:32:13)
        callHandler :: CallRes par2
                       -> Text
                       -> Int
                       -> Int
                       -> STrans
                            (ContT Bool)
                            TaskQ
                            NoSplitter
                            '[CallIdle TaskQ par1 "call1"]
                            '( '[CallIdle TaskQ par1 "call1"], '[])
                            (ComposeFunc
                               (EmbedFunc
                                  (Dynamics :&& Not ("call1" :? IsCallOffered))
                                  (BindFunc
                                     IDFunc
                                     (BindFunc
                                        IDFunc
                                        (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                           'One GetNextAllFunc))))
                               (BindFunc
                                  IDFunc
                                  (ComposeFunc
                                     (EmbedFunc
                                        ("call1" :? IsCallAlive)
                                        (ComposeFunc
                                           (NewResFunc (CallRes par2) "call2" TaskQ)
                                           (ComposeFunc
                                              (InvokeAllFunc MakeCall "call2")
                                              (ComposeFunc
                                                 (NewResFunc TimerRes "timer1" TaskQ)
                                                 (ComposeFunc
                                                    (InvokeAllFunc StartTimer "timer1")
                                                    (ComposeFunc
                                                       (NewResFunc TimerRes "timer2" TaskQ)
                                                       (EmbedFunc
                                                          (("call2" :? IsCallAlive)
                                                           :&& (Not ("timer1" :? IsTimerTriggered)
                                                                :&& Not
                                                                      ("timer2"
                                                                       :? IsTimerTriggered)))
                                                          (ComposeFunc
                                                             (EmbedFunc
                                                                (Dynamics
                                                                 :&& Not
                                                                       ("call2" :? IsCallAnswered))
                                                                (BindFunc
                                                                   IDFunc
                                                                   (BindFunc
                                                                      IDFunc
                                                                      (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                         'One GetNextAllFunc))))
                                                             (ComposeFunc
                                                                (InvokeAllFunc StopTimer "timer1")
                                                                (BindFunc
                                                                   IDFunc
                                                                   (ComposeFunc
                                                                      (InvokeAllFunc
                                                                         AnswerCall "call1")
                                                                      (ComposeFunc
                                                                         (EmbedFunc
                                                                            (Dynamics
                                                                             :&& Not
                                                                                   (("call2"
                                                                                     :? IsCallConnected)
                                                                                    :&& ("call1"
                                                                                         :? IsCallConnected)))
                                                                            (BindFunc
                                                                               IDFunc
                                                                               (BindFunc
                                                                                  IDFunc
                                                                                  (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                     ('Succ 'One)
                                                                                     GetNextAllFunc))))
                                                                         (ComposeFunc
                                                                            (InvokeAllFunc
                                                                               StartTimer "timer2")
                                                                            (BindFunc
                                                                               IDFunc
                                                                               (BindFunc
                                                                                  IDFunc
                                                                                  (BindFunc
                                                                                     IDFunc
                                                                                     (beseder-0.1.1.0:Beseder.Base.Internal.STrans.ReplicateFunc
                                                                                        'One
                                                                                        GetNextAllFunc)))))))))))))))))
                                     (ComposeFunc
                                        (CaptureFunc (By "timer1") (ClearAllFunc "timer1"))
                                        (ComposeFunc
                                           (CaptureFunc (By "timer2") (ClearAllFunc "timer2"))
                                           (ComposeFunc
                                              (CaptureFunc (By "call2") (ClearAllFunc "call2"))
                                              (ComposeFunc
                                                 (CaptureFunc
                                                    ("call1" :? IsCallAlive)
                                                    (InvokeAllFunc DropCall "call1"))
                                                 (InvokeAllFunc ResetCall "call1"))))))))
                            ()
          (bound at Examples\JanusB2B\JanusB2BTimerSkip2App.hs:32:1)
   |
50 |       _t1 :: _ <- whatNext -- [(call1/Connected, call2/Connected, t1/Stopped, t2/Armed)]
   |              ^
*** Simplifier [JanusB2BTimerSkip2App]:
Result size of Simplifier iteration=1
  = {terms: 4,550,
     types: 389,461,
     coercions: 943,841,315,
     joins: 0/1,061}
Result size of Simplifier
  = {terms: 4,550,
     types: 389,461,
     coercions: 943,784,539,
     joins: 0/1,061}
!!! Simplifier [JanusB2BTimerSkip2App]: finished in 7478984.38 milliseconds, allocated 1054932.375 megabytes
*** Specialise [JanusB2BTimerSkip2App]:
Result size of Specialise
  = {terms: 4,819,
     types: 390,081,
     coercions: 943,784,728,
     joins: 0/1,058}
!!! Specialise [JanusB2BTimerSkip2App]: finished in 124671.88 milliseconds, allocated 51.568 megabytes
*** Float out(FOS {Lam = Just 0,
                   Consts = True,
                   OverSatApps = False}) [JanusB2BTimerSkip2App]:
Result size of Float out(FOS {Lam = Just 0,
                              Consts = True,
                              OverSatApps = False})
  = {terms: 6,439,
     types: 405,436,
     coercions: 943,784,728,
     joins: 0/122}
!!! Float out(FOS {Lam = Just 0,
                   Consts = True,
                   OverSatApps = False}) [JanusB2BTimerSkip2App]: finished in 1091328.12 milliseconds, allocated 63.484 megabytes
*** Simplifier [JanusB2BTimerSkip2App]:
Result size of Simplifier iteration=1
  = {terms: 5,464,
     types: 370,029,
     coercions: 943,782,032,
     joins: 0/75}
Result size of Simplifier iteration=2
  = {terms: 5,368,
     types: 369,749,
     coercions: 943,781,967,
     joins: 0/70}
Result size of Simplifier
  = {terms: 5,366,
     types: 369,740,
     coercions: 943,781,948,
     joins: 0/70}
!!! Simplifier [JanusB2BTimerSkip2App]: finished in 10544156.25 milliseconds, allocated 862400.072 megabytes
*** Simplifier [JanusB2BTimerSkip2App]:
Result size of Simplifier iteration=1
  = {terms: 5,365,
     types: 369,730,
     coercions: 943,781,948,
     joins: 1/71}
Result size of Simplifier iteration=2
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
Result size of Simplifier
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
!!! Simplifier [JanusB2BTimerSkip2App]: finished in 10098406.25 milliseconds, allocated 861948.451 megabytes
*** Simplifier [JanusB2BTimerSkip2App]:
Result size of Simplifier
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
!!! Simplifier [JanusB2BTimerSkip2App]: finished in 4124593.75 milliseconds, allocated 287316.853 megabytes
*** Float inwards [JanusB2BTimerSkip2App]:
Result size of Float inwards
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
!!! Float inwards [JanusB2BTimerSkip2App]: finished in 777578.12 milliseconds, allocated 53.105 megabytes
*** Called arity analysis [JanusB2BTimerSkip2App]:
Result size of Called arity analysis
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
!!! Called arity analysis [JanusB2BTimerSkip2App]: finished in 108687.50 milliseconds, allocated 16.964 megabytes
*** Simplifier [JanusB2BTimerSkip2App]:
Result size of Simplifier iteration=1
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
Result size of Simplifier
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
!!! Simplifier [JanusB2BTimerSkip2App]: finished in 6774109.38 milliseconds, allocated 574632.874 megabytes
*** Demand analysis [JanusB2BTimerSkip2App]:
Result size of Demand analysis
  = {terms: 5,357,
     types: 369,720,
     coercions: 943,781,948,
     joins: 0/70}
!!! Demand analysis [JanusB2BTimerSkip2App]: finished in 396031.25 milliseconds, allocated 32.343 megabytes
*** Worker Wrapper binds [JanusB2BTimerSkip2App]:
Result size of Worker Wrapper binds
  = {terms: 5,380,
     types: 370,005,
     coercions: 943,781,948,
     joins: 0/75}
!!! Worker Wrapper binds [JanusB2BTimerSkip2App]: finished in 644765.62 milliseconds, allocated 1.269 megabytes
*** Simplifier [JanusB2BTimerSkip2App]:
Result size of Simplifier iteration=1
  = {terms: 5,366,
     types: 369,985,
     coercions: 943,781,948,
     joins: 0/70}
Result size of Simplifier
  = {terms: 5,366,
     types: 369,985,
     coercions: 943,781,948,
     joins: 0/70}
!!! Simplifier [JanusB2BTimerSkip2App]: finished in 8223000.00 milliseconds, allocated 600408.231 megabytes
*** Exitification transformation [JanusB2BTimerSkip2App]:
Result size of Exitification transformation
  = {terms: 5,366,
     types: 369,985,
     coercions: 943,781,948,
     joins: 0/70}
!!! Exitification transformation [JanusB2BTimerSkip2App]: finished in 209187.50 milliseconds, allocated 1.166 megabytes
*** Float out(FOS {Lam = Just 0,
                   Consts = True,
                   OverSatApps = True}) [JanusB2BTimerSkip2App]:
Result size of Float out(FOS {Lam = Just 0,
                              Consts = True,
                              OverSatApps = True})
  = {terms: 6,140,
     types: 398,788,
     coercions: 943,781,948,
     joins: 0/71}
!!! Float out(FOS {Lam = Just 0,
                   Consts = True,
                   OverSatApps = True}) [JanusB2BTimerSkip2App]: finished in 169531.25 milliseconds, allocated 205.154 megabytes
*** Common sub-expression [JanusB2BTimerSkip2App]:
-}