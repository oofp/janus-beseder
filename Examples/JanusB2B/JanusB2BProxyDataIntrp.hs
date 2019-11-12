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

module JanusB2BProxyDataIntrp where

import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Misc.Misc
import            Control.Monad.Cont (ContT)
import            Beseder.Base.ControlData

import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO, wait,
                                               (>>), (>>=), forever, until,try,on, gets, First)

import            JanusB2BProxyData

type IncomingCallState1  = '[(JCallOffered TaskQ "call1", JCallIdle TaskQ "call2")]
type IncomingCallState2  = '[(JCallIdle TaskQ "call1", JCallOffered TaskQ "call2")]

--instance CallProv TaskQ JanusCallRes --- ????

--handleIncomingCall1 =  handleIncomingCall #src #dest "100" 10 15
--handleIncomingCall2 =  handleIncomingCall #dest #src "100" 10 15

handleIncomingCall1 :: (_) => Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter IncomingCallState1 _ _ _ ()
handleIncomingCall1 dest timeoutSec1 timeoutSec2 = interpret (handleIncomingCall #call1 #call2 dest timeoutSec1 timeoutSec2)

handleIncomingCall2 :: (_) => Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter IncomingCallState2 _ _ _ ()
handleIncomingCall2 dest timeoutSec1 timeoutSec2 = interpret (handleIncomingCall #call2 #call1 dest timeoutSec1 timeoutSec2)
