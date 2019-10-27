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
{-# LANGUAGE TemplateHaskell       #-} 

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module JanusB2BProxyTH where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Resources.Timer
import            Beseder.Base.Control
import            Beseder.Base.Common
import            Beseder.Misc.Misc
import            Data.String
import            Control.Monad.Cont (ContT)
import            Language.Haskell.TH
import            Language.Haskell.TH.Quote
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO, wait,
                                               (>>), (>>=), forever, until,try,on, gets, First)

{-
handleIncomingCall :: forall src dest par1 par2. 
   ( --Has (CallOffered TaskQ par1 src) xs
     --, Has (CallIdle TaskQ par2 dest) xs
     src ~ "src"
   , dest ~ "dest"
   , _
   ) => Named src -> Named dest -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[(CallOffered TaskQ par1 src, CallIdle TaskQ par2 dest)] _ _ _ ()
handleIncomingCall srcCall destCall destAddress timeoutSec1 timeoutSec2 = do
-}  
handleIncomingCall :: Name -> Name -> ExpQ
handleIncomingCall src dest =
    [|
      \destAddress timeoutSec1 timeoutSec2 -> 
        do  
          let srcN = Named @($srcT)
              destN = Named @($destT)
          sdpOffer <- opRes srcN getSDPOffer
          try @($srcT :? IsCallAlive) $ do 
            invoke destN (MakeCall destAddress sdpOffer)
            newRes #timer1 TimerRes
            invoke #timer1 (StartTimer timeoutSec1)
            newRes #timer2 TimerRes
            try @($destT :? IsCallAlive 
                  :&& (Not ("timer1" :? IsTimerTriggered))
                  :&& (Not ("timer2" :? IsTimerTriggered))) $ do 
              skipTo @($destT :? IsCallAnswered)
              invoke #timer1 StopTimer
              sdpAnswr <- opRes destN getSDPAnswer
              invoke srcN (AnswerCall sdpAnswr)
              --
              skipTo @($srcT :? IsCallConnected :&& $destT :? IsCallConnected) 
              invoke #timer2 (StartTimer timeoutSec2)
              skipAll
          on @(By "timer1") (clear #timer1)  
          on @(By "timer2") (clear #timer2)  
      |]
    where 
      srcT = conT src
      destT = conT dest

