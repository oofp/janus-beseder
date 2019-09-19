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

module JanusB2BLoopApp where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Control
import            Beseder.Misc.Misc
import            Beseder.Utils
import            Data.String
import            Control.Monad.Cont (ContT)
import            Control.Monad.Identity (IdentityT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, gets, First)


b2bTrans :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
    CallRes par1 -> CallRes par2 -> Text -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ ()      
b2bTrans callRes1 callRes2 dest = do 
  newRes #call1 callRes1
  while (callHandler callRes2 dest >> return True) 

callHandler :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
                  CallRes par2 -> Text -> STrans (ContT Bool) TaskQ NoSplitter '[CallIdle TaskQ par1 "call1"] _ _ ()
callHandler callRes2 dest = do
  reach @("call1" :? IsCallOffered) nextEv
  sdpOffer <- opRes #call1 getSDPOffer
  try @("call1" :? IsCallAlive) $ do
    newRes #call2 callRes2
    invoke #call2 (MakeCall dest sdpOffer)
    try @("call2" :? IsCallAlive) $ do
      handleEvents $ do
        on @("call1" :? IsCallOffered :&& "call2" :? IsCallAnswered) $ do
          sdpAnswr <- opRes #call2 getSDPAnswer
          invoke #call1 (AnswerCall sdpAnswr)
  clear #call2
  on @("call1" :? IsCallAlive) (invoke #call1 DropCall)
  invoke #call1 ResetCall

