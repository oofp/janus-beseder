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

module JanusB2BApp where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Control
import            Beseder.Misc.Misc
import            Beseder.Utils
import            Data.String
import            Control.Monad.Cont (ContT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, gets, First)


b2bTrans :: (CallProv TaskQ par1, CallProv TaskQ par2, Show par1, Show par2) => 
    CallRes par1 -> CallRes par2 -> Text -> STransApp (ContT Bool) TaskQ NoSplitter '[()] '[()] '[] ()      
b2bTrans callRes1 callRes2 dest = MkApp $ do        -- [()]
  newRes #call1 callRes1                            -- [(call1/Idle)]
  while $ do  
    reach @("call1" :? IsCallOffered) nextEv        -- [(call1/Offered)]
    sdpOffer <- opRes #call1 getSDPOffer
    try @("call1" :? IsCallAlive) $ do              -- break out if call1 disconnects
      newRes #call2 callRes2                        -- [(call1/Offered, call2/Idle)]
      invoke #call2 (MakeCall dest sdpOffer)        -- [(call1/Offered, call2/Dialing)]
      try @("call2" :? IsCallAlive) $ do            -- break out if call2 disconnects
        reach @("call2" :? IsCallAnswered) nextEv   -- [(call1/Offered, call2/Answered)]
        sdpAnswr <- opRes #call2 getSDPAnswer
        invoke #call1 (AnswerCall sdpAnswr)         -- [(call1/Answering, call2/Answered)]
        reach @("call2" :? IsCallConnected 
            :&& "call1" :? IsCallConnected) 
          (nextEv >> nextEv)                        -- [(call1/Connected, call2/Connected)]
        nextEv
    on @(By "call2") (clear #call2)  
    on @("call1" :? IsCallAlive) (invoke #call1 DropCall)
    invoke #call1 ResetCall                         -- [(call1/Idle)]
    return True  
  clear #call1                                      -- [()]


