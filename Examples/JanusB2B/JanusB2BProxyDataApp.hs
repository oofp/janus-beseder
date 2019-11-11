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
-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module JanusB2BProxyDataApp 
  ( proxyApp
  ) where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Base.Control
import            Beseder.Base.Common
import            Beseder.Misc.Misc
import            Control.Monad.Cont (ContT)
import            JanusB2BProxyDataIntrp
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO, wait,
                                               (>>), (>>=), forever, until,try,on, gets, First)

proxyApp :: JanusCallRes -> JanusCallRes -> Text -> Text -> Int -> Int -> STransApp (ContT Bool) TaskQ NoSplitter '[()] '[()] '[] () 
proxyApp callRes1 callRes2 destAddress1 destAddress2 timeoutSec1 timeoutSec2 = MkApp $ do
  newRes #call1 callRes1
  newRes #call2 callRes2
  while $ do 
    callHandler destAddress1 destAddress2 timeoutSec1 timeoutSec2
    return True 
  clear #call1  
  clear #call2  

callHandler :: Text -> Text -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[(JCallIdle TaskQ "call1",JCallIdle TaskQ "call2")] _ _ _ () 
callHandler destAddress1 destAddress2 timeoutSec1 timeoutSec2 = do
  skipTo @("call1" :? IsCallOffered :|| "call2" :? IsCallOffered)
  on @("call1" :? IsCallOffered) $
    handleIncomingCall1 destAddress2 timeoutSec1 timeoutSec2 
  on @("call2" :? IsCallOffered) $
    handleIncomingCall2 destAddress1 timeoutSec1 timeoutSec2
  clearCall #call1
  clearCall #call2

clearCall :: forall call. (_) => Named call -> STrans (ContT Bool) TaskQ NoSplitter _ _ _ _ ()
clearCall call_ = do
   on @(call :? IsCallAlive) (invoke call_ DropCall)
   invoke call_ ResetCall
