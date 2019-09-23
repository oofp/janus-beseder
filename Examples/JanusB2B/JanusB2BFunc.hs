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

module JanusB2BFunc where

import            Beseder.Janus.JanusCallProv
import            Beseder.Janus.JanusCallProvImpl
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Control
import            Beseder.Misc.Misc
import            Beseder.Utils
import            Data.String
import            Control.Monad.Trans
import            Control.Monad.Cont (ContT)
import            Control.Monad.Identity (IdentityT)
import            Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, gets, First)

type B2BCallFunc m pars1 pars2 = 
  "res1"  |> NewRes "call1" (CallRes pars1) m
  :>> ForeverFunc
    ( SkipTo ("call1" :? IsCallOffered)
      :>> Try ("call1" :? IsCallAlive) 
        ("res2" |> NewRes "call2" (CallRes pars2) m
        :>> ("makeCall2Pars" |> Invoke "call2" MakeCall)
        :>> Try ("call2" :? IsCallAlive)
          ( SkipTo ("call2" :? IsCallAnswered)
          :>> ("answerCall1Pars" |> Invoke "call1" AnswerCall)
          :>> SkipTo ("call1" :? IsCallConnected :&& "call2" :? IsCallConnected)
          ) 
        )
      :>> ClearResourcesExcept '["call1"]
      :>> On ("call1" :? IsCallAlive) (Invoke "call1" DropCall)
      :>> Invoke "call1" ResetCall
    )  


data Dict (m :: * -> *) pars1 pars2 = 
  Dict
    { dest :: Text
    , res1 :: CallRes pars1
    , res2 :: CallRes pars2
    }

instance -- forall pars1 pars2 q m xs. 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , Has (CallAnswered m pars2 "call2") xs
  , CallProv m pars1, CallProv m pars2) => TransDict q m (Dict m pars1 pars2) "answerCall1Pars" xs  AnswerCall where 
  getTransFromDict  _dict _named = MkApp $ do
    let getSDP :: CallAnswered m pars2 "call2" -> m SDPAnswer
        getSDP = getSDPAnswer
    sdpAnswer <- opRes #call2 getSDP
    return $  AnswerCall sdpAnswer

instance -- forall pars1 pars2 q m xs. 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , Has (CallOffered m pars1 "call1") xs
  , CallProv m pars1 --, CallProv m pars2
  ) => TransDict q m (Dict m pars1 pars2) "makeCall2Pars" xs  MakeCall where 
  getTransFromDict  dict _named = MkApp $ do
    let getSDP :: CallOffered m pars1 "call1" -> m SDPOffer
        getSDP = getSDPOffer
    sdpOffer <- opRes #call1 getSDP
    return $  MakeCall (dest dict) sdpOffer

instance 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , CallProv m pars1 --, CallProv m pars2
  ) => TransDict q m (Dict m pars1 pars2) "res1" xs  (CallRes pars1) where 
  getTransFromDict  dict _named = MkApp $ do
    return $ (res1 dict)
  

instance 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , CallProv m pars2 --, CallProv m pars2
  ) => TransDict q m (Dict m pars1 pars2) "res2" xs  (CallRes pars2) where 
  getTransFromDict  dict _named = MkApp $ do
    return $ (res2 dict)
    
--buildB2BTrans :: (Show pars1, Show pars2, CallProv TaskQ pars1, CallProv TaskQ pars2) => Dict TaskQ pars1 pars2 -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ (B2BCallFunc TaskQ pars1 pars2) ()
--buildB2BTrans dict = buildTrans dict 
          
-- :kind! Eval (B2BCallFunc TaskQ () () NoSplitter '[()])
-- :kind! Eval (FuncWithTrace TaskQ (B2BCallFunc TaskQ NoSplitter '[()])

