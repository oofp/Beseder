{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  Beseder.Base.Internal.STransDataTH 
  ( mkSTransDataType
  )where

import           Protolude hiding (Type)                   
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Data.List
import           Prelude (error)    

reifyValue :: String -> Q (Maybe Info) -- Q [Dec]
reifyValue valName = do
  maybeName <- lookupValueName valName
  case maybeName of 
    Nothing -> do
      liftIO $ putStrLn ("=================== Value NOT found"::Text)
      return Nothing -- [] -- Nothing
    (Just valName) -> do
      rInfo <- reify valName
      liftIO $ putStrLn (show rInfo)
      return $ Just rInfo
      --liftIO $ putStrLn ("=================== Value found"::Text)
      --return []

-- $(stringE . show =<< reifyValue "timer1")

getFuncType :: Info -> Maybe Type
getFuncType (VarI _varName (ForallT _varBnd _ctx (AppT (AppT _ fType) _)) _maybeDec) = Just fType
getFuncType _ = Nothing

reifyFunc :: String -> Q (Maybe Type)
reifyFunc valName = do
  maybeInfo <- reifyValue valName
  return (maybeInfo >>= getFuncType) 

getVarTNames :: String -> Q (Maybe [Name])
getVarTNames valName = do
  tpMaybe <- reifyFunc valName
  return (fmap varTNames tpMaybe)

varTNames :: Type -> [Name]
varTNames (VarT name) = [name]
varTNames (AppT t1 t2) = nub (varTNames t1 <> varTNames t2)
varTNames _ = []

{-
[ TySynD StState1_0
    [ PlainTV m_1
    , PlainTV res_2
    , PlainTV name_3
    ]
    ( AppT
        ( AppT ( ConT Beseder.Base.Internal.Core.St )
            ( AppT
                ( AppT ( ConT Beseder.Resources.ResourceDefSample.State1 ) ( VarT m_1 ) ) ( VarT res_2 )
            )
        ) ( VarT name_3 )
    )
]
data TyVarBndr = PlainTV Name
data Dec
  = ... | TySynD Name [TyVarBndr] Language.Haskell.TH.Type
-}
  
mkSTransDataType :: String -> String -> Q [Dec]
mkSTransDataType funcName typeNameStr = do
  tpMaybe <- reifyFunc funcName
  case tpMaybe of 
    Nothing -> error "Cannot exract function type"
    Just fnType -> do
      let names = varTNames fnType
          typeName = mkName typeNameStr  
      return [TySynD typeName (fmap PlainTV names) fnType]
  
-- $(stringE . show =<< reifyFunc "timer0")

{-AppT 
  (AppT 
    (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
    (AppT 
      (AppT (AppT (ConT Beseder.Base.Internal.STransDef.NewResFunc) (ConT Beseder.Resources.Timer.TimerRes.TimerRes)) (LitT (StrTyLit "t1"))) 
      (VarT m_6989586621680000580)
    )
  ) 
  (AppT 
    (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.Resources.Timer.TimerRes.StartTimer)) (LitT (StrTyLit "t1")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.CaptureFunc) (ConT Beseder.Base.Internal.SplitOps.Dynamics)) (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc))) (AppT (ConT Beseder.Base.Internal.STransDef.ClearAllFunc) (LitT (StrTyLit "t1"))))
  )

VarI TimerDataDemo1.timer1 
  (ForallT 
    [KindedTV m_6989586621679681078 (AppT (AppT ArrowT StarT) StarT),KindedTV sp_6989586621679681079 StarT] [] 
    (AppT (AppT ArrowT (ConT GHC.Types.Int)) 
           (AppT 
            (AppT 
              (AppT (AppT (ConT Beseder.Base.Internal.STransData.STransData) (VarT m_6989586621679681078)) (VarT sp_6989586621679681079)) 
              (AppT 
                (AppT 
                  (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
                  (AppT (AppT (AppT (ConT Beseder.Base.Internal.STransDef.NewResFunc) (ConT Beseder.Resources.Timer.TimerRes.TimerRes)) (LitT (StrTyLit "t1"))) (VarT m_6989586621679681078))
                ) 
                (AppT 
                  (AppT 
                    (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
                    (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.Resources.Timer.TimerRes.StartTimer)) (LitT (StrTyLit "t1")))
                  ) 
                  (AppT 
                    (AppT 
                      (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
                      (AppT (AppT (ConT Beseder.Base.Internal.STransDef.CaptureFunc) (ConT Beseder.Base.Internal.SplitOps.Dynamics)) (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc))
                    ) 
                    (AppT (ConT Beseder.Base.Internal.STransDef.ClearAllFunc) (LitT (StrTyLit "t1")))
                  )
                )
              )
            ) 
            (TupleT 0)
          )
    )
  ) Nothing
-}


{-
VarI TimerDataDemo1TH.timer0 
  (ForallT 
    [KindedTV m_6989586621679197242 (AppT (AppT ArrowT StarT) StarT),KindedTV sp_6989586621679197243 StarT] [] 
    (AppT 
      (AppT 
        (AppT (AppT (ConT Beseder.Base.Internal.STransData.STransData) (VarT m_6989586621679197242)) (VarT sp_6989586621679197243)) 
        (AppT 
          (AppT 
            (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
            (AppT 
              (AppT (AppT (ConT Beseder.Base.Internal.STransDef.NewResFunc) (ConT Beseder.Resources.Timer.TimerRes.TimerRes)) (LitT (StrTyLit "t1"))) 
              (VarT m_6989586621679197242)
            )
          ) 
          (AppT 
            (AppT 
              (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
              (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.Resources.Timer.TimerRes.StartTimer)) (LitT (StrTyLit "t1")))
            ) 
            (AppT 
              (AppT 
                (ConT Beseder.Base.Internal.STransDef.ComposeFunc) 
                (AppT 
                  (AppT 
                    (ConT Beseder.Base.Internal.STransDef.CaptureFunc) 
                    (ConT Beseder.Base.Internal.SplitOps.Dynamics)
                  ) 
                  (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc)
                )
              ) 
              (AppT (ConT Beseder.Base.Internal.STransDef.ClearAllFunc) (LitT (StrTyLit "t1")))
            )
          )
        )
      ) 
      (TupleT 0)
    )
  ) Nothing)

-}

 

