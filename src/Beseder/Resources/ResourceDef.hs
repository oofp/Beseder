{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}

module Beseder.Resources.ResourceDef where

import           Protolude    
import           Prelude (error)    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Beseder.Base.Internal.StHelper

type MkResDef m resPars initSt = resPars -> m (initSt)
type RequestDef m req st results = req -> st -> m (V results)
type TransitionDef m st nextStates = st -> ((V nextStates) -> m ()) -> m ()
type TermDef m st = st -> m ()

mkResDefName :: Name
mkResDefName = ''MkResDef

--
buildRes :: Name -> Q [Dec]
buildRes className = do
  classDef <- reify className  
  case classDef of 
    (ClassI (ClassD _cxt _cn _tyVarBndr _funDep decEntries) _) -> parseDecs className decEntries
    _ -> error "Should be class declaration"

parseDecs :: Name -> [Dec] -> Q [Dec]
parseDecs className decs = concat <$> mapM (parseDec className) decs


-- 
parseDec :: Name -> Dec -> Q [Dec]
parseDec _className (DataFamilyD stateName _ _) = parseDataFam stateName
parseDec className -- (SigD sigName sigType) = 
          (SigD funcName
            ( ForallT _ _ 
              ( AppT 
                (AppT 
                  (AppT 
                    (ConT _mkResDef) _ -- Beseder.Resources.ResourceDef.MkResDef) 
                  ) _
                ) 
                ( AppT (AppT (ConT initState) _) _
                )
              )
            )
          ) = parseMkRes className funcName initState    

parseDec className          
          (SigD funcName 
            (ForallT _ _ 
              (AppT 
                (AppT (AppT (ConT _transitionDef) _) (AppT (AppT (ConT fromState) _) _)) 
                statesList
              )
            )
          ) = parseTransition className funcName fromState statesList  

parseDec className          
        (SigD funcName 
          (ForallT _ _ 
            (AppT 
              (AppT (AppT (AppT (ConT _requestDef) _) (ConT reqName)) (AppT (AppT (ConT fromState) _) _)) 
              statesList
            )
          )
        ) = parseRequest className funcName reqName fromState statesList
parseDec className          
        (SigD funcName 
          (ForallT _ _  
            (AppT 
              (AppT (ConT _termDef) _) (AppT (AppT (ConT fromState) _) _))
          )
        ) = parseTerminate className funcName fromState
              
parseDec _ dec = do 
  liftIO $ putStrLn (("***************** Cannot recognize entry: " :: Text) <> show dec)
  return []

{-
***************** Cannot recognize entry: SigD Beseder.Resources.ResourceDefSample.createMyRes (ForallT [KindedTV k_6989586621679263184 StarT,KindedTV m_6989586621679263154 (AppT (AppT ArrowT StarT) StarT),KindedTV res_6989586621679263155 (VarT k_6989586621679263184)] [AppT (AppT (ConT Beseder.Resources.ResourceDefSample.MyResDef) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)] (AppT (AppT (AppT (ConT Beseder.Resources.ResourceDef.MkResDef) (VarT m_6989586621679263154)) (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.ResPar) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State1) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))))
***************** Cannot recognize entry: SigD Beseder.Resources.ResourceDefSample.req1 (ForallT [KindedTV k_6989586621679263184 StarT,KindedTV m_6989586621679263154 (AppT (AppT ArrowT StarT) StarT),KindedTV res_6989586621679263155 (VarT k_6989586621679263184)] [AppT (AppT (ConT Beseder.Resources.ResourceDefSample.MyResDef) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)] (AppT (AppT (AppT (AppT (ConT Beseder.Resources.ResourceDef.RequestDef) (VarT m_6989586621679263154)) (ConT Beseder.Resources.ResourceDefSample.Req1)) (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State1) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State2) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (SigT PromotedNilT (AppT ListT StarT)))))
***************** Cannot recognize entry: SigD Beseder.Resources.ResourceDefSample.trans1 (ForallT [KindedTV k_6989586621679263184 StarT,KindedTV m_6989586621679263154 (AppT (AppT ArrowT StarT) StarT),KindedTV res_6989586621679263155 (VarT k_6989586621679263184)] [AppT (AppT (ConT Beseder.Resources.ResourceDefSample.MyResDef) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)] (AppT (AppT (AppT (ConT Beseder.Resources.ResourceDef.TransitionDef) (VarT m_6989586621679263154)) (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State1) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State2) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State1) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (SigT PromotedNilT (AppT ListT StarT))))))
***************** Cannot recognize entry: SigD Beseder.Resources.ResourceDefSample.term2 (ForallT [KindedTV k_6989586621679263184 StarT,KindedTV m_6989586621679263154 (AppT (AppT ArrowT StarT) StarT),KindedTV res_6989586621679263155 (VarT k_6989586621679263184)] [AppT (AppT (ConT Beseder.Resources.ResourceDefSample.MyResDef) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)] (AppT (AppT (ConT Beseder.Resources.ResourceDef.TermDef) (VarT m_6989586621679263154)) (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State2) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))))
-}

{-
SigD Beseder.Resources.ResourceDefSample.trans1 
  (ForallT 
    [KindedTV k_6989586621679263184 StarT,KindedTV m_6989586621679263154 (AppT (AppT ArrowT StarT) StarT),KindedTV res_6989586621679263155 (VarT k_6989586621679263184)] 
    [AppT (AppT (ConT Beseder.Resources.ResourceDefSample.MyResDef) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)] 
    (AppT (AppT (AppT (ConT Beseder.Resources.ResourceDef.TransitionDef) (VarT m_6989586621679263154)) (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State1) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State2) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)))
     (AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDefSample.State1) (VarT m_6989586621679263154)) (VarT res_6989586621679263155))) (SigT PromotedNilT (AppT ListT StarT))))))

SigD funcName 
(ForallT 
  _ -- [KindedTV k_6989586621679263184 StarT,KindedTV m_6989586621679263154 (AppT (AppT ArrowT StarT) StarT),KindedTV res_6989586621679263155 (VarT k_6989586621679263184)] 
  _ -- [AppT (AppT (ConT Beseder.Resources.ResourceDefSample.MyResDef) (VarT m_6989586621679263154)) (VarT res_6989586621679263155)] 
  (AppT (AppT (AppT (ConT _transitionDef) _) (AppT (AppT (ConT fromState) _) _)) 
  (AppT (AppT PromotedConsT (AppT (AppT (ConT nextState) _) _))
        (AppT (AppT PromotedConsT (AppT (AppT (ConT nextStat1) _) _)) (SigT PromotedNilT _)))))

SigD funcName 
  (ForallT _  
    (AppT (AppT (ConT _termDef) _) (AppT (AppT (ConT _fromState) _) _)))

-}

buildState :: Name -> TypeQ
buildState stateName = 
    return $
      AppT
        ( AppT ( ConT $ mkName "St")
          ( AppT
              ( AppT ( ConT stateName ) ( VarT mPar ) ) ( VarT resPar )
          )
        ) 
        ( VarT namePar )
  where
    stStrName = nameBase stateName
    mPar = mkName "m"
    namePar = mkName "name"
    resPar = mkName "res"

parseDataFam :: Name -> Q [Dec]
parseDataFam stateName = return $ 
  if stStrName == "ResPar" -- no need to create entry for ResPar
    then []
    else 
      [ TySynD aliasName
        [ PlainTV mPar
        , PlainTV resPar
        , PlainTV namePar
        ]
        ( AppT
            ( AppT ( ConT $ mkName "St")
                ( AppT
                    ( AppT ( ConT stateName ) ( VarT mPar ) ) ( VarT resPar )
                )
            ) ( VarT namePar )
        )
      ]
    where
      stStrName = nameBase stateName
      mPar = mkName "m"
      namePar = mkName "name"
      resPar = mkName "res"
      aliasName = mkName $ "St" <> stStrName    

parseMkRes :: Name -> Name -> Name -> Q [Dec]
parseMkRes className funcName initStateName = do
  let mName = mkName "m"
      nameName = mkName "name"
      resName = mkName "res"
      mkResName = mkName "MkRes"
      resParName = mkName "ResPar"
      resStName = mkName "ResSt"
      resParFuncArgName = mkName "resPar"
  liftIO $ putStrLn ("***************** parseMkRes" :: Text)
  return $  
    [ InstanceD Nothing
      [ AppT
          ( AppT ( ConT className ) ( VarT mName ) ) ( VarT resName )
      ]
      ( AppT
          ( AppT ( ConT mkResName ) ( VarT mName ) )
          ( AppT
              ( AppT ( ConT resParName ) ( VarT mName ) ) ( VarT resName )
          )
      )
      [ TySynInstD resStName
          ( TySynEqn
              [ VarT mName
              , AppT
                  ( AppT ( ConT resParName ) ( VarT mName ) ) ( VarT resName )
              ]
              ( AppT
                  ( AppT ( ConT initStateName ) ( VarT mName ) ) ( VarT resName )
              )
          )
      , FunD (mkName "mkRes")
          [ Clause [ VarP resParFuncArgName ]
              ( NormalB
                  ( AppE ( VarE funcName ) ( VarE resParFuncArgName ) )
              ) []
          ]
      ]
    ]  
-- [d|type $(conT stTypeName) m res name = St ( Language.Haskell.TH.Type$(conT stateName) m res) name|]  
--[d| type $(conT stTypeName) m res name = StLanguage.Haskell.TH.Type ($(ConT stDateName) m res) name |]


parseTransition :: Name -> Name -> Name -> Language.Haskell.TH.Type -> Q [Dec]
parseTransition className funcName fromStateName statesListType = 
    --liftIO $ putStrLn (("***************** parseTransition states" :: Text) <> show stateNames)
    --liftIO $ putStrLn ("***************** parseTransition" :: Text)
    [d|
        type instance StateTrans (St ($(conT fromStateName) m res) name) = 'Dynamic    
        instance 
          ( $(conT className) m res
          -- , StVar $(statesListTypeNew) name -- '[State2 m res, State1 m res] name
          , MonadIO m
          ) => Transition m (St ($(conT fromStateName) m res) name) where
          type NextStates (St ($(conT fromStateName) m res) name) = StList name $(statesListTypeNew) 
          next st@(St fromState) cb = $(varE funcName) fromState (\nextStates -> void $ cb (asStVar (nameFromSt st) nextStates)) >> return True
    |]    
  where
    mName = mkName "m"
    nameName = mkName "name"
    resName = mkName "res"
    stateNames = parseStatesList statesListType
    statesListTypeNew = return $ buildStateList stateNames resName mName 

parseRequest :: Name -> Name -> Name -> Name -> Language.Haskell.TH.Type -> Q [Dec]
parseRequest className funcName reqName fromStateName statesListType = -- do
    -- liftIO $ putStrLn ("***************** parseRequest" :: Text)
    [d|
      instance 
        ( $(conT className) m res
        ) => Request m $(conT reqName) (St ($(conT fromStateName) m res) name) where
          type ReqResult $(conT reqName) (St ($(conT fromStateName) m res) name) = StList name $(statesListTypeNew) 
          request reqData st@(St fromState) = fmap (asStVar (nameFromSt st)) ($(varE funcName) reqData fromState) 
    |]    
  where
      mName = mkName "m"
      nameName = mkName "name"
      resName = mkName "res"
      stateNames = parseStatesList statesListType
      statesListTypeNew = return $ buildStateList stateNames resName mName 
  
parseTerminate :: Name -> Name -> Name -> Q [Dec]
parseTerminate className funcName fromStateName = -- do
    -- liftIO $ putStrLn ("***************** parseRequest" :: Text)
    [d|
      type instance StateTrans (St ($(conT fromStateName) m res) name) = 'Static    
      instance 
        ( $(conT className) m res
        ) => TermState m (St ($(conT fromStateName) m res) name) where
          terminate (St fromState) = $(varE funcName) fromState
    |]    
      
parseStatesList :: Language.Haskell.TH.Type -> [Name]
parseStatesList 
  (AppT 
    (AppT 
      PromotedConsT 
      (AppT 
        (AppT (ConT stateName) _) _)) moreEntries) = stateName : parseStatesList moreEntries
parseStatesList _ = []

buildStateList :: [Name] -> Name -> Name -> Language.Haskell.TH.Type
buildStateList [] resName monadName = SigT PromotedNilT (AppT ListT StarT) 
buildStateList (stateName : moreStates) resName monadName = 
  AppT 
    (AppT 
      PromotedConsT 
      (AppT (AppT (ConT stateName) (VarT monadName)) (VarT resName))
    ) 
    (buildStateList moreStates resName monadName)

--(AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDef.State2) (VarT m_6989586621691543984)) (VarT res_6989586621691543985)))
--(AppT (AppT PromotedConsT (AppT (AppT (ConT Beseder.Resources.ResourceDef.State1) (VarT m_6989586621691543984)) (VarT res_6989586621691543985))) (SigT PromotedNilT (AppT ListT StarT)))
--)

