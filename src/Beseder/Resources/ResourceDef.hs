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
import           Data.Text (pack, unpack)
import           Data.List ((\\))

type MkResDef m resPars initSt = resPars -> m (initSt)
type RequestDef m req st results = req -> st -> m (V results)
type TransitionDef m st nextStates = st -> ((V nextStates) -> m ()) -> m ()
type TermDef m st = st -> m ()
type OpDef m st a = st -> m a 

data ResDsc = ResDsc 
  { resStates :: [Text]
  , initStates :: [Text]
  , transitions :: [(Text,[Text])]
  , requests :: [(Text,Text,[Text])]
  , termStates :: [Text]
  } deriving (Show)

instance Semigroup ResDsc where
  r1 <> r2 = 
    ResDsc 
      (resStates r1 <> resStates r2)   
      (initStates r1 <> initStates r2)   
      (transitions r1 <> transitions r2)   
      (requests r1 <> requests r2)   
      (termStates r1 <> termStates r2)   

instance Monoid ResDsc where
  mempty = emptyResDsc

emptyResDsc :: ResDsc
emptyResDsc = ResDsc [] [] [] [] []

addState :: Text -> ResDsc -> ResDsc 
addState stateName resDsc = resDsc {resStates = stateName : resStates resDsc}

addInitState :: Text -> ResDsc -> ResDsc 
addInitState stateName resDsc = resDsc {initStates = stateName : initStates resDsc}

addTrans :: (Text,[Text]) -> ResDsc -> ResDsc 
addTrans transEntry resDsc = resDsc {transitions = transEntry : transitions resDsc}

addReq :: (Text,Text,[Text]) -> ResDsc -> ResDsc 
addReq reqEntry resDsc = resDsc {requests = reqEntry : requests resDsc}

addTerm :: Text -> ResDsc -> ResDsc 
addTerm termState resDsc = resDsc {termStates = termState : termStates resDsc}

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
parseDecs className decs = 
  evalStateT (parseDecsState className decs) emptyResDsc

printAsUml :: Text -> ResDsc -> IO ()
printAsUml classText resDsc = do
  putStrLn ("@startuml" :: Text)
  putStrLn ("hide empty description"::Text)
  putStrLn ("title " <> classText)

  forM_ (initStates resDsc) 
    (\st -> putStrLn ("[*] --> " <> st))
  forM_ (transitions resDsc) 
    (\(fromState, toStates) -> 
        forM toStates (\toState -> putStrLn (fromState <> " --> " <> toState <> " : trans")))
  forM_ (requests resDsc) 
    (\(req, fromState, toStates) -> 
        forM toStates (\toState -> putStrLn (fromState <> " --> " <> toState <> " : " <> req)))
  forM_ (termStates resDsc) 
    (\st -> putStrLn (st <> "--> [*]"))
  putStrLn ("@enduml" :: Text)
        

parseDecsState :: Name -> [Dec] -> StateT ResDsc Q [Dec]
parseDecsState className decs = do
  decs <- concat <$> mapM (parseDec className) decs
  resDsc <- get
  liftIO $ putStrLn (("***************** ResDsc: " :: Text) <> show resDsc)
  liftIO $ printAsUml (pack $ nameBase className) resDsc
  let defStates = (termStates resDsc) <> (fst <$> (transitions resDsc))
      undefStates = (resStates resDsc) \\ defStates
  liftIO $ putStrLn (("Identified static states: " :: Text) <> show undefStates)
  undefStateDecs <- lift $ concat <$> mapM mkStaticStateTrans undefStates
  stateTitleDecs <- lift $ concat <$> mapM mkStateTitle (resStates resDsc)
  return $ decs <> undefStateDecs <> stateTitleDecs

mkStaticStateTrans :: Text -> Q [Dec]
mkStaticStateTrans stateTxt = 
    [d|
      type instance StateTrans (St ($(conT stateName) m res) name) = 'Static    
    |]  
  where
    stateName = mkName (unpack stateTxt)  

mkStateTitle :: Text -> Q [Dec]
mkStateTitle stateTxt = 
    return $ 
      [ TySynInstD (mkName "StateTitle")
        ( TySynEqn
            [ AppT
                ( AppT ( ConT stateName ) ( VarT mName ) ) ( VarT resName )
            ]
            ( LitT ( StrTyLit (unpack stateTxt) ) )
        )
      ]
  where      
    stateName = mkName (unpack stateTxt)  
    mName = mkName "m"
    resName = mkName "res"
    
-- 
parseDec :: Name -> Dec -> StateT ResDsc Q [Dec]
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

parseDataFam :: Name -> StateT ResDsc Q [Dec]
parseDataFam stateName =  
    if stStrName == "ResPar" -- no need to create entry for ResPar
      then return $ []
      else do
        modify (addState $ pack stStrName)
        return $
            (buildStateTypeSyn stateName) : (buildStatePred stateName)
  where
    stStrName = nameBase stateName
  
buildStateTypeSyn :: Name -> Dec    
buildStateTypeSyn stateName =
    TySynD aliasName 
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
  where
    stStrName = nameBase stateName
    mPar = mkName "m"
    namePar = mkName "name"
    resPar = mkName "res"
    aliasName = mkName $ "St" <> stStrName    


buildStatePred :: Name -> [Dec]
buildStatePred stateName = 
      [ DataD [] isStateName []
        ( Just
            ( AppT ( AppT ArrowT StarT )
                ( AppT (ConT (mkName "Exp")) (ConT boolName))
            )
        ) [] []
    , TySynInstD (mkName "Eval")
        ( TySynEqn
            [ AppT ( ConT isStateName ) ( VarT stPar ) ]
            ( AppT ( ConT isStateFamName ) ( VarT stPar ) )
        )
    , ClosedTypeFamilyD
        ( TypeFamilyHead isStateFamName [ PlainTV stPar ]
            ( KindSig (ConT boolName)) Nothing
        )
        [ TySynEqn
            [ AppT
                ( AppT ( ConT (mkName "St") )
                    ( AppT
                        ( AppT ( ConT stateName ) ( VarT mPar ) ) ( VarT resPar )
                    )
                ) ( VarT namePar )
            ] ( PromotedT (mkName "True") )
        , TySynEqn [ WildCardT ] ( PromotedT (mkName "False"))
        ]
    ]  
  where
    stStrName = nameBase stateName
    isStateName = mkName $ "Is" <> stStrName    
    isStateFamName = mkName $ "Is" <> stStrName <> "Fam"    
    mPar = mkName "m"
    namePar = mkName "name"
    resPar = mkName "res"
    stPar = mkName "st"
    boolName = mkName "Bool"

parseMkRes :: Name -> Name -> Name -> StateT ResDsc Q [Dec]
parseMkRes className funcName initStateName = do
  let mName = mkName "m"
      nameName = mkName "name"
      resName = mkName "res"
      mkResName = mkName "MkRes"
      resParName = mkName "ResPar"
      resStName = mkName "ResSt"
      resParFuncArgName = mkName "resPar"
  liftIO $ putStrLn ("***************** parseMkRes" :: Text)
  modify (addInitState $ pack (nameBase initStateName))
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


parseTransition :: Name -> Name -> Name -> Language.Haskell.TH.Type -> StateT ResDsc Q [Dec]
parseTransition className funcName fromStateName statesListType = do
    --liftIO $ putStrLn (("***************** parseTransition states" :: Text) <> show stateNames)
    --liftIO $ putStrLn ("***************** parseTransition" :: Text)
    modify (addTrans transEntry)
    lift $   
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
    stateTexts = fmap (pack . nameBase) stateNames
    fromStateText = pack $ nameBase fromStateName
    transEntry = (fromStateText,stateTexts)    
    statesListTypeNew = return $ buildStateList stateNames resName mName 

parseRequest :: Name -> Name -> Name -> Name -> Language.Haskell.TH.Type -> StateT ResDsc Q [Dec] 
parseRequest className funcName reqName fromStateName statesListType = do
    -- liftIO $ putStrLn ("***************** parseRequest" :: Text)
    modify (addReq reqEntry)
    lift $   
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
      stateTexts = fmap (pack . nameBase) stateNames
      fromStateText = pack $ nameBase fromStateName
      reqText = pack $ nameBase reqName
      reqEntry = (reqText, fromStateText, stateTexts)
    
parseTerminate :: Name -> Name -> Name -> StateT ResDsc Q [Dec]
parseTerminate className funcName fromStateName = do
    -- liftIO $ putStrLn ("***************** parseRequest" :: Text)
    modify (addTerm (pack $ nameBase fromStateName))
    lift $
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


