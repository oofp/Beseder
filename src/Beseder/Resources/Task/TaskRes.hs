{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.Resources.Task.TaskRes where

import           Protolude    
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
import           Data.Coerce

data CancelTask = CancelTask deriving (Eq, Show)
  
class TaskData res where
  type TaskResult res :: *
  type TaskError res :: *
  
class (Monad m, TaskData res) => TaskProv m res where
  data  TaskInProgress m res 
  data  TaskCompleted m res 
  data  TaskFailed m res 
  data  TaskCancelled m res 
  data  ResPar m res

  startTask :: MkResDef m (ResPar m res) (TaskInProgress m res)
  cancelTask :: RequestDef m CancelTask (TaskInProgress m res) '[TaskCancelled m res]  
  taskTransition :: TransitionDef m (TaskInProgress m res) '[TaskCompleted m res, TaskFailed m res]
  termCompleted :: TermDef m (TaskCompleted m res)
  termCancelled :: TermDef m (TaskCancelled m res)
  termFailed :: TermDef m (TaskFailed m res)

  _taskResult :: TaskCompleted m res -> m (TaskResult res)
  _taskError :: TaskFailed m res -> m (TaskError res)
  
buildRes ''TaskProv

instance GetInstance CancelTask where getInstance = CancelTask

type instance TermRequest (StTaskInProgress m res name) = CancelTask

taskResult :: TaskProv m res => StTaskCompleted m res name -> m (TaskResult res)
taskResult = _taskResult . coerce 
taskError :: TaskProv m res => StTaskFailed m res name -> m (TaskError res)
taskError = _taskError . coerce 
