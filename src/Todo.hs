
{-|
Module      : Todo
Description : Core todo module.
Copyright   : (c) Simon Goller, 2016
License     : BSD

Provides the basic functionality for the tasks.

-}

module Todo (
  -- * Data types
  Task,
  ActiveTask,
  PooledTask,
  TastMgmt,
  TaskState,

  -- * Todo state accessor fuctions
  getActives,
  setActives,
  getPool,
  setPool,
  addActive,
  addPool
  )
       where


import Data.Time.Calendar (Day)
import Control.Monad.State.Lazy
import Data.List (delete)


--- Data declaration

-- | Basic task type
data Task = Task {
  tTitle :: String, -- ^ Identifier.
  tDesc :: String,  -- ^ Short but more detailed description of the task.
  tFactor :: Float  -- ^ How important the task is.
} deriving (Show, Read)

-- | An activates tasks
--
-- Additionally holds a due date and if and when
-- it was finished.
data ActiveTask = ActiveTask {
  atTask :: Task,   -- ^ The task itself.
  atDue :: Day,     -- ^ Deadline for to complete the task.
  atFinished :: Maybe Day -- ^ If and when the task was finished.
} (Show, Read)

-- | Task which will randomly picked and activated
data PooledTask = PooledTask {
  ptTask :: Task,  -- ^ The task itself.
  ptCoolDown :: Int, -- ^ How long the task should not be activated after finished
  ptLastFinished :: Day, -- ^ When the task was finished the last time
  ptProp :: Float -- ^ Propability if the task is picked
} (Show, Read)

-- | Overall object which will be used for the state
data TastMgmt = TaskMgmt {
  tmActives :: [ActiveTask], -- ^ All activated tasks
  tmPool :: [PooledTask],    -- ^ All tasks which can be activated
  tmToday :: Day             -- ^ Today's day
}


-- | State monad where TaskMgmt is the state.
--
-- This is the main type
type TaskState = State TastMgmt


--- Define equality only on task titles
eqTask :: Task -> Task -> Bool
eqTask task1 task2 = tTitle task1 == tTitle task2

eqATask :: ActiveTask -> ActiveTask -> Bool
eqATask task1 task2 = atTitle task1 == atTitle task2

eqPTask :: PooledTask -> PooledTask -> Bool
eqPTask task1 task2 = ptTitle task1 == ptTitle task2

instance Eq Task where
  (==) = eqTask

instance Eq ActiveTask where
  (==) = eqATask

instance Eq PooledTask where
  (==) = eqPTask


-- | Get the task title from an active task
atTitle :: ActiveTask -> String
atTitle aTask = tTitle $ atTask aTask

-- | Get the task title from a pooled task
ptTitle :: PooledTask -> String
ptTitle pTask = tTitle $ ptTask pTask




--- Basic state accessors
-- | Set new list of active tasks
setActives :: [ActiveTask] -> TaskState ()
setActives tasks = modify $ \st -> st { tmActives = tasks}

-- | Get list of active tasks
getActives ::  TaskState [ActiveTask]
getActives = tmActives `liftM` get

-- | Modify the list of active tasks
modifyActives :: ([ActiveTask] -> [ActiveTask]) -> TaskState ()
modifyActives fn = do
  tasks <- getActives
  let tasks' = fn tasks
  setActives tasks'


-- | Set list of pooled tasks
setPool :: [PooledTask] -> TaskState ()
setPool tasks = modify $ \st -> st { tmPool = tasks }

-- | Get list of pooled tasks
getPool :: TaskState [PooledTask]
getPool = tmPool `liftM` get

-- | Modify the list of active tasks
modifyPool :: ([PooledTask] -> [PooledTask]) -> TaskState ()
modifyPool fn = do
  tasks <- getPool
  let tasks' = fn tasks
  setPool tasks'


-- | Get the today date inside the monad
getToday :: TaskState Day
getToday = tmToday `liftM` get


-- | Add a new active task
addActive :: ActiveTask -> TaskState ()
addActive task = modifyActives $ \tasks -> task : tasks

-- | Add a pooled task
addPool :: PooledTask -> TaskState ()
addPool task = modifyPool $ \tasks -> task : tasks

-- | Mark task with the given title as done
finishTask :: String -> TaskState ()
finishTask taskTitle = do
  today <- getToday
  modifyActiveTask taskTitle $ \task { atFinished = Just today }

-- | Modifier for a active task
modifyActiveTask :: String -> (ActiveTask -> ActiveTask) -> TaskState ()
modifyActiveTask title fn = do
  aTaskMaybe <- lookupActiveTask title
  let aTask' = fn aTask
  replaceActiveTask aTask

-- | Find the active task with the given list
lookupActiveTask :: String -> TaskState (Maybe PooledTask)
lookupActiveTask title = do
  actives <- getActives
  let tuples = map (\x -> (atTitle x, x)) actives
  lookup title tuples


-- |
replacePooledTask :: ActiveTask -> TaskState ()
replacePooledTask pTask = do
  modifyActiveTask 
