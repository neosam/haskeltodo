
{-|
Module      : Todo
Description : Core todo module.
Copyright   : (c) Simon Goller, 2016
License     : BSD

Provides the basic functionality for the tasks.

-}

module Todo (
  -- * Data types
  Task(Task, tTitle, tDesc, tFactor),
  ActiveTask(ActiveTask, atTask, atDue, atFinished),
  PooledTask(PooledTask, ptTask, ptDueOffset, ptCoolDown, ptLastFinished, ptProp),
  TaskMgmt,
  TaskState,

  -- * TaskMgmt accessor fuctions
  addActive,
  addActiveM,

  addPool,
  addPoolM,

  activate,
  activateM,

  getOverdue,
  getOverdueM,

  -- * Generate and updating 'TaskMgmt'
  emptyTaskMgmt,
  newTaskMgmt,
  updateTaskMgmt,

  taskMgmtFromString,
  taskMgmtToString
  )
       where


import Data.Time (Day(ModifiedJulianDay), addDays, diffDays,
                  utctDay, getCurrentTime)
import Control.Monad.State.Lazy (State, get, modify,
                                 execState, evalState)
import Control.Monad (liftM, filterM)
import Data.List (delete)
import Data.Maybe (isNothing)
import System.Random (StdGen, random, mkStdGen, newStdGen)

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
} deriving (Show, Read)

-- | Task which will randomly picked and activated
data PooledTask = PooledTask {
  ptTask :: Task,  -- ^ The task itself.
  ptDueOffset :: Int, -- ^ How long it should be inactive until it can be
                      --   activated again after it was finished
  ptCoolDown :: Int, -- ^ How long the task should not be activated after finished
  ptLastFinished :: Day, -- ^ When the task was finished the last time
  ptProp :: Float -- ^ Propability if the task is picked
} deriving (Show, Read)

-- | Overall object which will be used for the state
data TaskMgmt = TaskMgmt {
  tmActives :: [ActiveTask],   -- ^ All activated tasks
  tmPool :: [PooledTask],      -- ^ All tasks which can be activated
  tmToday :: Day,              -- ^ Today's day
  tmRand :: StdGen             -- ^ The random number generator
}


-- | State monad where TaskMgmt is the state.
--
-- This is the main type
type TaskState = State TaskMgmt


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


-- | Get the random generator
getRand :: TaskState StdGen
getRand = tmRand `liftM` get

-- | Set the random generator
setRand :: StdGen -> TaskState ()
setRand rand = modify $ \st -> st { tmRand = rand }



-- | Add a new active task
addActiveM :: ActiveTask -> TaskState ()
addActiveM task = modifyActives $ \tasks -> task : tasks

-- | Add a pooled task
addPoolM :: PooledTask -> TaskState ()
addPoolM task = modifyPool $ \tasks -> task : tasks

-- | Mark task with the given title as done
finishTask :: String -> TaskState ()
finishTask taskTitle = do
  today <- getToday
  _ <- modifyActiveTask taskTitle $ \task -> task { atFinished = Just today }
  return ()

-- | Modifier for a active task
modifyActiveTask :: String -> (ActiveTask -> ActiveTask) -> TaskState (Maybe String)
modifyActiveTask title fn = do
  aTaskMaybe <- lookupActiveTask title
  case aTaskMaybe of
    Nothing -> return $ Just "Title not found"
    Just aTask -> do
      let aTask' = fn aTask
      replaceActiveTask aTask
      return Nothing

-- | Find the active task with the given list
lookupActiveTask :: String -> TaskState (Maybe ActiveTask)
lookupActiveTask title = do
  actives <- getActives
  let tuples = map (\x -> (atTitle x, x)) actives
  return $ lookup title tuples


-- | Replaces the task with the given title with the new one
replaceActiveTask :: ActiveTask -> TaskState ()
replaceActiveTask aTask = do
  aTasks <- getActives
  modifyActives $ \tasks -> delete aTask aTasks
  addActiveM aTask

-- | Remove completed tasks from active tasks
cleanupActives :: TaskState ()
cleanupActives = modifyActives $ \tasks -> filter isDone tasks

-- | Check if the active task is done.
isDone :: ActiveTask -> Bool
isDone aTask = isNothing $ atFinished aTask


-- | Get the overdue tasks
getOverdueM :: TaskState [ActiveTask]
getOverdueM = do
  actives <- getActives
  today <- getToday
  let actives' = filter (\x -> atDue x > today) actives
  return actives'


-- | Produce a random float number
randomFloat :: TaskState Float
randomFloat = do
  rand <- getRand
  let (val, rand') = random rand
  setRand rand'
  return val

-- | Randomly activate pooled tasks
activateM :: TaskState ()
activateM = do
  tasksToActivate <- pickPooledTasks
  activateTasks tasksToActivate

-- | Pick tasks which can be activated from the pool
pickPooledTasks :: TaskState [PooledTask]
pickPooledTasks = do
  pool <- getPool
  pool' <- filterM isPotentialPooledTask pool
  return pool'

-- | Check if task can be activated and randomly activate according to its
--   prop attribute.
--
--   The criteria are:
--   * Is the task not cooling down.  If it was completed earlier it cannot
--     be activated.
--   * Is the task not active.  If it is it makes no sense to add it again.
isPotentialPooledTask :: PooledTask -> TaskState Bool
isPotentialPooledTask task = do
  cooldown <- isCoolingDown task
  active <- isPooledTaskActive task
  randVal <- randomFloat
  let taskVal = ptProp task
  return $ not cooldown && not active && (randVal < taskVal)

-- | Check if the task is cooling dows right now
--
--   A task shall not be picked if it was finished recently.  The days where
--   it should not be picked is defined in the 'PooledTask'.
isCoolingDown :: PooledTask -> TaskState Bool
isCoolingDown task = do
  today <- getToday
  let taskFinished = ptLastFinished task
      cooldown = ptCoolDown task
  return $ toInteger (diffDays today taskFinished) < (toInteger cooldown)

-- | Check if a pooled task is already active.
--
--   A pooled task should not be activated twice.
isPooledTaskActive :: PooledTask -> TaskState Bool
isPooledTaskActive pTask = do
  actives <- getActives
  let actives' = map atTask actives
      task = ptTask pTask
  return $ task `elem` actives'

-- | Calculate 'ActiveTask's out of the 'PooledTask's and activate them
activateTasks :: [PooledTask] -> TaskState ()
activateTasks pTasks = do
  aTasks <- mapM pTasksToActives pTasks
  modifyActives $ \actives -> aTasks ++ actives

-- | Transform a 'PooledTask' to an 'ActiveTask'.
pTasksToActives :: PooledTask -> TaskState ActiveTask
pTasksToActives pTask = do
  today <- getToday
  let dueOffset = ptDueOffset pTask
      due = addDays (toInteger dueOffset) today
  return $ ActiveTask {
    atTask = ptTask pTask,
    atDue = due,
    atFinished = Nothing
  }



--- State modifiers/getter for outside the State Monad
-- | Add a new 'ActiveTask'
addActive :: ActiveTask -> TaskMgmt -> TaskMgmt
addActive aTask tm = execState (addActiveM aTask) tm

-- | Add a new 'PooledTask'
addPool :: PooledTask -> TaskMgmt -> TaskMgmt
addPool pTask tm = execState (addPoolM pTask) tm

-- | Get all overdue 'ActiveTask's
getOverdue :: TaskMgmt -> [ActiveTask]
getOverdue tm = evalState getOverdueM tm

-- | Randomty activate 'PooledTask's
activate :: TaskMgmt -> TaskMgmt
activate tm = execState activateM tm

-- | Get all 'ActiveTask's
allActives :: TaskMgmt -> [ActiveTask]
allActives = tmActives

-- | Get all 'PooledTask's
allPooled :: TaskMgmt -> [PooledTask]
allPooled = tmPool


--- Creation, seriatization and updating
-- | Empty TaskMgmt.  Time is set to zero and always uses
--   the same random number generator
emptyTaskMgmt :: TaskMgmt
emptyTaskMgmt = TaskMgmt {
  tmActives = [],
  tmPool = [],
  tmToday = ModifiedJulianDay 0,
  tmRand = mkStdGen 0
}

-- | Update the TaskMgmt with random generator and set the right today date.
updateTaskMgmt :: TaskMgmt -> IO TaskMgmt
updateTaskMgmt tm = do
  timeNow <- getCurrentTime
  let today = utctDay timeNow
  rand <- newStdGen
  return $ tm {
    tmToday = today,
    tmRand = rand
  }

-- | New 'TaskMgmt' which is ready to use
newTaskMgmt :: IO TaskMgmt
newTaskMgmt = updateTaskMgmt emptyTaskMgmt

-- | Turns 'TaskMgmt' into its String representation
taskMgmtToString :: TaskMgmt -> String
taskMgmtToString tm =
  show (tmActives tm, tmPool tm, tmToday tm)

-- | Trunsforms string generated by taskMgmtToString back to a 'TaskMgmt'
taskMgmtFromString :: String -> TaskMgmt
taskMgmtFromString str =
  let (actives, pool, day) = read str
  in emptyTaskMgmt {
       tmActives = actives,
       tmPool = pool,
       tmToday = day
     }
