module Rede.Utils.Alarm 
    (
        newAlarm,
        cancelAlarm,
        Alarm,
        AlarmCancelResult
    )
    where 


import Control.Concurrent
-- import Control.Concurrent.MVar 


data AlarmCancelResult a = 
     Success_ACR
    |AlreadyFired_ACR (MVar a)


data AlarmState = 
     Waiting 
    |Cancelled
    |HandlerStarted


data Alarm a = 
    Alarm_AtFire (MVar a) (MVar AlarmState)


newAlarm ::  Int -> IO a ->IO (Alarm a)
newAlarm wait_time comp = do
    wait_for_a <- newEmptyMVar
    alarm_state <- newMVar Waiting
    forkIO $ do 
        threadDelay wait_time 
        modifyMVar_ alarm_state $ \ state -> case state of 

            Cancelled -> return Cancelled

            Waiting   -> do 
                forkIO $ do
                    a <- comp 
                    putMVar wait_for_a a 
                return $ HandlerStarted 

            HandlerStarted -> error "Logical error"

    return $ Alarm_AtFire wait_for_a alarm_state


cancelAlarm :: Alarm a -> IO (AlarmCancelResult a)
cancelAlarm (Alarm_AtFire wait_for_a alarm_state) = do 
    modifyMVar alarm_state $ \ state -> case state of 

        Waiting -> do 
            return (Cancelled, Success_ACR)

        Cancelled -> do 
            return (Cancelled, Success_ACR)

        HandlerStarted -> do 
            return $ (HandlerStarted, AlreadyFired_ACR wait_for_a)




