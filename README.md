# Beseder

Library for typesafe state machines. Make impossible state transitions impossible and more

_The Hebrew word for “okay” and for “alright” is “beseder” (be-se-der)_

## Beseder goals:

* Provide a way of capturing protocols, APIs, business data model and business rules  in an uniformed manner that covers all aspects of resource lifecycle: resource creation resource disposal, invoking requests, external changes at resources states
* Strong type safety and compile time guarantees: 
  * all allocated resources are released, even in most messy cases of interleaved resource usage
  * only operations that are supported for given resource state are allowed
 * all possible resource state changes are observed
* Effortless parallelism 
* Structured concurrency done naturally and seamlessly 
* Informal notion of linear types: no access to a stale resource states, no state can be left unhandled
* Allows describing properties of the system as constraints, reducing the number of required tests.
* Leverage the powerful type system, type inference and type level computation to enable interactive and entertaining domain exploration and development process.


## Beseder inspirations:

* [Finite-State Machines articles by Oskar Wickström] (https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html)
* [Control Flow in Haskell - Flow with Variant by Sylvain Henry] (https://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-3.html)
* [Indexed Monads by Kwang Yul Seo] (https://kseo.github.io/posts/2017-01-12-indexed-monads.html)
* [Thinking with types by Sandy Maguire] (https://leanpub.com/thinking-with-types) 
* [Introducing ST: Working with State] (http://docs.idris-lang.org/en/latest/st/state.html)

## Beseder core concepts:

* The basic entity at Beseder is a resource. The system state at any given moment is represented by the states of the resources. In other words state of the system is a product of the system's resources states.

* Resources are created, going through a series of state transitions and eventually deleted. 
The state transition is caused by request (operation) invoked on the resource or change in resource state that happened outside of the system (unsolicited state change). Every resource state is represented by a specific distinctive Haskell type. The following type classes capture all supported resource related operations:

```
--create resource
class MkRes ctx res  where
    type ResSt ctx res  :: *
    mkRes :: res -> ctx (ResSt ctx res)

--clear resource    
class TermState ctx state where
    terminate :: state -> ctx ()
  
--unsolicted resource state transition    
class Transition ctx state1 where
    type NextStates state1 :: [*]
    next :: state1 -> (V (NextStates state1) -> ctx Bool) -> ctx Bool
  
-- request invocation    
class Request ctx req state1 where
    type ReqResult (req :: *) (st :: *) :: [*]
    request :: req -> state1 -> ctx (V (ReqResult req state1))
 
```
### Resource definition
The simplest way to create new resource is to use type class that defines resource initial and terminated states, as well as supported requests and state transitions for every state. Then we call _buildRes_ that uses Template Haskell to generate all required declarations. As a bonus buildRes also generates resource state diagram 

* Here is an example of *[CardReader](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/Resources/CardReaderRes.hs)* resource definition from *[ATM demo program](https://github.com/oofp/beseder-demo-apps)*: 

![CardReader](https://user-images.githubusercontent.com/25211514/70853186-9dc40700-1e78-11ea-8bdf-1ac5ccd80b52.png)


### Example of Beseder application (inspired by my condo door behavior):
Door (*door resource*) gets open when either the fob is read (*fobReader*) or internal proximity sensor (*inDet*) was triggered. The door should stay open for predefined time interval (*doorTimeoutSec*). The door should stay open as long as at least one of proximity sensors (*inDet* or *outDet*) are on  

```
doorHandler doorTimeoutSec = 
  handleEvents $ do
    on @("fobReader" :? IsMessageReceived) $ do 
      invoke #fobReader GetNextMsg
      openDoorIfClosed doorTimeoutSec       
    on @("inDet" :? IsBinMonitorOn) $ do 
      openDoorIfClosed doorTimeoutSec    
    on @("doorTimer" :? IsTimerTriggered) $ do 
      onOrElse @("inDet" :? IsBinMonitorOn :|| "outDet" :? IsBinMonitorOn)
        (restartTimer doorTimeoutSec)
        closeDoor        

openDoorIfClosed :: Int -> STransData m sp _ ()     
openDoorIfClosed doorTimeoutSec = do
  on @("door" :? IsBinSwitchOff) $ do
    invoke #door TurnOn
    newRes #doorTimer TimerRes
    invoke #doorTimer (StartTimer doorTimeoutSec)

closeDoor :: STransData m sp _ () 
closeDoor = do
  clear #doorTimer   
  invoke #door TurnOff

restartTimer :: Int -> STransData m sp _ () 
restartTimer doorTimeoutSec = do
  clear #doorTimer
  newRes #doorTimer TimerRes
  invoke #doorTimer (StartTimer doorTimeoutSec)

```
and here is state diagram extracted for this application:

![EntranceDoor](https://user-images.githubusercontent.com/25211514/73844951-9a515a00-47ef-11ea-8874-65b12d607588.png)
