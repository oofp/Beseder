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
