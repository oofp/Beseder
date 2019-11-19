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

## Beseder milestones and Aha moments:

* The system is represented by a list of possible system states. Each entry of the list is the product of the resources states. Program is a sequence of  steps that fold and unfold the list by creating resources, clearing resources, invoking operations on the resources and observing unsolicted changes of resources states. To be executable , program should start and end with empty state.
* The app developer can enquire list content and use it to decide about the next step of the program. 
* The program is represented by AST implemented by GADT parameterized  by splitter that determines to what part of the list the operation is applied
* Defunctionalization trick (that enabled partial type family applications) allows representing splitter as type level predicate. 

* GADT representing AST step is also parameterized by type level function (again using defunctionalization trick) that transforms input list of system states to output list of the states.  It is used at type level computation for example to compute all possible states within event handling loop.

* There is also alternative (and probably preferable) way of writing program by defining program as type (approach that is somewhat similar to Servant). The required term (value)  level computations are indicated by named placeholder (patches) which are resolved   when reifying the application type  to application AST. The correctness of the type can be quickly tested by using type level computation applied to input list of states. It helps to overcome slow compilation time , since time consuming reifying can be infrequently while type level computation on app type (invoked using :kind! from ghci) is very quick.

* AST can be transformed (for example, instrumented for logging or presentation possible). It seems to open very interesting and powerful possibilities (may be even UI generation) that still needs to be explored further.    
 
* Implementing "resources as type". Resource consists of other (internal resources). Resource creating, internal resources state change and requests can be described as types. Important "aha" moment was the realization that request invocation can be done with no breaking the soundness.


