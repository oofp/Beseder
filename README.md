# Beseder

Library for typesafe state machines. Make impossible state transitions impossible and more

_The Hebrew word for “okay” and for “alright” is “beseder” (be-se-der)_

## Beseder goals:

* Provide a way of capturing protocols, APIs, business data model and business rules  in an uniformed manner that covers all aspects of resource lifecycle: resource creation resource disposal, invoking requests, external changes at resources states
* Strong type safety and compile time guarantees: 
  * all allocated resources are released, even in most messy cases of interleaved resource usage
  * only requests that are supported for given resource state are allowed
 * all possible resource state changes are observed
* Effortless parallelism 
* Structured concurrency done naturally and seamlessly 
* Informal notion Linear types: no access to a stale resource states, no state can be left unhandled
* Allows describing properties of the system as constraints, reducing the number of required tests.
* Leverage the powerful type system, type inference and type level computation to enable interactive and entertaining domain exploration and development process.


## Beseder inspirations:

* [Finite-State Machines articles by Oskar Wickström] (https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html)
* [Control Flow in Haskell - Flow with Variant by Sylvain Henry] (https://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-3.html)
* [Indexed Monads by Kwang Yul Seo] (https://kseo.github.io/posts/2017-01-12-indexed-monads.html)
* [Thinking with types by Sandy Maguire] (https://leanpub.com/thinking-with-types) 
* [Introducing ST: Working with State] (http://docs.idris-lang.org/en/latest/st/state.html)
