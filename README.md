# Hippo

> A web-based heap dump viewer in full-stack Scala

Status (as of **October 2021**): 

I am actively trying to abandon this project as I've learnt what I wanted from
it. But it's hard. There's something so cool about it (mostly scodec part).

Very feature poor, barely a proof-of-concept.

## About

This project serves purely as an exercise for learning [scodec](https://github.com/scodec/scodec) and [Waypoint](https://github.com/raquo/waypoint), as well as Scala 3 and its new syntax.

It should be as simple as running this (in SBT shell):

> cli/run --hprof heapdump-1631032668990.hprof

And it will launch the server with frontend at http://localhost:9000

https://user-images.githubusercontent.com/1052965/140199076-a9268b4b-864e-4a17-b9ad-a7937024dd61.mp4

Not a single thing here was possible without the libraries of incredible quality one can find in Scala ecosystem. I am but a toddler fitting chiseled marble pieces into perfectly shaped holes, unfit to hold the chisel, admiring the glory of it all.

## Modules and some comments

- Analyser ([modules/analyser](modules/analyser))
  - Scala 3.1, [Scodec 2.x](https://github.com/scodec/scodec)
  - Pretty gratuitous usage of the word "module"
  - Responsible for reading the heap dump and converting it into data models we can exchange with frontend (see [codecs.scala](modules/analyser/src/main/scala/codecs.scala))


- Backend ([modules/backend](modules/backend))
   - Scala 3.1, [Http4s 0.23](https://http4s.org/), [Cats Effect 3](https://typelevel.org/cats-effect/), [fs2](https://fs2.io/)
   - Responsible for 
     - Preparing some views, e.g. "valid string", "strings that are class names", etc. See [views.scala](modules/backend/src/main/scala/hippo/backend/Views.scala)
     - Defining a HTTP API that the frontend can call to retrieve information about the heap dump. See [routes.scala](modules/backend/src/main/scala/hippo/backend/Routes.scala)

- Shared ([modules/shared](modules/shared))
  - Scala 3.1, [Circe](https://github.com/circe/circe/)
  - Defines strongly typed models for the dozens of different parts of heap profile information (see [models.scala](modules/shared/src/main/scala/models.scala))
    - This part makes heavy use of [opaque type aliases](https://docs.scala-lang.org/scala3/reference/other-new-features/opaques-details.html) to ensure zero overhead of wrapping things like various identifiers (which are all `Long`s) in different types, to reduce the possible errors.

      There's 14 different data types that are `Long`s under the hood and 22 that are various identifiers (size of which varies depending on the platform)

      The usage of opaque types here was important for me to understand the ergonomics for the end user when a lot are required. Another important factor was memory usage - heap profiles grow **very** quickly, and preserving typesafety while adding `case class` overhead to each identifier would've been prohibitive.

      That said, I've not made a "before" and "after" comparison of memory usage, because that would be a lot of effort. I'm making some educated guesses based on the bytecode that is generated, but that's it.

  - Defines models which are important for protocol, but not part of heap profile definitions, like summaries produced only for the purposes of rendering on the frontend

- CLI ([modules/cli](modules/cli))
  - This is the entry point with a single runnable class, which is responsible for:
    - Loading the heap dump
    - Instantiating the Http4s routes that interrogate said dump
    - (on the build level) Scala.js frontend (see below) is being added to module's resources as a single optimised JS file
    - Launches the Http4s server with both the backend routes and the routes serving frontend (and other assets)
  
- Frontend ([modules/frontend](modules/frontend))
  - Scala 3.1, [Scala.js](https://www.scala-js.org/), [Laminar](https://laminar.dev/), [Waypoint](https://github.com/raquo/waypoint) and transitively Circe (from Shared), [Sttp](https://sttp.softwaremill.com/en/latest/)
  
  - This is a single page application (SPA) with some very basic rendering of the data retrieved from the backend about the heap dump.
  
  - Waypoint is used for managing in-browser URLs, page state, history, etc. The rest is implemented in vanilla Laminar

  - Sttp is used to communicate with the backend (it defers to built-in `fetch` in the browser, but provides a nice uniform API in Scala)