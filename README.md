# JavaScript as an embedded DSL in Scala #

### Documentation

* ECOOP 2012 paper ([PDF](https://github.com/js-scala/js-scala/blob/master/papers/ecoop2012/article.pdf?raw=true))
* [Scala Days 2012 talk](http://skillsmatter.com/podcast/scala/javascript-embedded-dsl-scala)

### Setup

1. Setup [virtualization-lms-core](http://github.com/js-scala/virtualization-lms-core), including Scala-Virtualized. Ensure the `SCALA_VIRTUALIZED_VERSION` environment variable is set consistently.

2. Run `sbt`. `test` to ensure everything works. Then `publish-local`.

### Further projects

* [play-js-validation](http://github.com/js-scala/play-js-validation) uses this DSL to enable form validation code in Play 2.0 to be written once and checked on both client and server sides.

* [forest](http://github.com/js-scala/forest) uses this DSL to enable HTML templates to be written once and shared between client and server sides, both for initial rendering and automatic updating.
