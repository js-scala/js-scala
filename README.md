# JavaScript as an embedded DSL in Scala #

### Presentations / Papers

* ECOOP 2012 paper ([PDF](https://github.com/js-scala/js-scala/blob/paper-revision/papers/ecoop2012/article.pdf?raw=true))
* [Scala Days 2012 talk](http://skillsmatter.com/podcast/scala/javascript-embedded-dsl-scala)

### Setup

1. Setup [virtualization-lms-core](http://github.com/gkossakowski/virtualization-lms-core), including Scala-Virtualized.

2. Copy the file `local.properties` from virtualization-lms-core to this root project directory.

3. Run `sbt`. `test` to ensure everything works. Then `publish-local`. You can also run the dummy `Main` program with `run`.

### Further projects

[play-js-validation](http://github.com/namin/play-js-validation) uses this DSL to enable form validation code in Play 2.0 to be written once and checked on both client and server sides.
