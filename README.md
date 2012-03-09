# JavaScript as an embedded DSL in Scala #

### Setup

1. Setup [virtualization-lms-core](http://github.com/gkossakowski/virtualization-lms-core), including Scala-Virtualized.

2. Copy the file `local.properties` from virtualization-lms-core to this root project directory.

3. Run `sbt`. `test` to ensure everything works. Then `publish-local`. You can also run the dummy `Main` program with `run`.

### Further projects

[play-js-validation](http://github.com/namin/play-js-validation) uses this DSL to enable form validation code in Play 2.0 to be written once and checked on both client and server sides.