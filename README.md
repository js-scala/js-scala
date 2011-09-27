### Setup

* From [`scala-dev`](http://github.com/gkossakowski/scala-dev):
  * Run `git checkout virtualized-master`.
  * Run `ant replacelocker`.
  * Run `git checkout js`.
  * Run `ant clean && ant fastdist`.

* From [`virtualization-lms-core`](http://github.com/TiarkRompf/virtualization-lms-core):
  * Run `git checkout delite-develop`.
  * Create a file `local.properties` with the line `scala.virtualized.home=<path-to-scala-virtualized>/dists/latest`.
  * Run `sbt update`, then `sbt publish-local`. You can run the tests with `sbt test`.

* From this project:
  * Create a file `local.properties` with the line `scala.virtualized.home=<path-to-scala-virtualized>/dists/latest`.
  * Run `sbt update`.

### Run
* From this project:
  * Run `sbt run`.