### Setup

From [`scala-virtualized`](http://github.com/TiarkRompf/scala-virtualized):
Run `ant fastdist`.

From [`virtualization-lms-core`](http://github.com/TiarkRompf/virtualization-lms-core):
Create a file `local.properties` with the line `scala.virtualized.home=<path-to-scala-virtualized>/dists/latest`.
Run `sbt update`, then `sbt publish-local`. You can run the tests with `sbt test`.

From this project:
Create a file `local.properties` with the line `scala.virtualized.home=<path-to-scala-virtualized>/dists/latest`.
Run `sbt update`.

### Run
Run `sbt run`.