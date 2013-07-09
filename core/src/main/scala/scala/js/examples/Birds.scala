package scala.js.examples

import java.io.PrintWriter
import scala.js.exp.JSExp
import scala.js.gen.js.GenJSOpt
import scala.js.language.JS

object Birds {

  trait BirdProg { this: JS =>
    def Bird() = {
      val self = inlineDynamic("this")
      val scope = self
      inlineDynamic("THREE.Geometry").call( self )

      v(   5,   0,   0 )
      v( - 5, - 2,   1 )
      v( - 5,   0,   0 )
      v( - 5, - 2, - 1 )

      v(   0,   2, - 6 )
      v(   0,   2,   6 )
      v(   2,   0,   0 )
      v( - 3,   0,   0 )

      f3( 0, 2, 1 )
      f3( 4, 7, 6 )
      f3( 5, 6, 7 )

      self.computeCentroids()
      self.computeFaceNormals()

      def v = fun { (x: Rep[Int], y: Rep[Int], z: Rep[Int]) =>
	scope.vertices.push(newDynamic("THREE.Vertex")(newDynamic("THREE.Vector3")(x, y, z)))
      }

      def f3 = fun { (a: Rep[Int], b: Rep[Int], c: Rep[Int]) =>
	scope.faces.push(newDynamic("THREE.Face3")(a, b, c))
      }

      self
    }
  }

  def codegen(pw: PrintWriter) {
    new BirdProg with JSExp { self =>
      val codegen = new GenJSOpt { val IR: self.type = self }
      codegen.emitSource0(Bird _, "Bird", pw)
    }
  }

 def writeJs(filename: String) = {
   val out = new PrintWriter(filename)
   codegen(out)
   out.println("Bird.prototype = new THREE.Geometry()")
   out.println("Bird.prototype.constructor = Bird")
   out.close()
 }
}
