package scala.js

import java.io.PrintWriter

object Koch {

  trait KochProg { this: JS with Doms =>
    val deg = Math.PI/180;    // For converting degrees to radians

    def snowflake(c : DynamicRep, n : Rep[Int], x : Rep[Int], y : Rep[Int], len : Rep[Int]) {
      // Draw a single leg of a level-n Koch snowflake.
      // This function leaves the current point at the end of the leg it has
      // drawn and translates the coordinate system so the current point is (0,0).
      // This means you can easily call rotate() after drawing a leg.
      def leg: Rep[Int => Any] = fun { n : Rep[Int] =>
        c.save();             // Save the current transformation
        if (n == 0) {         // Nonrecursive case:
          c.lineTo(len, 0);   //   Just draw a horizontal line
        }                     //                                       _  _
        else {                // Recursive case: draw 4 sub-legs like:  \/
          c.scale(1.0/3,1.0/3);   // Sub-legs are 1/3rd the size of this leg
          leg(n-1);           // Recurse for the first sub-leg
          c.rotate(60*deg);   // Turn 60 degrees clockwise
          leg(n-1);           // Second sub-leg
          c.rotate(-120*deg); // Rotate 120 degrees back
          leg(n-1);           // Third sub-leg
          c.rotate(60*deg);   // Rotate back to our original heading
          leg(n-1);           // Final sub-leg
        }
        c.restore();          // Restore the transformation
        c.translate(len, 0);  // But translate to make end of leg (0,0)
      }

      c.save();               // Save current transformation
      c.translate(x,y);       // Translate origin to starting point
      c.moveTo(0,0);          // Begin a new subpath at the new origin
      leg(n);                 // Draw the first leg of the snowflake
      c.rotate(-120*deg);     // Now rotate 120 degrees counterclockwise
      leg(n);                 // Draw the second leg
      c.rotate(-120*deg);     // Rotate again
      leg(n);                 // Draw the final leg
      c.closePath();          // Close the subpath
      c.restore();            // And restore original transformation
    }

    def draw() {
      val canvas = document.getElementById("canvas");
      val c = canvas.getContext("2d");
      snowflake(c,0,5,115,125);    // A level-0 snowflake is an equilateral triangle
      snowflake(c,1,145,115,125);  // A level-1 snowflake is a 6-sided star
      snowflake(c,2,285,115,125);  // etc.
      snowflake(c,3,425,115,125);
      snowflake(c,4,565,115,125);  // A level-4 snowflake looks like a snowflake!
      c.stroke();                  // Stroke this very complicated path
    }
  }

  def codegen(pw: PrintWriter) {
    new KochProg with JSExp with DomsExp { self =>
      val codegen = new JSGenOpt with GenDoms { val IR: self.type = self }
      //codegen.emitSource5(snowflake _, "snowflake", pw)
      //It's enough to emit draw, it will call snowflake and will inline
      //what's needed. It's quite amazing, actually.
      codegen.emitSource0(draw _, "draw", pw)
    }
  }

  def genHtml = {
    <html>
      <head>
        <title>Koch</title>
        <script type="text/javascript">
          {
            val writer = new java.io.StringWriter
            codegen(new java.io.PrintWriter(writer))
            scala.xml.Unparsed(writer.toString)
          }
        </script>
      </head>
      <body onLoad="draw();">
        <canvas id="canvas" width="1000" height="500"></canvas>
      </body>
    </html>
  }

  def writeHtml(filename: String) = {
    scala.xml.XML.save(filename, genHtml)
  }
}
