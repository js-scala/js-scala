import java.io.PrintWriter

object Koch {

  trait KochProg { this: JS =>
    val deg = Math.PI/180;    // For converting degrees to radians

    def snowflake(c_ : Rep[Any], n : Rep[Double], x : Rep[Double], y : Rep[Double], len : Rep[Double]) {
      val c = dynamic(c_)
      // Draw a single leg of a level-n Koch snowflake.
      // This function leaves the current point at the end of the leg it has
      // drawn and translates the coordinate system so the current point is (0,0).
      // This means you can easily call rotate() after drawing a leg.
      def leg: Rep[Double => Any] = fun { n : Rep[Double] =>
        c.save();             // Save the current transformation
        if (n == 0) {         // Nonrecursive case:
          c.lineTo(len, 0);   //   Just draw a horizontal line
        }                     //                                       _  _
        else {                // Recursive case: draw 4 sub-legs like:  \/
          c.scale(1/3,1/3);   // Sub-legs are 1/3rd the size of this leg
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
  }

  def codegen(pw: PrintWriter) {
    new KochProg with JSExp { self =>
      val codegen = new JSGen { val IR: self.type = self }
      codegen.emitSource5(snowflake _, "snowflake", pw)
    }
  }
}
