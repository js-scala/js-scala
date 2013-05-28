package scala.js.examples

import scala.js.language.{JS, Dynamics}
import scala.js.exp.{JSExp, DynamicsExp}
import scala.js.gen.js.{GenJSOpt, GenDynamics}
import java.io.PrintWriter

object D3 {

  trait D3API extends Dynamics {
    def d3 = inlineDynamic("d3")
    def js_this = inlineDynamic("this")
  }

  trait D3APIExp extends D3API with DynamicsExp

  trait JSGenD3API extends GenDynamics {
    val IR: D3APIExp
    import IR._
  }

  // example from http://christopheviau.com/d3_tutorial/
  trait SimpleExample { this: JS with D3API =>
    def draw() {
      val sampleSVG = d3.select("#viz")
          .append("svg:svg")
          .attr("width", 100)
          .attr("height", 100)

      sampleSVG.append("svg:circle")
          .style("stroke", "gray")
          .style("fill", "white")
          .attr("r", 40)
          .attr("cx", 50)
          .attr("cy", 50)
          .on("mouseover", fun{() => d3.select(js_this).style("fill", "aliceblue")})
          .on("mouseout", fun{() => d3.select(js_this).style("fill", "white")})
    }
  }

  def codegen(pw: PrintWriter) {
    new SimpleExample with JSExp with D3APIExp { self =>
      val codegen = new GenJSOpt with JSGenD3API { val IR: self.type = self }
      codegen.emitExecution(draw(), pw)
    }
  }

  def genHtml = {
    <html>
    <head>
      <script type="text/javascript" src="http://mbostock.github.com/d3/d3.js"></script>
    </head>
    <body>
      <div id="viz"></div>
      <script type="text/javascript">
      {
        val writer = new java.io.StringWriter
        codegen(new java.io.PrintWriter(writer))
        scala.xml.Unparsed(writer.toString)
      }
      </script>
    </body>
    </html>
  }

  def writeHtml(filename: String) = {
    scala.xml.XML.save(filename, genHtml)
  }
}
