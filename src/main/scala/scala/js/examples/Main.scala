package scala.js.examples

object Main extends App {
  Koch.writeHtml("examples/canvas/koch_.html")
  if (args.contains("graphics"))
    Koch.run()
  Birds.writeJs("examples/birds/Bird_.js")
  Twitter.writeJs("examples/ajax/twitter_.js")

  D3.writeHtml("examples/d3/d3_.html")
}
