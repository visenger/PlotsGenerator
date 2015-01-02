import scala.io.{BufferedSource, Source}
import scala.util.parsing.json.JSON

/**
 * Created by visenger on 29/12/14.
 */
object PgfplotsGenerator {


  def main(args: Array[String]) {
    //read config file

    //    val source: BufferedSource = Source.fromURL(getClass().getResource("pgfplots.conf"))
    //    val configAsString: String = source.getLines().mkString("\n")
    //    println("configAsString = " + configAsString)

    val json: String = Source.fromFile("src/main/resources/pgfplots.conf").getLines().mkString("\n")

    //parse json

    val parsedJson: Option[Any] = JSON.parseFull(json)
    val afterParseMap: Map[String, Any] = parsedJson match {
      case Some(x) => x.asInstanceOf[Map[String, Any]]
      case None => Map[String, Any]()
    }

    val plots: List[String] = generatePlots(afterParseMap.getOrElse("accounts", Map()))
    plots.foreach(println)



    //generate the diepest element.... zoom out
    //generate table environment according to the number of plots
    //generate document environment
    //save to file filename.tex
    // run pdflatex <filename>.tex
  }

  def generatePlots(input: Any): List[String] = {
    val plotsStr: List[String] = input match {
      case x :: xs => {
        val firstStr: String = x match {
          case y: Map[String, Any] => generateSinglePlot(y)
          case _ => ""
        }
        firstStr :: generatePlots(xs)
      }
      case x: Map[String, Any] => generateSinglePlot(x) :: Nil
      case _ => Nil // todo: change this!
    }


    plotsStr


  }

  def generateSinglePlot(json: Map[String, Any]): String = {
    s"generated plot: $json; \n\n"
  }

}
