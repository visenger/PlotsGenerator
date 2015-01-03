import scala.io.{BufferedSource, Source}
import scala.util.parsing.json.JSON

/**
 * Created by visenger on 29/12/14.
 */
object PgfplotsGenerator {


  def main(args: Array[String]) {
    val parsedConfig: Map[String, Any] = parseConfiguration("src/main/resources/pgfplots.conf")

    val config = parsedConfig.getOrElse("accounts", Map())

    //generate table environment according to the number of plots
    val tabEnv: (String, String) = generateTable(config)

    val table: String = latexTableTemplate(tabEnv._1, tabEnv._2)
    val document: String = latexDocumentTemplae(table)

    println("document = " + document)

    //save to file filename.tex
    // run pdflatex <filename>.tex
  }

  def generateTable(config: Any): (String, String) = {
    config match {
      case x: List[_] => {
        x.size match {
          case 2 =>
          case 3 =>
          case 4 =>
          case 5 =>
          case 6 =>
        }
      }
      case x: Map[String, Any] =>
    }
    val plots: List[String] = generatePlots(config)
    ("lll", plots.mkString("\n"))
  }

  def parseConfiguration(path: String): Map[String, Any] = {
    //    val source: BufferedSource = Source.fromURL(getClass().getResource("pgfplots.conf"))
    //    val configAsString: String = source.getLines().mkString("\n")
    //    println("configAsString = " + configAsString)
    val json: String = Source.fromFile(path).getLines().mkString("\n")

    //parse json

    val parsedJson: Option[Any] = JSON.parseFull(json)
    val afterParseMap: Map[String, Any] = parsedJson match {
      case Some(x) => x.asInstanceOf[Map[String, Any]]
      case None => Map[String, Any]()
    }
    afterParseMap
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
    //generate the deepest element.... zoom out
    val rawPlot: Map[String, Any] = json.getOrElse("plot", Map()).asInstanceOf[Map[String, Any]]


    s"generated plot: $json; \n\n"

  }

  def latexDocumentTemplae(latexTable: String): String = {
    s"""
        |\\documentclass[a4paper, landscape]{article}
          |\\usepackage{pgfplots}
          |\\pgfplotsset{small,  compat=1.5}
          |\\begin{document}

          |\\pgfplotsset{
          |small
          |}
          |% insert table here
          |$latexTable
        |\\end{document}
     """.stripMargin
  }

  def latexTableTemplate(tabularSettings: String, cells: String): String = {
    s"""
       |\\begin{center}
        |\\begin{tabular}{$tabularSettings}
          |% insert cells here
          |$cells
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
  }


}
