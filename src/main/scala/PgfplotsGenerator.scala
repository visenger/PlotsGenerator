import java.util

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
    val table: String = generateTable(config)

    //val table: String = latexTableTemplate(tabEnv._1, tabEnv._2)
    val document: String = latexDocumentTemplae(table)

    println("document = " + document)

    //save to file filename.tex
    // run pdflatex <filename>.tex
  }


  def generateTable(config: Any): String = {

    val plots: List[String] = generatePlots(config)

    val table: String = config match {
      case x: List[_] => {
        x.size match {
          case 1 => generate1x1Table(plots)
          case 2 => generate1x2Table(plots)
          case 3 => generate1x3Table(plots)
          case 4 => generate2x2Table(plots)
          case 5 | 6 => generate2x3Table(plots)
        }
      }
      case _ => "error: can not generate table bigger than 6 cells"
    }

    //todo: remove!
    // plots.mkString("\n")
    table
  }

  def generate1x1Table(plots: List[String]) = {

    s"""
       |\\begin{center}
        |\\begin{tabular}{l}
          |% insert cells here
          |${plots.head}
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
  }

  def generate1x2Table(plots: List[String]) = {

    s"""
       |\\begin{center}
        |\\begin{tabular}{ll}
          |% insert cells here
          |
          |${plots.head}
          |&
          |${plots.tail.head}
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
  }

  def generate1x3Table(plots: List[String]) = {


    s"""
       |\\begin{center}
        |\\begin{tabular}{lll}
          |% insert cells here 1x3
          |
          |${plots(0)}
          |&
          |${plots(1)}
          |&
          |${plots(2)}
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
  }

  def generate2x2Table(plots: List[String]) = {
    val nline = "\\\\"
    s"""
       |\\begin{center}
        |\\begin{tabular}{ll}
          |% insert cells here 2x2
          |
          |${plots(0)}
          |&
          |${plots(1)}
          |\\\\
          |${plots(2)}
          |&
          |${plots(3)}
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
  }

  def generate2x3Table(plots: List[String]) = {
    s"""
       |\\begin{center}
        |\\begin{tabular}{lll}
          |% insert cells here 2x3
          |
          |${plots(0)}
          |&
          |${plots(1)}
          |&
          |${plots(2)}
          |\\\\
          |${plots(3)}
          |&
          |${plots(4)}
          |&
          |${if (plots.size == 6) plots(5) else ""}
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
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

    // todo: use tikzpictureEnv method
    val tikzpicture: String = tikzpictureEnv(rawPlot)

    s"%GENERATED: $tikzpicture \n"

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

  def latexTableTemplate(tabularSettings: String, cells: List[String]): String = {
    s"""
       |\\begin{center}
        |\\begin{tabular}{$tabularSettings}
          |% insert cells here
          |${cells.mkString("\n")}
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
  }

  def tikzpictureEnv(singlePlotConfig: Map[String, Any]): String = {
    //todo: analyse the map and convert it into the tikzpicture environment.
    s"""
    |\\begin{tikzpicture}[baseline]
    |     \\begin{loglogaxis}[
    |         title=Title 1,
    |         xlabel={Dof},
    |         ylabel={$$L_\\infty$$ error}, ]
    |         \\addplot table[x=dof,y=Lmax] {/Users/visenger/Documents/tmp/pgfplots/data/datafile.dat};
    |         \\addplot table[x=dof,y=L2] {/Users/visenger/Documents/tmp/pgfplots/data/datafile.dat};
    |         \\legend{$$d=2$$,$$d=3$$},
    |     \\end{loglogaxis}
    |\\end{tikzpicture}
     """.stripMargin
  }


}
