import java.io.BufferedWriter

import scala.io.Source
import scala.util.parsing.json.JSON

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * Created by visenger on 29/12/14.
 */
object PgfplotsGenerator {


  def main(args: Array[String]) {
    val parsedConfig: Map[String, Any] = parseConfiguration("src/main/resources/pgfplots.conf")

    val config = parsedConfig.getOrElse("plots", Map())

    //generate table environment according to the number of plots
    val table: String = generateTable(config)

    //val table: String = latexTableTemplate(tabEnv._1, tabEnv._2)
    val document: String = latexDocumentTemplae(table)

    println("document = " + document)
    writeToFile(document, "/Users/visenger/Documents/tmp/pgfplots/plots_new.tex")


    // run pdflatex <filename>.tex
  }

  def writeToFile(document: String, fileName: String) {
    val path: Path = Paths.get(fileName)
    val writer: BufferedWriter = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    writer.write(document)
    writer.close()
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
      case _ => "% error: can not generate table bigger than 6 cells"
    }

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

    plots match {
      case x@List(first, second, third) => s"""
       |\\begin{center}
        |\\begin{tabular}{lll}
          |% insert cells here 1x3
          |
          |$first
          |&
          |$second
          |&
          |$third
        |\\end{tabular}
       |\\end{center}
     """.stripMargin
      case _ => ""
    }
  }

  def generate2x2Table(plots: List[String]) = {
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
    //analyse the map and convert it into the tikzpicture environment.
    println("singlePlotConfig = " + singlePlotConfig)

    val axis = singlePlotConfig.getOrElse("axis", "")

    val titleStr = singlePlotConfig.getOrElse("title", "")
    val title = if (titleStr == "") "" else s"title=$titleStr"

    val xlabelStr = singlePlotConfig.getOrElse("xlabel", "")
    val xlabel = if (xlabelStr == "") "" else s"xlabel=$xlabelStr"

    val ylabelStr = singlePlotConfig.getOrElse("ylabel", "")
    val ylabel = if (ylabelStr == "") "" else s"ylabel=$ylabelStr"

    val legendStr = singlePlotConfig.getOrElse("legend", "")
    val legend = if (legendStr == "") "" else s"legend{$legendStr}"

    val addplot = createAddplotEnv(singlePlotConfig.getOrElse("addplot", List()))

    s"""
    |\\begin{tikzpicture}[baseline]
    |     \\begin{$axis}[
    |         $title,
    |         $xlabel,
    |         $ylabel, ]
    |
    |$addplot
    |
    |         \\$legend,
    |
    |     \\end{$axis}
    |\\end{tikzpicture}
     """.stripMargin
  }


  def convertToAddplot(value: Map[String, Any]): String = {
    //Map(table -> x=dof, y=L2, data -> /Users/visenger/Documents/tmp/pgfplots/data/datafile.dat)
    //  \\addplot table[x=dof,y=Lmax] {/Users/visenger/Documents/tmp/pgfplots/data/datafile.dat};
    val table = value.getOrElse("table", "")
    val data = value.getOrElse("data", "")
    s"         \\addplot table[$table] {$data};"
  }

  def createAddplotEnv(singlePlotConfig: Any): String = {


    val plots: List[String] = singlePlotConfig match {
      case x: List[Map[String, Any]] => {
        x.map(convertToAddplot(_))

      }
      case x: Map[String, Any] => convertToAddplot(x) :: Nil
    }
    plots.mkString("\n")
  }
}
