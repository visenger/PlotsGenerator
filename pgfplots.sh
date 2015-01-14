#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.BufferedWriter

import scala.io.Source
import scala.util.parsing.json.JSON

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}


object PgfplotsGenerator {


  def main(args: Array[String]) {

    val configPath: String = "src/main/resources/pgfplots.conf"
    
    val dir = System.getProperty("user.dir")
    val texOutputPath: String = s"$dir/plots_only.tex"
    
    args.foreach(println)
    //analyse args
    val parsedConfig: Map[String, Any] = parseConfiguration(configPath)

    val config = parsedConfig.getOrElse("plots", Map())


    val plotsCollection: String = plots(generatePlots(config))

    val document: String = latexDocumentTemplae(plotsCollection)

    writeToFile(document, texOutputPath)


    // run pdflatex <filename>.tex
  }

  def writeToFile(document: String, fileName: String) {
    val path: Path = Paths.get(fileName)
    val writer: BufferedWriter = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    writer.write(document)
    writer.close()
  }

  def plots(input: List[String]): String = {
    val separator =
      s"""
         |%
         |\\hskip 10pt % insert a non-breaking space of specified width.
         |%
       """.stripMargin
    input.mkString(separator)
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
          case y: Map[_, _] => generateSinglePlot(y)
          case _ => ""
        }
        firstStr :: generatePlots(xs)
      }
      case x: Map[_, _] => generateSinglePlot(x) :: Nil
      case _ => Nil // todo: change this!
    }
    plotsStr
  }

  def generateSinglePlot(json: Map[_, Any]): String = {
    //generate the deepest element.... zoom out
    val rawPlot: Map[String, Any] = json.asInstanceOf[Map[String, Any]].getOrElse("plot", Map()).asInstanceOf[Map[String, Any]]
    // todo: use tikzpictureEnv method
    val tikzpicture: String = tikzpictureEnv(rawPlot)

    s"%GENERATED: $tikzpicture \n"

  }

  def latexDocumentTemplae(pgfPlots: String): String = {
    s"""
        |\\documentclass[a4paper, landscape]{article}
          |\\usepackage{pgfplots}
          |\\pgfplotsset{small,  compat=1.5}
          |\\begin{document}

          |\\pgfplotsset{
          |small
          |}
          |% insert plots here
          |$pgfPlots
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


  def convertToAddplot(value: Any): String = {
    //Map(table -> x=dof, y=L2, data -> /Users/visenger/Documents/tmp/pgfplots/data/datafile.dat)
    //  \\addplot table[x=dof,y=Lmax] {/Users/visenger/Documents/tmp/pgfplots/data/datafile.dat};
    val table = value.asInstanceOf[Map[String, Any]].getOrElse("table", "")
    val data = value.asInstanceOf[Map[String, Any]].getOrElse("data", "")
    s"         \\addplot table[$table] {$data};"
  }

  def createAddplotEnv(singlePlotConfig: Any): String = {
    val plots: List[String] = singlePlotConfig match {
      case x: List[_] => {
        x.map(convertToAddplot(_))
      }
      case x: Map[_, _] => convertToAddplot(x) :: Nil
      case _ => Nil
    }
    plots.mkString("\n")
  }
}

PgfplotsGenerator.main(args)
