import java.io.BufferedWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.collection.immutable.IndexedSeq
import scala.io.Source
import scala.util.parsing.json.JSON

/**
  * Created by visenger on 29/12/14.
  *
  * http://tex.stackexchange.com/questions/188147/how-to-put-legend-below-the-chart
  */
object PgfplotsGenerator {


  def main(args: Array[String]) {

    //    todo: arguments parser.
    // -config <json file name> -output <path/file.tex>

    val parseConfig = ConfigParser.parseConfig(args)

    val configPath: String = parseConfig
      .getOrElse('config, "src/main/resources/msag-p-r.conf")
      .toString

    //val configPath: String = "src/main/resources/msag-p-r.conf"
    val texOutputPath = parseConfig
      .getOrElse('output, s"${System.getProperty("user.dir")}/msag-plots.tex").toString
    //val texOutputPath: String = s"${System.getProperty("user.dir")}/msag-plots.tex"

    args.foreach(println)
    /* parse config and generate plots */
    val parsedConfig: Map[String, Any] = parseConfiguration(configPath)

    val config = parsedConfig.getOrElse("plots", Map())

    val plotsCollection: String = plots(generatePlots(config))

    val document: String = latexDocumentTemplate(plotsCollection)

    println("latex document = " + document)
    // writeToFile(document, texOutputPath)

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
    val json: String = Source.fromFile(path).getLines().mkString("\n")

    //parse json
    val parsedJson: Option[Any] = JSON.parseFull(json)
    val afterParseMap: Map[String, Any] = parsedJson match {
      case Some(x) => x.asInstanceOf[Map[String, Any]]
      case None => Map[String, Any]()
    }
    afterParseMap
  }

  @SuppressWarnings(Array("all"))
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

    val tikzpicture: String = tikzpictureEnv(rawPlot)

    s"%GENERATED: $tikzpicture \n"

  }

  def latexDocumentTemplate(pgfPlots: String): String = {
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


    val addPlotsConfig = singlePlotConfig.getOrElse("addplot", List())
    val addplot = createAddplotEnv(addPlotsConfig)

    val legend = generateLegendString(addPlotsConfig, singlePlotConfig.getOrElse("legend", ""))

    s"""
       |\\begin{tikzpicture}
       |%\\selectcolormodel{gray}
       |\\begin{$axis}[
       |legend pos=outer north east,
       | $title,
       | $xlabel,
       | $ylabel, ]
       |
                                                       |$addplot
       |
        |\\$legend,
       |
                    |\\end{$axis}
       |\\end{tikzpicture}
     """.stripMargin
  }


  private def generateLegendString(addPlotsConfig: Any, legendStr: Any): String = {
    if (legendStr == "") {
      //todo:create default legend from the size of plots
      val legendString: String = addPlotsConfig match {
        case x: Traversable[_] => {
          val defaultLegends: IndexedSeq[String] = for (i <- 1 until x.size) yield s"plot $i"
          defaultLegends.mkString(",")
        }
        case _ => ""
      }
      s"legend{$legendString}"
    } else s"legend{$legendStr}"
  }

  def convertToAddplot(value: Any): String = {
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

// create config for plots generation.
object PlotsDataSizeConfigurationGenerator extends App {

  //NOISE	CFDF1	MDF1	CFDMDF1	TIME

  val dataSetSizes = Array(500, 1000, 10000, 20000, 30000, 40000, 50000, 70000, 90000, 100000)

  val plotsForDataSize: Array[String] = for (i <- dataSetSizes) yield {
    val plotConfig =
      s"""{
      "plot": {
        "axis":"axis",
        "title": "TPC-H Data Cleaning for ${i}k",
        "xlabel": "noise",
        "ylabel": "F1",
        "addplot": [
          {
            "table": "x=NOISE, y=CFDF1",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-$i-datasize.tsv"
          },
          {
            "table": "x=NOISE, y=MDF1",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-$i-datasize.tsv"
          },
          {
            "table": "x=NOISE, y=CFDMDF1",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-$i-datasize.tsv"
          }
        ],
        "legend": "$$cfd$$,$$md$$,$$cfd+md$$"
      }
    }"""
    plotConfig
  }
  val start =
    s"""{
  "plots": [
    """
  val sep =
    s""",
       |
     """.stripMargin
  val end =
    s"""]
       |}
     """.stripMargin
  val plotsConfig: String = plotsForDataSize.mkString(start, sep, end)


  print(plotsConfig)
}

object PlotModelAccuracyGenerator extends App {


  def singlePlotConfig(data: String) =
    s"""{
      "plot": {
        "axis":"axis",
        "title": "Model accuracy for the different sizes of trainings data on ${data}",
        "xlabel": "Fraction of data for training model",
        "ylabel": "Accuracy",
        "addplot": [
          {
            "table": "x=DATASIZE, y=B-$data",
            "data": "/Users/visenger/research/datasets/EXPERIMENTS/strategies/23-01-2018/training-accuracy.tsv"
          },
          {
            "table": "x=DATASIZE, y=S-$data",
            "data": "/Users/visenger/research/datasets/EXPERIMENTS/strategies/23-01-2018/training-accuracy.tsv"
          }
        ],
        "legend": "$$bagging$$,$$stacking$$"
      }
    }"""

  val datasets: Seq[String] = Seq("blackoak", "hosp", "salaries", "flights")

  val plots: Seq[String] = datasets.map(dataset => singlePlotConfig(dataset.toUpperCase()))

  val start =
    s"""{
  "plots": [
      """.stripMargin
  val sep =
    s""",
       |
     """.stripMargin
  val end =
    s"""]
       |}
     """.stripMargin

  val collectedPlots: String = plots.mkString(start, sep, end)


  println(collectedPlots)


}

object PlotsNoiseConfigGeneration extends App {

  //DATASIZE	CFDF1	MDF1	CFDMDF1	TIME

  val noise = Array(2, 4, 6, 8, 10)

  val plotsForNoise: Array[String] = for (i <- noise) yield {
    val plotConfig =
      s"""{
      "plot": {
        "axis":"axis",
        "title": "TPC-H Data Cleaning for noise ${i}\\%",
        "xlabel": "data size in k",
        "ylabel": "F1",
        "addplot": [
          {
            "table": "x=DATASIZE, y=CFDF1",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-$i-noise.tsv"
          },
          {
            "table": "x=DATASIZE, y=MDF1",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-$i-noise.tsv"
          },
          {
            "table": "x=DATASIZE, y=CFDMDF1",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-$i-noise.tsv"
          }
        ],
        "legend": "$$cfd$$,$$md$$,$$cfd+md$$"
      }
    }"""
    plotConfig
  }

  val plotConfig =
    s"""{
      "plot": {
        "axis":"axis",
        "title": "runtime for HOSP Data Cleaning",
        "xlabel": "data size in k",
        "ylabel": "seconds",
        "addplot": [
          {
            "table": "x=DATASIZE, y=TIME",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-2-noise.tsv"
          },
          {
            "table": "x=DATASIZE, y=TIME",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-4-noise.tsv"
          },
          {
            "table": "x=DATASIZE, y=TIME",
            "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-6-noise.tsv"
          },
          {
          "table": "x=DATASIZE, y=TIME",
          "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-8-noise.tsv"
          },
          {
          "table": "x=DATASIZE, y=TIME",
          "data": "/Users/visenger/data/TPC-H/tpch_2_17_0/tables/evaluation/evaluation-10-noise.tsv"
          }
        ],
        "legend": "$$noise 2$$,$$noise 4$$,$$noise 6$$,$$noise 8$$,$$noise 10$$"
      }
    },
    """

  val start =
    s"""{
  "plots": [
    $plotConfig"""
  val sep =
    s""",
       |
     """.stripMargin
  val end =
    s"""]
       |}
     """.stripMargin


  val plotsConfig: String = plotsForNoise.mkString(start, sep, end)


  print(plotsConfig)

}

