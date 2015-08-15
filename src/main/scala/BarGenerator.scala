import scala.io.Source

/**
 * Created by visenger on 14/08/15.
 */

case class BarEvalLine(noise: String, datasize: String,
                       pCfdMd: String, rCfdMd: String, f1CfdMd: String, timeCfdMd: String,
                       pMdCfd: String, rMdCfd: String, f1MdCfd: String, timeMdCfd: String,
                       pJoint: String, rJoint: String, f1Joint: String, timeJoint: String)

object BarGenerator {


  def main(args: Array[String]) {
    val rawEvalLines: List[String] = Source.fromFile("/Users/visenger/data/HOSP2/evaluation-exec-order/evaluation-exec-order.tsv").getLines().toList.tail

    val evalLines: List[BarEvalLine] = rawEvalLines.map(l => {
      val Array(noise, datasize, pCfdMd, rCfdMd, f1CfdMd, timeCfdMd, pMdCfd, rMdCfd, f1MdCfd, timeMdCfd, pJoint, rJoint, f1Joint, timeJoint) = l.split("\\t")
      BarEvalLine(noise, datasize, pCfdMd, rCfdMd, f1CfdMd, timeCfdMd, pMdCfd, rMdCfd, f1MdCfd, timeMdCfd, pJoint, rJoint, f1Joint, timeJoint)
    })

    val groupedBySize: Map[String, List[BarEvalLine]] = evalLines.groupBy(l => l.datasize)
    val groupedByNoise: Map[String, List[BarEvalLine]] = evalLines.groupBy(l => l.noise)

    val allBars = generateBarsBySize(groupedBySize)
    println(allBars)

  }


  def generateBarsBySize(groupedBySize: Map[String, List[BarEvalLine]]): String = {
    groupedBySize.map(g => {
      generateSingleBarTex(g)
    }).mkString
  }

  def generateSingleBarTex(tuple: (String, List[BarEvalLine])): String = {
    val dataSize: String = tuple._1
    val evalData: List[BarEvalLine] = tuple._2

    val coords: List[String] = evalData.map(e => {
      e.noise
    })
    val xCoords: String = coords.mkString(",")
    val symbXCoords = s"symbolic x coords={$xCoords}".toString

    val plotF1CfdMd: String = evalData.map(e => {
      s"(${e.noise}, ${e.f1CfdMd})"
    }).mkString(" ")
    val cfdMdPlot = s"\\addplot coordinates {$plotF1CfdMd}"

    val plotF1MdCfd: String = evalData.map(e => {
      s"(${e.noise}, ${e.f1MdCfd})"
    }).mkString(" ")
    val mdCfdPlot = s"\\addplot coordinates {$plotF1MdCfd}"

    val plotF1Joint: String = evalData.map(e => {
      s"(${e.noise}, ${e.f1Joint})"
    }).mkString(" ")
    val jointPlot = s"\\addplot coordinates {$plotF1Joint}"
    val addPlots = s"$cfdMdPlot;\n$mdCfdPlot;\n$jointPlot;\n"
    val bar = getBarTemplate(symbXCoords, addPlots)

    bar
  }

  def getBarTemplate(symbXCoords: String, addPlots: String): String = {
    s"""
|\\begin{tikzpicture} \\begin{axis}[
    |ybar,
    |enlargelimits=0.15,
    |legend style={at={(0.5,-0.15)}, anchor=north,legend columns=-1},
    |bar width=4,
    |ylabel={F1},
    |$symbXCoords,
    |xtick=data,
    |nodes near coords align={vertical},
    |]
|$addPlots
|\\legend{cfd+md,md+cfd,joint cfd md}
|\\end{axis}
|\\end{tikzpicture}""".stripMargin
  }

}


