

/**
 * Created by visenger on 07/10/15.
 */


class Util {

}

object ConfigParser {
  //args: Array[String]
  val usage =
    """
      |Usage PgfplotsGenerator -config <json file> -output <path/file.tex>
    """.stripMargin

  type OptionMap = Map[Symbol, Any]

  def parseConfig(args: Array[String]): OptionMap = {
    val config: OptionMap = parseNextOption(Map(), args.toList)
    config
  }

  private def parseNextOption(map: OptionMap, input: List[String]): OptionMap = {
    //    println(s"map before = ${map} input = ${input} ")
    input match {
      case Nil => map
      case "-config" :: configFile :: tail => parseNextOption(map ++ Map('config -> configFile), tail)
      case "-output" :: output :: tail => parseNextOption(map ++ Map('output -> output), tail)
      case undefined::tail => parseNextOption(map, tail)
      case _ => map
    }
    //    println(s"map after = ${map} input = ${input} ")


  }
}

object ConfigPlayground extends App {
  type OptionMap = Map[Symbol, Any]
  val arguments: Array[String] = Array("kuku", "-config", "json file", "-output", "path/file.tex", "bla bla")
  val config = ConfigParser.parseConfig(arguments)
  println(config)


}


class Greeting{
  var str="hello world"
}

object Tester extends App{
  val greetingToday= new Greeting()
  val anotherGreeting= greetingToday
  anotherGreeting.str="oh no..."

  println(s" greetingToday = ${greetingToday.str}")
}
