import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object FlagParser extends App {

  case class MyObj(s: scala.Option[String], i: scala.Option[Int])

  def parseArgs(s: String, flags: List[String]): Map[String, String] = {
    @tailrec
    def go(i: String, a: List[String], m: mutable.Map[String, String] = mutable.Map()): Map[String, String] = a match {
      case Nil => m.toMap
      case h :: t =>
        if(i.contains("-" + h + "=")){
          m.put(h, i.split("-" + h + "=")(1).split(" ").head)
          go(i, t, m)
        } else {
          go(i, t, m)
        }
    }


    if(s.matches("^(-\\w+=[^ =-]+ )*(-\\w+=[^ =-]+)$")) go(s, flags) else Map()

  }

  val parsed = parseArgs("-s=dadel -i=23 -de=er", List("s", "i"))

  println(MyObj(parsed.get("s"), parsed.get("i").flatMap(i => Try(i.toInt).toOption)))

}


