import java.util.regex.Pattern


object IndexFinder extends App {

  def getStartIndex(input: String)(marker: String, occ: Int, offset: Int): Int = Pattern.quote(marker)
    .r.findAllMatchIn(input).map(_.end).toList.lift(occ - 1).getOrElse(1) - 1 + offset

  def getIndexLength(index: Int, length: Int): Int = if (index > length) length else index

  def getIndices(marker: String, occ: Int, offset: Int, length: Int)(input: String): (Int, Int) = {
    val startIndex = getStartIndex(input)(marker, occ, offset)
    val endIndex = startIndex + length
    (getIndexLength(startIndex, input.length), getIndexLength(endIndex, input.length))
  }

  def getIndices(sMarker: String, sOcc: Int, sOffset: Int, eMarker: String, eOcc: Int, eOffset: Int)(input: String): (Int, Int) = {
    val startIndex = getStartIndex(input)(sMarker, sOcc, sOffset)
    val endIndex = Pattern.quote(eMarker).r.findAllMatchIn(input).map(_.start).toList.lift(eOcc - 1).getOrElse(0) + eOffset
    (getIndexLength(startIndex, input.length), getIndexLength(endIndex, input.length))
  }


  val (s, e) = getIndices("-", 2, 0, 200)("hello-hello-hello")
  println("hello-hello-hello".substring(s,e))
  println(s)
  println(e)

  val (s2, e2) = getIndices("P", 2, 1, "Q", 1, 1)("hello-hello-hello*ee")
  println(s2)
  println(e2)

}
