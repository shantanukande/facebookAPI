/**
 * Created by Shantanu on 12/13/2015.
 */
import org.json4s.ShortTypeHints
import org.json4s.native.Serialization
import org.json4s.native.Serialization._

import scala.collection.mutable.ArrayBuffer

trait  Amber
//to convert data into json format
object Amber {
  val ambers = List[Amber](
  )


  private implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[IndUser],classOf[postdata],classOf[officialPage])))
  def toJson(ambers: List[Amber]): String = writePretty(ambers)
  def toJson(ambers: ArrayBuffer[Amber]): String = writePretty(ambers)
  def toJson(amber: Amber): String = writePretty(amber)

}
