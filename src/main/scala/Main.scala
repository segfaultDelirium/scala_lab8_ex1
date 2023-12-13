import scala.annotation.tailrec
def stream(o: List[Any]): String = {

  @tailrec
  def streamRec(o: List[Any], res: String): String = {
    if o.size == 0 then return res
    val head = o.head
    val tail = o.tail

    val typeName = head match {
      case _: Int    => "Int"
      case _: Double => "Double"
      case _: String => "String"
    }

    val newRes = s"$res ${typeName}($head)"
    streamRec(tail, newRes)
  }

  streamRec(o, "")
}

def unstream(s: String): List[Any] = {
  // println("in unstream")
  val splits = s.slice(0, s.length - 1).split("\\) ")
  val typenameValuePairs = splits.map(split => {
    val typeNameAndValue = split.split("\\(")
    val typename = typeNameAndValue(0).trim()
    val value = typeNameAndValue(1)
    (typename, value)
    // println(s"$typename: $value")
    (typename, value)
  })
  val res = unstreamRec(typenameValuePairs, List())
  // println("after unstream")
  res
}

object IntExtractor {
  def unapply(tuple: (String, String)): Option[Int] = {
    val res = tuple match {
      case ("Int", value) => Some(value.toInt)
      case _              => None
    };
    return res
  }
}

object DoubleExtractor {
  def unapply(tuple: (String, String)): Option[Double] = {
    val res = tuple match {
      case ("Double", value) => Some(value.toDouble)
      case _                 => None
    };
    return res
  }
}

object ObjectExtractor {
  def unapply(tuple: (String, String)): Option[Int | Double | String] = {
    val toAppend = tuple match {
      case ("Int", value)    => Some(value.toInt)
      case ("String", value) => Some(value)
      case ("Double", value) => Some(value.toDouble)
      case _                 => None
    }
    toAppend
  }
}

@tailrec
def unstreamRec(
    elements: Array[(String, String)],
    res: List[Any]
): List[Any] = {
  if elements.size == 0 then return res

  val head = elements.head
  val tail = elements.tail
  // println(s"head is `${head._1}`, `${head._2}`")
  // val unapplied = head._2.unapply()
  // Int.unapply(head._2)

  // val toAppend = IntExtractor
  //   .unapply(head)
  //   .getOrElse(
  //     DoubleExtractor
  //       .unapply(head)
  //       .getOrElse(head._2)
  //   )

  // val Some(toAppendValue) = toAppend
  // .getOrElse((t, value) => value)
  // val toAppend = head match {
  //   case ("Int", value)    => value.toInt
  //   case ("String", value) => value
  //   case ("Double", value) => value.toDouble
  // }

  val toAppendValue = ObjectExtractor.unapply(head).get
  val newRes = res.appended(toAppendValue)
  unstreamRec(tail, newRes)
}

@main def hello: Unit = {

  println("Hello world!")
  val l = List(1, "hello", 2.56, 0x45, "key")
  println(l)

  val s = stream(l)
  println(s)

  val b = unstream(s)
  println(b)
  assert(l.equals(b))
}
