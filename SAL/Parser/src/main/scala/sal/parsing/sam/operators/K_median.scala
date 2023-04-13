package sal.parsing.sam.operators

import com.typesafe.scalalogging.LazyLogging
import sal.parsing.sam.Constants
import sal.parsing.sam.Util
import scala.collection.mutable.HashMap

trait KMedians extends BaseParsing with LazyLogging {
  def kMediansOperator : Parser[KMediansExp] =
    kMediansKeyWord ~ "(" ~ identifier ~ "," ~ posInt ~ ")" ^^
      { case km ~ lpar ~ id ~ c ~ k ~ rpar =>
        KMediansExp(id, k, memory)
      }
}

case class KMediansExp(identifier: String, k: Int, memory: HashMap[String, String])
  extends OperatorExp(identifier, memory) with Util with LazyLogging {
  override def createOpString(): String = {
    logger.info("KMediansExp.createOpString")
    val lstream = memory(Constants.CurrentLStream)
    val rstream = memory(Constants.CurrentRStream)
    val tupleType = memory(Constants.TupleType)

    val numKeys = memory(lstream + Constants.NumKeys).toInt
    var keysString = ""
    for (i <- 0 until numKeys) {
      keysString = keysString +
        memory(lstream + Constants.KeyStr + i) + ", "
    }
    keysString = keysString.dropRight(2)

    var rString = "  identifier = \"" + identifier + "\";\n"
    rString += "  auto " + identifier +
      " = std::make_shared<KMedians<" + tupleType +
      ", " + k.toString + ", " + keysString + ">>(" +
      k.toString + ");\n"
    rString += addRegisterStatements(lstream, rstream, memory, identifier)
    rString
  }
}
