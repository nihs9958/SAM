package sal.parsing.sam.operators

import scala.collection.mutable.HashMap
import sal.parsing.sam.BaseParsing
import sal.parsing.sam.Constants
import sal.parsing.sam.Util

trait KMedians extends BaseParsing {
  val kMediansKeyWord: String = "kmedians"

  def kMediansOperator: Parser[KMediansExp] =
    kMediansKeyWord ~ "(" ~ identifier ~ "," ~ int ~ ")" ^^
      { case kmed ~ lpar ~ id ~ c1 ~ k ~ rpar =>
        KMediansExp(id, k, memory)
      }
}

case class KMediansExp(field: String, k: Int, memory: HashMap[String, String])
    extends OperatorExp(field, memory) with Util {

 override def createOpString(): String = {
  val lstreamOpt = memory.get(Constants.CurrentLStream)
  val rstreamOpt = memory.get(Constants.CurrentRStream)

  if (lstreamOpt.isEmpty || rstreamOpt.isEmpty) {
    throw new IllegalStateException("CurrentLStream or CurrentRStream not found in memory")
  }

  val lstream = lstreamOpt.get
  val rstream = rstreamOpt.get

  val tupleTypeOpt = memory.get(lstream + Constants.TupleType)

  if (tupleTypeOpt.isEmpty) {
    throw new IllegalStateException("TupleType not found in memory for LStream")
  }

  val tupleType = tupleTypeOpt.get

  // Generate SAM code for the K-medians operator
  val opString = s"""  identifier = "$lstream";
  auto $lstream = std::make_shared<KMedians<$tupleType>>($k);
  ${addRegisterStatements(lstream, rstream, memory)}"""

  opString
}

}
