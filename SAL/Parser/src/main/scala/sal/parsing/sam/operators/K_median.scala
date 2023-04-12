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
    val lstream = memory.getOrElse(Constants.CurrentLStream, throw new IllegalStateException("CurrentLStream not found in memory"))
    val rstream = memory.getOrElse(Constants.CurrentRStream, throw new IllegalStateException("CurrentRStream not found in memory"))

    // Get the tuple type of the input stream
    val tupleType = memory.getOrElse(lstream + Constants.TupleType, throw new IllegalStateException("TupleType not found in memory"))

    // Get the key fields of the input stream
    val numKeys = memory.getOrElse(lstream + Constants.NumKeys, throw new IllegalStateException("NumKeys not found in memory")).toInt
    val keyFields = (0 until numKeys).map(i => memory.getOrElse(lstream + Constants.KeyStr + i, throw new IllegalStateException("KeyStr not found in memory"))).toList

    // Generate SAM code for the K-medians operator
    val opString = s"""  identifier = "$lstream";
    auto $lstream = std::make_shared<KMedians<$tupleType, ${keyFields.mkString(", ")}>>($k);
    ${addRegisterStatements(lstream, rstream, memory)}"""
  
    opString
  }
}
