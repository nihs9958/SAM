import org.scalatest.FlatSpec
import sal.parsing.sam.operators.KMedians
import sal.parsing.sam.Constants
import sal.parsing.sam.Util

class KMediansSpec extends FlatSpec with KMedians {

  "A k-medians operator" must "parse a valid input string" in {
    memory.clear()
    memory += Constants.CurrentLStream -> "stream1"
    memory += Constants.CurrentRStream -> "stream2"
    memory += "stream1" + Constants.TupleType -> "TupleType1"
    memory += "stream2" + Constants.TupleType -> "TupleType2"
    memory += "field1" + Constants.TupleType -> "TupleType1"
    memory += "field1" + Constants.NumKeys -> 1.toString
    memory += "field1" + Constants.KeyStr + 0 -> "key1"
    val input = "kmedians(field1, 5)"
    assert(parseAll(kMediansOperator, input).successful)
  }

  it must "fail to parse an invalid input string" in {
    memory.clear()
    memory += Constants.CurrentLStream -> "stream1"
    memory += Constants.CurrentRStream -> "stream2"
    memory += "stream1" + Constants.TupleType -> "TupleType1"
    memory += "stream2" + Constants.TupleType -> "TupleType2"
    memory += "field1" + Constants.TupleType -> "TupleType1"
    memory += "field1" + Constants.NumKeys -> 1.toString
    memory += "field1" + Constants.KeyStr + 0 -> "key1"
    val input = "kmedians(field1, -1)"
    assert(parseAll(kMediansOperator, input).isEmpty)
  }

  it must "generate correct C++ code" in {
    memory.clear()
    memory += Constants.CurrentLStream -> "stream1"
    memory += Constants.CurrentRStream -> "stream2"
    memory += "stream1" + Constants.TupleType -> "TupleType1"
    memory += "stream2" + Constants.TupleType -> "TupleType2"
    memory += "field1" + Constants.TupleType -> "TupleType1"
    memory += "field1" + Constants.NumKeys -> 1.toString
    memory += "field1" + Constants.KeyStr + 0 -> "key1"
    val field = "field1"
    val k = 5
    val expectedOutput = s"""identifier = "$field";
auto $field = std::make_shared<KMedians<TupleType1>>($k);
${addRegisterStatements(field, "stream2", memory)}"""
    val actualOutput = KMediansExp(field, k, memory).createOpString()
    assert(actualOutput.trim == expectedOutput.trim)
  }

  it must "generate C++ code with the correct identifier" in {
    memory.clear()
    memory += Constants.CurrentLStream -> "stream1"
    memory += Constants.CurrentRStream -> "stream2"
    memory += "stream1" + Constants.TupleType -> "TupleType1"
    memory += "stream2" + Constants.TupleType -> "TupleType2"
    memory += "field1" + Constants.TupleType -> "TupleType1"
    memory += "field1" + Constants.NumKeys -> 1.toString
    memory += "field1" + Constants.KeyStr + 0 -> "key1"
    val field = "field1"
    val k = 5
    val expectedOutput = s"""identifier = "$field";"""
    val actualOutput = KMedians
    Exp(field, k, memory).createOpString()
    assert(actualOutput.trim.startsWith(expectedOutput.trim))
  }
}
