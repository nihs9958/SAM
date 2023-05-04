import org.scalatest.FlatSpec
import sal.parsing.sam.operators.KMedians
import sal.parsing.sam.Constants
import sal.parsing.sam.Util

class KMediansSpec extends FlatSpec with KMedians with Util {

  "A k-medians operator" must "parse a valid input string" in {
    val input = "kmedians(field1, 5)"
    parseAll(kMediansOperator, input) match {
      case Success(matched,_) =>
        assert(matched.field == "field1")
        assert(matched.k == 5)
        assert(matched.memory == memory)
      case Failure(msg,_) => fail(msg)
      case Error(msg,_) => fail(msg)
    }
  }

  it must "fail to parse an invalid input string" in {
    val input = "kmedians(,)"
    assertThrows[Exception] {
      parseAll(kMediansOperator, input)
    }
  }

  it must "generate correct C++ code" in {
    memory.clear()
    memory += Constants.CurrentLStream -> "stream1"
    memory += Constants.CurrentRStream -> "stream2"
    memory += "stream1" + Constants.TupleType -> "TupleType1"
    val k = 3
    val field = "field1"
    val expectedOutput =
      s"""identifier = "$field";
auto $field = std::make_shared<KMedians<TupleType1>>($k);
${addRegisterStatements(field, "stream2", memory)}"""
    val actualOutput = KMediansExp(field, k, memory).createOpString()
    assert(actualOutput.trim.startsWith(expectedOutput.trim))
  }

  it must "generate C++ code with the correct identifier" in {
    memory.clear()
    memory += Constants.CurrentLStream -> "stream1"
    memory += Constants.CurrentRStream -> "stream2"
    memory += "stream1" + Constants.TupleType -> "TupleType1"
    val k = 3
    val field = "field1"
    val actualOutput = KMediansExp(field, k, memory).createOpString()
    assert(actualOutput.contains(s"""identifier = "$field";"""))
  }
}
