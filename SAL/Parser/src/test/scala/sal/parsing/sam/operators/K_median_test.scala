import org.scalatest.FlatSpec
import sal.parsing.sam.operators.KMedians
import sal.parsing.sam.Constants
import sal.parsing.sam.Util

class KMediansSpec extends FlatSpec with KMedians with Util {

  "A k-medians operator" must "parse a valid input string" in {
    val input = "kmedians(field1, 5)"
    parseAll(kMediansOperator, input) match {
      case Success(matched, _) =>
        assert(matched.field == "field1")
        assert(matched.k == 5)
        assert(matched.memory == memory)
      case Failure(msg, _) => fail(msg)
      case Error(msg, _) => fail(msg)
    }
  }

  it must "fail to parse an invalid input string" in {
    val input = "kmedians(,)"
    assertThrows[Exception] {
      parseAll(kMediansOperator, input)
    }
  }

  it must "generate correct C++ code" in {
    val input = "kmedians(field1, 3)"
    val expectedOutput =
      s"""|identifier = "field1";
          |auto field1 = std::make_shared<KMedians<>>(3);
          |addOperator(field1);
          |registerConsumer(field1, "field1");
          |if (subscriber != NULL) {
          |  producer->registerSubscriber(subscriber, field1);
          |}""".stripMargin
    val parsedResult = parseAll(kMediansOperator, input)
    assert(parsedResult.successful)
    assert(parsedResult.get.createOpString() == expectedOutput)
  }

  it must "generate C++ code with the correct identifier" in {
    val input = "kmedians(field1, 3)"
    val expectedIdentifier = """identifier = "field1";"""

    val parsedResult = parseAll(kMediansOperator, input)
    assert(parsedResult.successful)
    assert(parsedResult.get.createOpString().contains(expectedIdentifier))
  }
}
