import org.scalatest.FlatSpec
import sal.parsing.sam.operators.KMedians
import sal.parsing.sam.Constants

class KMediansSpec extends FlatSpec with KMedians {
  "A k-medians operator" must "parse correctly with valid input" in {
    val input = "kmedians(Stream1, 5)"
    val expectedOutput = """|identifier = "Stream1";
                            |auto Stream1 = std::make_shared<KMedians<>>(5);
                            |addOperator(Stream1);
                            |registerConsumer(Stream1, "Stream1");
                            |if (subscriber != NULL) {
                            |  producer->registerSubscriber(subscriber, Stream1);
                            |}""".stripMargin

    val parsedResult = parseAll(kMediansOperator, input)
    assert(parsedResult.successful)
    assert(parsedResult.get.createOpString() == expectedOutput)
  }

  it must "fail to parse with invalid input" in {
    val input = "kmedians(Stream1, invalid)"
    val parsedResult = parseAll(kMediansOperator, input)
    assert(parsedResult.isInstanceOf[Failure])
  }

  it must "generate correct C++ code" in {
    val input = "kmedians(Stream1, 5)"
    val expectedOutput = """  identifier = "Stream1";
    auto Stream1 = std::make_shared<KMedians<$tupleType>>(5);
    addOperator(Stream1);
    registerConsumer(Stream1, "Stream1");"""

    val parsedResult = parseAll(kMediansOperator, input)
    assert(parsedResult.successful)
    assert(parsedResult.get.createOpString() == expectedOutput)
  }

  it must "generate C++ code with the correct identifier" in {
    val input = "kmedians(MyStream, 5)"
    val expectedOutput = """|identifier = "MyStream";
                            |auto MyStream = std::make_shared<KMedians<>>(5);
                            |addOperator(MyStream);
                            |registerConsumer(MyStream, "MyStream");
                            |if (subscriber != NULL) {
                            |  producer->registerSubscriber(subscriber, MyStream);
                            |}""".stripMargin

    val parsedResult = parseAll(kMediansOperator, input)
    assert(parsedResult.successful)
    assert(parsedResult.get.createOpString() == expectedOutput)
  }
}
