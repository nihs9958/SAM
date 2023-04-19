package sal.parsing.sam.operators

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.HashMap

class KMediansSpec extends AnyFlatSpec with Matchers with KMedians {
  
  "kMediansOperator" should "parse K-medians operator correctly" in {
    val input = "kmedians(myField, 3)"
    parse(kMediansOperator, input) match {
      case Success(result, _) =>
        result.field shouldBe "myField"
        result.k shouldBe 3
      case Failure(msg, _) =>
        fail(s"Parsing failed with message: $msg")
      case Error(msg, _) =>
        fail(s"Parsing failed with message: $msg")
    }
  }

  "createOpString" should "generate the correct C++ code" in {
    val memory = HashMap[String, String](
      Constants.CurrentLStream -> "lstream",
      Constants.CurrentRStream -> "rstream",
      "lstream" + Constants.TupleType -> "TupleType"
    )
    val kMediansExp = KMediansExp("myField", 3, memory)

    val expectedResult = """identifier = "myField";
    auto myField = std::make_shared<KMedians<TupleType>>(3);
    addOperator(myField);
    registerConsumer(myField, "myField");
    if (subscriber != NULL) {
      producer->registerSubscriber(subscriber, myField);
    }"""

    kMediansExp.createOpString() shouldBe expectedResult
  }
}
