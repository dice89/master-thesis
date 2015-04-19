import de.unima.dws.oamatching.measures.StringMeasureHelper

/**
 * Created by mueller on 16/04/15.
 */
class StringFctSpecs extends  UnitSpec{
  "The first letter capitalizer " should "capitalize the first letter" in {

    val test = "mother"
    val result = StringMeasureHelper.upper_case_first_letter(test)
    assert("Mother".equals(result))
    assert(test.equalsIgnoreCase(result))
  }
}
