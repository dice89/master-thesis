package de.unima.dws.oamatching.measures

/**
 *
 * Object that contains all implemented basic string metrics
 * @author Alexander Mueller
 *
 */

import de.unima.dws.oamatching.measures.wrapper._
import fr.inrialpes.exmo.ontosim.string.StringDistances
/**
 *  Util classes for string matching
 * 
 * @author Alexander C. Mueller
 *
 */
object StringMeasureHelper {

  /**
   *  Curryable Function to match normalized Strings
   * @param normalization A Normalization Function e.g. to lower case
   * @param matching A Matching function that takes 2 Strings as an Input and then returns a Double value for it
   * @param a
   * @param b
   * @return A double valued string distance
   */
  def distance_normalized(normalization: (String, String) => (String, String))(matching: (String, String) => Double)(a: String, b: String): Double = {
    matching.tupled(normalization(a, b))
  }

  def to_lower_case(a: String, b: String): (String, String) = {
    //println("Lower Case")
    (a.toLowerCase(), b.toLowerCase())
  }

  def distance_lower_cased = distance_normalized(to_lower_case) _

}