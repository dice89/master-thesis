import de.unima.dws.oamatching.core.OntologyLoader
import de.unima.dws.oamatching.matcher.elementlevel.SimpleStringFunctionMatcher
import de.unima.dws.oamatching.matcher.structurallevel.PropertiesMatcher
import de.unima.dws.oamatching.measures.{StringMeasures, StringMeasureHelper}
import de.unima.dws.oamatching.pipeline.MatchingProblem

/**
 * Created by mueller on 25/03/15.
 */
class PropertyMatcherSpec extends UnitSpec{
  val base_matcher = new SimpleStringFunctionMatcher(true, true, true, true, StringMeasureHelper.distance_lower_cased(StringMeasures.computeEquality))
  val prop_matcher = new PropertiesMatcher()
  "The Property matcher" should "discover non string equal matchings of object and data properties with matching domains" in {
    val onto1 = OntologyLoader.load_fast_ontology("ontos/testontos/prop/onto1.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/testontos/prop/onto2.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.6)

    assert(initial_alignment.correspondences.size > 0)
    val problem = MatchingProblem(onto1,onto2,"test")

    val new_alingment = prop_matcher.align(problem,initial_alignment,0.0)


    val organizes_size = new_alingment.correspondences.filter(_.entity1.contains("organizes")).size
    assert(organizes_size > 0)
    val name_size = new_alingment.correspondences.filter(_.entity1.contains("name")).size


    println(new_alingment.correspondences)
    assert(name_size == 2)
  }

  "The Property matcher" should "discover non string equal matchings when not the direct domain is matching but one element of the domain is matching to the parent of the other element" in {
    val onto1 = OntologyLoader.load_fast_ontology("ontos/testontos/prop/onto1.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/testontos/prop/onto3.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.6)

    assert(initial_alignment.correspondences.size > 0)
    val problem = MatchingProblem(onto1,onto2,"test")

    val new_alingment = prop_matcher.align(problem,initial_alignment,0.0)


    val organizes_size = new_alingment.correspondences.filter(_.entity1.contains("organizes")).size
    assert(organizes_size > 0)
    val name_size = new_alingment.correspondences.filter(_.entity1.contains("name")).size


    println(new_alingment.correspondences)
    assert(name_size == 2)
  }
}
