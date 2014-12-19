package de.unima.dws.omatching.matcher

import fr.inrialpes.exmo.align.impl.MatrixMeasure
import scala.collection.mutable.ListBuffer
import scala.collection.convert.Wrappers.JEnumerationWrapper
import fr.inrialpes.exmo.align.impl.BasicParameters
import java.util.Properties
import org.semanticweb.owl.align.Cell

class OptimizeOnlyMatcher(override val name: String, override val similarityObject: MatrixMeasure) extends PostPrunedMatcher(name, similarityObject, null) {
  protected override def align_match(threshold: Double) = {

    var properties: Properties = new BasicParameters()
    properties.setProperty("threshold", threshold + "")
    align(null, properties)
    null
  }
}