package de.unima.dws.omatching.pipeline.metaMatcher

import java.util.Properties

import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess

import fr.inrialpes.exmo.align.impl.URIAlignment

/**
 *
 * Simple Transforms and outlier matrix to a matching where the outlier score is the matching score will probably never work this way just for demonstrations purposes
 * @author Alexander C. Mueller
 *
 */
class SuperNaiveOutlierMatchingAlignment(val matchings: Map[(java.net.URI, java.net.URI, String), Double]) extends URIAlignment with AlignmentProcess {

  def align(alignment: Alignment, params: Properties) = {
    matchings.foreach(A =>
      addAlignCell(A._1._1, A._1._2, A._1._3, A._2))
  }
}