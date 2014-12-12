package de.unima.dws.omatching.pipeline.metaMatcher

import fr.inrialpes.exmo.align.impl.URIAlignment
import org.semanticweb.owl.align.AlignmentProcess
import de.unima.dws.omatching.matcher.MatchRelationURI

abstract class OutlierMatchingCombinationAlignment(val matchings: Map[MatchRelationURI, Double], val threshold:Double)  extends URIAlignment with AlignmentProcess  {

}