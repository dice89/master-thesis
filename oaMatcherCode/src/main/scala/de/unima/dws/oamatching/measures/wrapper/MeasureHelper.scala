package de.unima.dws.oamatching.measures.wrapper

import java.net.URI

trait MeasureHelper {
	def getURIFragement(uri:URI):String = {
	  uri.getFragment()
	  //TODO check if it is working
	}
}