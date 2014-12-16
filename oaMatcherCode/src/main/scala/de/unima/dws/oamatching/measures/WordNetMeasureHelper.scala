package de.unima.dws.oamatching.measures

import edu.cmu.lti.lexical_db.NictWordNet
import edu.cmu.lti.lexical_db.ILexicalDatabase
import edu.cmu.lti.ws4j.impl.Lin
import edu.cmu.lti.ws4j.impl.JiangConrath
import edu.cmu.lti.ws4j.util.WS4JConfiguration
import edu.cmu.lti.ws4j.impl.WuPalmer
import edu.cmu.lti.ws4j.impl.Path

object WordNetMeasureHelper {
  
	WS4JConfiguration.getInstance().setMFS(true)
	val db:ILexicalDatabase  = new NictWordNet();
	
	val lin:Lin = new Lin(db)
	val jianConrath = new JiangConrath(db);
	val wuPalmer = new WuPalmer(db);
	val path = new Path(db);
	

}