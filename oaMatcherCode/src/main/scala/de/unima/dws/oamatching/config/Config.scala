package de.unima.dws.oamatching.config

import java.io.File

object Config {
	// Word Net Parameters 
	val WNBASE_DIR:String 	= "/Users/mueller/WordNet/WordNet21"
	val WNVER:String 		=	"2.1"   
	val WNDIR:String		=	WNBASE_DIR + File.separatorChar + WNVER + File.separatorChar + "dict";
	val WNIC:String			=	WNBASE_DIR + File.separatorChar + "ic-bnc-resnik-add1_2.1.dat";
	val WNTMP:String		=	WNBASE_DIR + File.separatorChar + "WNTemplate.xml";
	val WNPROP:String		=	WNBASE_DIR + File.separatorChar + "file_properties.xml";
	
	
	//Val Handling of YAM++
	val UN_KNOWN:Float		=	-Float.MaxValue 
	val NOT_IS_A:Int		=	Int.MaxValue 
	
	//how many synsets should be taken
	val SENSE_DEPTH:Int		=	10;
	
	//Splitter 
	
	val MIN_LEN:Int = 3
	
}