package de.unima.dws.omatching.pipeline.util

import java.io.FileFilter
import java.io.File

class RdfFileFilter extends FileFilter {
		def accept(file:File):Boolean = {


		 return file.getName().endsWith(".rdf")
		}
}