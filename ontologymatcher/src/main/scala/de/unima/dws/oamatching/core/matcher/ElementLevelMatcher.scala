package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{FastOntology, MatchingCell, Alignment, Cell}
import org.semanticweb.owlapi.model._

import scala.collection.JavaConversions._
import scala.collection.mutable


case class ExtractedFields(val fragment:Option[String], val label: Option[String], val comment: Option[String], val synonym:Option[List[String]])

//case class MatchingResults(val fragment_fragment:Option[MatchingCell], val label_label: Option[MatchingCell],val label_fragment: Option[MatchingCell],val fragment_label: Option[MatchingCell], val comment_comment: Option[MatchingCell])
/**
 * Created by mueller on 21/01/15.
 */
abstract class ElementLevelMatcher(val similarity: Boolean, val useLabel: Boolean, val useFragment: Boolean, val useComment: Boolean) extends Matcher {

  val cache = mutable.HashMap[String,Double]()
  /**
   * Implements element-wise ontology matcher
   * @param onto1
   * @param onto2
   * @param threshold
   * @return
   */
  override def align( onto1:FastOntology,  onto2:FastOntology,threshold:Double) :Alignment = {
    var i = 0
    var n = 0
    val alignment:Alignment =  new Alignment(null,null,onto1,onto2,null,null)

    var entity1:IRI= null
    var entity2:IRI= null

    //match classes
    while(i < onto1.base_values.classes.size){
      n = 0
      entity1 = onto1.base_values.classes.get(i)
      while(n < onto2.base_values.classes.size){
        entity2= onto2.base_values.classes.get(n)
        alignment.correspondences.addAll(alignClass(entity1,entity2, onto1.classes_to_names.get(entity1).get, onto2.classes_to_names.get(entity2).get, threshold))
        n = n +1
      }
      i=i+1
    }

    //match data props
    while(i < onto1.base_values.data_properties.size){
      n = 0
      entity1 = onto1.base_values.data_properties.get(i)
      while(n < onto2.base_values.data_properties.size){
        entity2= onto2.base_values.data_properties.get(n)
        alignment.correspondences.addAll(alignDatatypeProperty(entity1,entity2, onto1.data_properties_to_names.get(entity1).get, onto2.data_properties_to_names.get(entity2).get, threshold))
        n = n +1
      }
      i=i+1
    }

    //match object props
    while(i < onto1.base_values.object_properties.size){
      n = 0
      entity1 = onto1.base_values.object_properties.get(i)
      while(n < onto2.base_values.object_properties.size){
        entity2= onto2.base_values.object_properties.get(n)
        alignment.correspondences.addAll(alignObjectProperty(entity1,entity2, onto1.object_properties_to_names.get(entity1).get, onto2.object_properties_to_names.get(entity2).get, threshold))
        n = n +1
      }
      i=i+1
    }

    System.gc()
    //return alignment
    alignment
  }


  def score(entity1: String, entity2: String): Double

  def score_cached(entity1: String, entity2: String, cached:Boolean): Double = {

    if(cached){
      val cached_score = cache.get(entity1+"##"+entity2)
      if(cached_score.isDefined){
        //println("from cache")
        cached_score.get
      }else {
        val score:Double = this.score(entity1,entity2)
        cache.put(entity1+"##"+entity2, score)
        score
      }

    } else {
      score(entity1,entity2)
    }
  }

  def score(entity1: IRI, entity2: IRI,entity1_fields:ExtractedFields, entity2_fields:ExtractedFields, threshold: Double, owlType: String): List[MatchingCell] = {


    //match fields combined
    if(Config.loaded_config.getBoolean("general.names_separated")){
      matchFieldsSeparated(entity1, entity2, threshold, owlType, entity1_fields, entity2_fields)
    }else{
      matchFieldsCombined(entity1, entity2,  threshold, owlType, entity1_fields, entity2_fields)
    }
  }

  def matchFieldsCombined(owlEntity1: IRI, owlEntity2: IRI, threshold: Double, owlType: String, entity1_fields: ExtractedFields, entity2_fields: ExtractedFields): List[MatchingCell] = {
    val entity1_combined =(entity1_fields.comment.getOrElse("") + " " + entity1_fields.fragment.getOrElse("") + " " + entity1_fields.label.getOrElse("")).replaceAll("  ", " ").trim
    val entity2_combined = (entity2_fields.comment.getOrElse("") + " " + entity2_fields.fragment.getOrElse("") + " " + entity2_fields.label.getOrElse("")).replaceAll("  ", " ").trim

    val value: Double = try {
        score(entity1_combined, entity2_combined)
      }catch {
        case e:Throwable =>{
          e.printStackTrace()
          0.0
      }}

      val cell = createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_NONE, value)
      List(cell).filter(_.isDefined).map(_.get)

  }

  def matchFieldsSeparated(entity1: IRI, entity2: IRI, threshold: Double, owlType: String, entity1_fields: ExtractedFields, entity2_fields: ExtractedFields): List[MatchingCell] = {
    //fragments matchings
    val fragment_score = if (entity1_fields.fragment.isDefined && entity2_fields.fragment.isDefined && useFragment) {
      val value = getSimilarity(score_cached(entity1_fields.fragment.get, entity2_fields.fragment.get,true))


      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    //label matchings
    val label_score = if (entity1_fields.label.isDefined && entity2_fields.label.isDefined && useLabel) {
      println("label matched")
      val value =getSimilarity(score_cached(entity1_fields.label.get, entity2_fields.label.get,true))
      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    //fragment label matching
    val fragment_label_score = if (entity1_fields.fragment.isDefined && entity2_fields.label.isDefined && useFragment && useLabel) {
      println("fragment label matched")
      val value = getSimilarity(score_cached(entity1_fields.fragment.get, entity2_fields.label.get,true))
      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }
    //label fragment matching
    val label_fragment_score = if (entity1_fields.label.isDefined && entity2_fields.fragment.isDefined && useFragment && useLabel) {
      println("label fragment matched")
      val value = getSimilarity(score_cached(entity1_fields.label.get, entity2_fields.fragment.get,true))
      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    val comment_comment_score = if (entity1_fields.comment.isDefined && entity2_fields.comment.isDefined && useComment) {
      //println("comment matched")
      val value = getSimilarity(score_cached(entity1_fields.comment.get, entity2_fields.comment.get,true))
      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    val fragment_comment_score = if (entity1_fields.fragment.isDefined && entity2_fields.comment.isDefined && useFragment && useComment) {
      println("comment matched")
      val value = getSimilarity(score_cached(entity1_fields.fragment.get, entity2_fields.comment.get,true))
      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    val comment_fragment_score = if (entity1_fields.comment.isDefined && entity2_fields.fragment.isDefined && useFragment && useComment) {
      //println("comment matched")
      val value = getSimilarity(score_cached(entity1_fields.comment.get, entity2_fields.fragment.get,true))
      createMatchingCellOptional(entity1, entity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    //get results
    List(fragment_score, label_score, fragment_label_score, label_fragment_score, comment_comment_score, fragment_comment_score, comment_fragment_score).filter(_.isDefined).map(_.get)
  }

  protected def alignClass(owlClass1: IRI,  owlClass2: IRI, entity1_fields: ExtractedFields, entity2_fields: ExtractedFields,  threshold: Double): List[MatchingCell] = {
    score(owlClass1, owlClass2,entity1_fields,entity2_fields,threshold,Cell.TYPE_CLASS)
  }

  protected def alignDatatypeProperty(owlProperty1:  IRI, owlProperty2: IRI,entity1_fields: ExtractedFields, entity2_fields: ExtractedFields, threshold: Double): List[MatchingCell] = {
    score(owlProperty1,owlProperty2,entity1_fields,entity2_fields,threshold, Cell.TYPE_DT_PROPERTY)
  }

  protected def alignObjectProperty(owlProperty1:  IRI, owlProperty2: IRI,entity1_fields: ExtractedFields, entity2_fields: ExtractedFields, threshold: Double): List[MatchingCell] = {
    score(owlProperty1,owlProperty2,entity1_fields,entity2_fields,threshold, Cell.TYPE_OBJECT_PROPERTY)
  }


  /**
   *Method to get a similarity value even when distance
   * @param value
   * @return
   */
  protected def getSimilarity(value:Double): Double = {
    if(similarity){
      value
    }else {
      1-value
    }
  }


  protected def createMatchingCellOptional(owlEntity1: IRI, owlEntity2: IRI, threshold: Double, owlType: String,matching_type:String, value: Double): Option[MatchingCell] = {
    if (value > threshold) {
      Option(createMatchingCell(owlEntity1, owlEntity2, value, owlType,matching_type))
    } else {
      Option.empty
    }
  }

  protected  def createMatchingCell(owlEntity1: IRI, owlEntity2: IRI, score: Double, owlType: String, matching_type:String): MatchingCell = {
    MatchingCell(owlEntity1.toString, owlEntity2.toString, score, "=", owlType,matching_type)
  }


  def getLabelAndFragmentOfEntity(oWLEntity: OWLEntity, ontology: OWLOntology):ExtractedFields = {

    val fragment = if (useFragment){
      Option(oWLEntity.getIRI.getFragment)
    }else {
      Option.empty
    }
    //get rdfs label
    val label: Option[String] = if (useLabel){
      val rdfs_labels = ontology.getAnnotationAssertionAxioms(oWLEntity.getIRI).filter(_.getProperty().isLabel)

      if(rdfs_labels.size > 0){
        Option(rdfs_labels.head.getValue().toString)
      }else {
        Option.empty
      }

    } else {
      Option.empty
    }

    val comment: Option[String] = if (useComment){
      val rdfs_comments = ontology.getAnnotationAssertionAxioms(oWLEntity.getIRI).filter(_.getProperty().isComment)

      if(rdfs_comments.size > 0){
        Option( rdfs_comments.head.getValue().toString)
      }else {
        Option.empty
      }

    } else {
      Option.empty
    }

    ExtractedFields(fragment,label,comment,Option.empty)
  }
}