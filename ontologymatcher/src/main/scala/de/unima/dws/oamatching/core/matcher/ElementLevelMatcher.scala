package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{MatchingCell, Alignment, Cell}
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
  override def align( onto1:OWLOntology,  onto2:OWLOntology,threshold:Double) :Alignment = {

    val entities1 = onto1.getSignature.toVector
    val entities2 = onto2.getSignature.toVector

    val alignment:Alignment =  new Alignment(null,null,onto1,onto2)


    println(entities1.size)
    println(entities2.size)
    var i = 0
    var n = 0
    var entity1:OWLEntity= null
    var entity2:OWLEntity= null
    while (i < entities1.size){
      n = 0
      entity1 = entities1(i)
      while(n < entities2.size){
        entity2 = entities2(n)
        if(entity1.isOWLClass && entity2.isOWLClass){
          alignment.correspondences.addAll(alignClass(entity1.asOWLClass(),onto1, entity2.asOWLClass(),onto2,threshold))
        }

        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          alignment.correspondences.addAll( alignDatatypeProperty(entity1.asOWLDataProperty(),onto1,entity2.asOWLDataProperty(),onto2,threshold))
        }

        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          alignment.correspondences.addAll(alignObjectProperty(entity1.asOWLObjectProperty(),onto1, entity2.asOWLObjectProperty(),onto2,threshold))
        }
        n = n+1
      }
      i = i+1
    }


    /*
    val it_e_1 = entities1.iterator
    while(it_e_1.hasNext){
      val it_e2 = entities2.iterator
      val entity1 = it_e_1.next()

      while(it_e2.hasNext){
          val entity2 = it_e2.next()
         if(entity1.isOWLClass && entity2.isOWLClass){
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(),onto1, entity2.asOWLClass(),onto2))

          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }
         }

        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),onto1,entity2.asOWLDataProperty(),onto2))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(),onto1, entity2.asOWLObjectProperty(),onto2))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }

      }
    }*/
/*
    entities1.foreach(entity1 => {
      entities2.foreach(entity2=> {
        //check for classes
        if(entity1.isOWLClass && entity2.isOWLClass){
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(), entity2.asOWLClass()))

          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }


        }

        //align Datatype Properties
        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),entity2.asOWLDataProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }

        //align Object Properties
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(), entity2.asOWLObjectProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }
      })

    })*/
    //start producing alignments time complexity is O(n^2)
    /*for(entity1 <- entities1.view;
        entity2 <- entities2.view){

        //check for classes
        if(entity1.isOWLClass && entity2.isOWLClass){
          val startime= System.currentTimeMillis()
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(), entity2.asOWLClass()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }
        }

        //align Datatype Properties
        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),entity2.asOWLDataProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }

        //align Object Properties
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(), entity2.asOWLObjectProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }

    }
*/
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

  def score(owlEntity1: OWLEntity, onto1: OWLOntology, owlEntity2: OWLEntity, onto2: OWLOntology, threshold: Double, owlType: String): List[MatchingCell] = {

    val entity1_fields = getLabelAndFragmentOfEntity(owlEntity1, onto1)
    val entity2_fields = getLabelAndFragmentOfEntity(owlEntity2, onto2)
    //match fields combined
    if(Config.loaded_config.getBoolean("general.names_separated")){
      matchFieldsSeparated(owlEntity1, owlEntity2, threshold, owlType, entity1_fields, entity2_fields)
    }else{
      matchFieldsCombined(owlEntity1, owlEntity2, threshold, owlType, entity1_fields, entity2_fields)
    }
  }

  def matchFieldsCombined(owlEntity1: OWLEntity, owlEntity2: OWLEntity, threshold: Double, owlType: String, entity1_fields: ExtractedFields, entity2_fields: ExtractedFields): List[MatchingCell] = {
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

  def matchFieldsSeparated(owlEntity1: OWLEntity, owlEntity2: OWLEntity, threshold: Double, owlType: String, entity1_fields: ExtractedFields, entity2_fields: ExtractedFields): List[MatchingCell] = {
    //fragments matchings
    val fragment_score = if (entity1_fields.fragment.isDefined && entity2_fields.fragment.isDefined && useFragment) {
      val value = getSimilarity(score_cached(entity1_fields.fragment.get, entity2_fields.fragment.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_FRAGMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    //label matchings
    val label_score = if (entity1_fields.label.isDefined && entity2_fields.label.isDefined && useLabel) {
      println("label matched")
      val value =getSimilarity(score_cached(entity1_fields.label.get, entity2_fields.label.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_LABEL_LABEL, value)
    } else {
      Option.empty
    }

    //fragment label matching
    val fragment_label_score = if (entity1_fields.fragment.isDefined && entity2_fields.label.isDefined && useFragment && useLabel) {
      println("fragment label matched")
      val value = getSimilarity(score_cached(entity1_fields.fragment.get, entity2_fields.label.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_FRAGMENT_LABEL, value)
    } else {
      Option.empty
    }
    //label fragment matching
    val label_fragment_score = if (entity1_fields.label.isDefined && entity2_fields.fragment.isDefined && useFragment && useLabel) {
      println("label fragment matched")
      val value = getSimilarity(score_cached(entity1_fields.label.get, entity2_fields.fragment.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_LABEL_FRAGMENT, value)
    } else {
      Option.empty
    }

    val comment_comment_score = if (entity1_fields.comment.isDefined && entity2_fields.comment.isDefined && useComment) {
      //println("comment matched")
      val value = getSimilarity(score_cached(entity1_fields.comment.get, entity2_fields.comment.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_COMMENT_COMMENT, value)
    } else {
      Option.empty
    }

    val fragment_comment_score = if (entity1_fields.fragment.isDefined && entity2_fields.comment.isDefined && useFragment && useComment) {
      println("comment matched")
      val value = getSimilarity(score_cached(entity1_fields.fragment.get, entity2_fields.comment.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_FRAGMENT_COMMENT, value)
    } else {
      Option.empty
    }

    val comment_fragment_score = if (entity1_fields.comment.isDefined && entity2_fields.fragment.isDefined && useFragment && useComment) {
      //println("comment matched")
      val value = getSimilarity(score_cached(entity1_fields.comment.get, entity2_fields.fragment.get,true))
      createMatchingCellOptional(owlEntity1, owlEntity2, threshold, owlType, Alignment.TYPE_COMMENT_FRAGMENT, value)
    } else {
      Option.empty
    }

    //get results
    List(fragment_score, label_score, fragment_label_score, label_fragment_score, comment_comment_score, fragment_comment_score, comment_fragment_score).filter(_.isDefined).map(_.get)
  }

  protected def alignClass(owlClass1: OWLClass, onto1: OWLOntology, owlClass2: OWLClass, onto2: OWLOntology, threshold: Double): List[MatchingCell] = {
    score(owlClass1, onto1, owlClass2, onto2,threshold,Cell.TYPE_CLASS)
  }

  protected def alignDatatypeProperty(owlProperty1:  OWLDataProperty, onto1: OWLOntology, owlProperty2: OWLDataProperty, onto2: OWLOntology,threshold: Double): List[MatchingCell] = {
    score(owlProperty1, onto1, owlProperty2, onto2,threshold, Cell.TYPE_DT_PROPERTY)
  }

  protected def alignObjectProperty(owlProperty1:  OWLObjectProperty, onto1: OWLOntology, owlProperty2:  OWLObjectProperty, onto2: OWLOntology,threshold: Double): List[MatchingCell] = {
    score(owlProperty1, onto1, owlProperty2, onto2,threshold, Cell.TYPE_OBJECT_PROPERTY)
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


  protected def createMatchingCellOptional(owlEntity1: OWLEntity, owlEntity2: OWLEntity, threshold: Double, owlType: String,matching_type:String, value: Double): Option[MatchingCell] = {
    if (value > threshold) {
      Option(createMatchingCell(owlEntity1, owlEntity2, value, owlType,matching_type))
    } else {
      Option.empty
    }
  }

  protected  def createMatchingCell(owlEntity1: OWLEntity, owlEntity2: OWLEntity, score: Double, owlType: String, matching_type:String): MatchingCell = {
    MatchingCell(owlEntity1.getIRI.toString, owlEntity2.getIRI.toString, score, "=", owlType,matching_type)
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


        //println("Comment: " +rdfs_comments.head.getValue.asLiteral().get().getLiteral)
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
