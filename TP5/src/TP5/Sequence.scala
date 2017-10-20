package TP5

import scala.collection.JavaConverters._
import test0.Sequence

class SequenceImp extends Sequence {

 //  A ajouter pour activer la conversion automatique de type T en 
//  HOL.equal[T]
  
implicit def equal_t[T]: HOL.equal[T] = new HOL.equal[T] {
val `HOL.equal` = (a: T, b: T) => a == b
}

  
  def subSeq(t1: Array[Object], t2: Array[Object]): Boolean = {

    // Conversion des tableaux Java en listes Scala 
    val sl1= t1.toList
    val sl2= t2.toList
    
    tp5.subSeq(sl1, sl2)
  }
}
