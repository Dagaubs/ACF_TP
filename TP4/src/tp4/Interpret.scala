package tp4

class Interpret {
  
  var valueTable:Map[String,Int] = Map()
  
  
  
  def eval(p:Statement, inList:List[Int], outList:List[Int]):(List[Int],List[Int])={
    p match {
         case Seq(s1, s2) => val res1 =  eval(s1, inList, outList) ; eval(s2, res1._1, res1._2)
         case If(c, s1, s2) => if(eval(c) == 1) eval(s1, inList, outList) else eval(s2, inList, outList)
         case While(c,s) => if(eval(c) == 1) { val res= eval(s, inList, outList) ; eval(p, res._1, res._2 ) } else (inList, outList)
         case Assignement(v,e) => if(valueTable.contains(v)) { valueTable = valueTable.updated(v, eval(e)) ; (inList, outList) }
                                   else { valueTable = valueTable+(v -> eval(e)) ; (inList, outList) }
         case Print(e) => (inList, outList:+eval(e))
         case Read(s) => if(valueTable.contains(s)){ valueTable = valueTable.updated(s, inList(0)) ; (inList.tail, outList)}
                                   else { valueTable = valueTable+(s -> inList(0)) ; (inList.tail, outList) }
       }
  }
  
  def eval(e:Expression):Int={
     e match {
        case IntegerValue(i) => i
        case VariableRef(v) => if(valueTable.contains(v)) valueTable(v) else 0
        case BinExpression(op, e1, e2) => op match{
          case "+" => eval(e1) + eval(e2)
          case "*" => eval(e1) * eval(e2)
          case "-" => eval(e1) - eval(e2)
          case "<" => if(eval(e1) < eval(e2)) 1 else 0
          case ">" => if(eval(e1) > eval(e2)) 1 else 0
          case "<=" => if(eval(e1) <= eval(e2)) 1 else 0
          case ">=" => if(eval(e1) >= eval(e2)) 1 else 0
          case "==" => if(eval(e1) == eval(e2)) 1 else 0
          case "&&" => if(eval(e1) == 1 && eval(e2) == 1) 1 else 0
          case "||" => if(eval(e1) == 1 || eval(e2) == 1) 1 else 0
        }
    }
  }
}