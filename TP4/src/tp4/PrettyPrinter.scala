package tp4

class PrettyPrinter {
    def stringOf(e:Expression):String={
      e match {
        case IntegerValue(i) => i.toString
        case VariableRef(v) => v
        case BinExpression(op, e1, e2) => stringOf(e1) + " " + op + " " + stringOf(e2)
      }
    }
    
    def stringOf(p:Statement):String={
       p match {
         case Seq(s1, s2) => stringOf(s1) + "" + stringOf(s2)
         case If(c, s1, s2) => "if (" + stringOf(c) + ") {\n" + stringOf(s1) + "} else {\n " + stringOf(s2) + "}\n"
         case While(c,s) => "while (" + stringOf(c) + ") do\n{\n" + stringOf(s) + "}\n"
         case Assignement(v,e) => v + ":= " + stringOf(e) + "\n"
         case Print(e) => "print(" + stringOf(e) + ")\n"
         case Read(s) => "read(" + s + ")\n"
       }
    }
}
