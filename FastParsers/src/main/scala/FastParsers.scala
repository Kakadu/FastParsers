/**
 * Created with IntelliJ IDEA.
 * User: Eric
 * Date: 12.02.14
 * Time: 15:56
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.mutable._
import scala.language.experimental.macros
import scala.reflect.api.Universe
import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
//import scala.util.parsing.input._

object FastParsers {


  trait Parser[T]{
    @compileTimeOnly("can’t be used outside FastParser")
    def ~[U](parser2: Parser[U]):Parser[T] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def ~>[U](parser2: Parser[U]):Parser[U] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def <~[U](parser2: Parser[U]):Parser[T] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def |(parser2: Parser[T]):Parser[T] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def ||(parser2: Parser[T]):Parser[T] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def `?`:Parser[List[T]] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def `+`:Parser[List[T]] =  ???
    @compileTimeOnly("can’t be used outside FastParser")
    def `*`:Parser[List[T]] =  ???

    @compileTimeOnly("can’t be used outside FastParser")
    def ^^[U](f:Any => U):Parser[U] = ???
    @compileTimeOnly("can’t be used outside FastParser")
    def map[U](f:Any => U):Parser[U] = ???
    @compileTimeOnly("can’t be used outside FastParser")
    def ^^^[U](f:U):Parser[U] = ???

    @compileTimeOnly("can’t be used outside FastParser")
    def filter[U](f:Any => Boolean):Parser[T] = ???        //(==>)   ???


    @compileTimeOnly("can’t be used outside FastParser")
    def rep(min:Int,max:Int):Parser[T] = ???

    @compileTimeOnly("can’t be used outside FastParser")
    def repfold(min:Int,max:Int):Parser[T] = ???

    @compileTimeOnly("can’t be used outside FastParser")
    def repFold[U](init:U)(f:(U,T) => U):Parser[U] = ???
  }

  @compileTimeOnly("can’t be used outside FastParser")
  def rep[T](p:Parser[T],min:Int = 0,max:Int = -1):Parser[List[T]] = ???
  @compileTimeOnly("can’t be used outside FastParser")
  def rep1[T](p:Parser[T]):Parser[T] = ???
  @compileTimeOnly("can’t be used outside FastParser")
  def opt[T](p:Parser[T]):Parser[T] = ???


  @compileTimeOnly("can’t be used outside FastParser")
  def repFold[T,U](p:Parser[T])(init:U)(f:(U,T)=> U):Parser[U] = ???


  def seq[T](elem:Parser[T]*):Parser[T] = ???
  def alt[T](elem:Parser[T]*):Parser[T] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def range[T](a:T,b:T):Parser[T] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def phrase[T](a:Parser[T]):Parser[T] = ???

  @compileTimeOnly("can’t be used outside FastParser")
  def not[T](a:Parser[T]):Parser[T] = ???
  //unary_! TODO

  @compileTimeOnly("can’t be used outside FastParser")
  def guard[T](a:Parser[T]):Parser[T] = ???
  //& operator TODO

  implicit def toElem[T](elem:T):Elem[T] = Elem(elem)
  case class Elem[T](elem:T) extends Parser[T]


  /**
   *
   * @param success If the result is a success
   * @param msg The error message if any (only relevant if success == false)
   * @param result The result generated by the parser
   * @param inputPos The position in the input at which the parser has finished to read
   * @tparam T Type of the result
   */
  case class ParseResult[+T](success:Boolean,msg:String,result:T, inputPos:Int)

  object Success {
    def unapply[T](p:ParseResult[T]):Option[T] =
      if (p.success) Some(p.result)
      else  None
  }

  object Failure {
    def unapply[T](p:ParseResult[T]):Option[String] =
      if (!p.success) Some(p.msg)
      else  None
  }

  def FastParser(rules: => Unit):Any = macro FastParser_impl
  def FastParser_impl(c: Context)(rules: c.Tree)= {
    import c.universe._

    trait Input {
      def currentInput:c.Tree
      def advance:c.Tree
      def advanceTo(offset:c.Tree):c.Tree
      def mark(code:c.Tree => c.Tree):c.Tree
      def isEOI:c.Tree
      def pos:c.Tree
      def offset:c.Tree
      def inputType:c.Tree
    }

    object ReaderInput extends Input {
      def currentInput = q"input.first"
      def advance = q"input = input.rest"
      def advanceTo(offset:c.Tree) = q"input = input.drop($offset)"

      def mark(code:c.Tree => c.Tree):c.Tree = {
        val input_tmp = TermName(c.freshName)
        q"""
          val $input_tmp = input
          ${code(q"input = $input_tmp")}
      """
      }
      def isEOI = q"input.atEnd"
      def pos = q"input.pos"
      def offset = q"input.offset"
      def inputType = tq"Reader[Char]"
    }

    object StreamMarkedInput extends Input {
      def currentInput = q"input.get"
      def advance = q"input.next"
      def advanceTo(offset:c.Tree) = q""
      def mark(code:c.Tree => c.Tree) = {
        val input_tmp = TermName(c.freshName)
        q"""
          val $input_tmp = input.mark
          ${code(q"input.rollBack($input_tmp)")}
      """
      }
      def isEOI = q"input.atEnd"
      def pos = q"input.offset"
      def offset = q"input.offset"
      def inputType = tq"StreamMarked[Char]"
    }

    val input = StreamMarkedInput

    /**
     * Define an individual result in a parser
     */
    type Result = (TermName,c.Tree,Boolean)

    /**
     * Get the "zero" value of a certain type
     * @param typ
     * @return
     */
    def zeroValue(typ:c.Tree):c.Tree = typ match {
      case Ident(TypeName(name)) => name match {
        case "Char" => q"' '"
        case "Int" => q"0"
        case "String" => q""
        case _ => q"null"
      }
      case AppliedTypeTree(Ident(TypeName("List")),_) => q"Nil"
      case _ => q"null"
    }

    /**
     * Generate the boilerplate code and the transformed code of the rule
     * @param rule The code to be transformed
     * @return The transformed code
     */
    def parseRule(rule:c.Tree):c.Tree = {
      val results = new ListBuffer[Result]()
      val transform = parseRuleContent(rule,results)
      val initResults = results.map(x => q"var ${x._1}:${x._2} = ${zeroValue(x._2)}")
      val tupledResults = combineResults(results)  //lol ?
      val result = q"""ParseResult(success,msg,if (success) $tupledResults else null,${input.offset})"""

      val tree = q"""
        ..$initResults
        $transform
        $result
      """
      tree
      //q"""println(show(reify($tree)))"""
    }

    /**
     * Combine a list of results into either a tuple of result or into the same result
     * @param results
     * @return
     */
      def combineResults(results:ListBuffer[Result]):c.Tree = {
      val usedResults = results.toList.filter(_._3)
      if (usedResults.size > 1)
          q"(..${usedResults.map(x => q"${x._1}")})"
      else if (usedResults.size == 1)
          q"${usedResults(0)._1}"
      else
          q""
    }




    def parseRep(a:c.Tree,min:c.Tree,max:c.Tree,results:ListBuffer[Result]):c.Tree = {
      val counter =  TermName(c.freshName)
      val cont = TermName(c.freshName)
      var results_tmp = new ListBuffer[Result]()
      val result = TermName(c.freshName)
      val tmp_result = TermName(c.freshName)
      val tree = input.mark {rollback =>
        q"""
          var $counter = 0
          var $cont = true
          val $tmp_result = new ListBuffer[Any]()
          success = $min == 0
          while($cont && !${input.isEOI}){
            ${parseRuleContent(a,results_tmp)}
            if (success) {
                $tmp_result.append(${combineResults(results_tmp)})
                if ($counter + 1 == $max)
                  $cont = false
            }
            else {
                success = $counter >= $min
                $cont = false
                if (!success)
                  msg = "expected at least " + $min + " occurence(s) of 'rule' in rep('rule') at " + ${input.pos}
            }
            $counter = $counter + 1
          }
          if (!success) {
            ${rollback}
          }
          else {
             $result = $tmp_result.toList
          }
        """
      }
      results_tmp = results_tmp.map(x => (x._1,x._2,false))
      results.append((result,AppliedTypeTree(Ident(TypeName("List")),Ident(TypeName("Any"))::Nil),true))
      results.appendAll(results_tmp)
      tree
    }

    def parseRepFold(a:c.Tree, init:c.Tree, f:c.Tree,results:ListBuffer[Result]) : c.Tree = {
      val cont = TermName(c.freshName)
      var results_tmp = new ListBuffer[Result]()
      val result = TermName(c.freshName)
      val last_result = TermName(c.freshName)
      val tree = q"""
        var $cont = true
        var $last_result = $init
        while ($cont &&  !${input.isEOI}){
           ${parseRuleContent(a,results_tmp)}
           if (success)
            $last_result =  $f($last_result,${combineResults(results_tmp)})
           else
            $cont = false
        }
        $result = $last_result
        success = true
      """
      results_tmp = results_tmp.map(x => (x._1,x._2,false))
      results.append((result,Ident(TypeName("Any")),true))
      results.appendAll(results_tmp)
      tree
    }

    def parseMap(a:c.Tree,f:c.Tree,results:ListBuffer[Result]) : c.Tree = {
      val result = TermName(c.freshName)
      val results_tmp = new ListBuffer[Result]()
      val tree = q"""
          ${parseRuleContent(a,results_tmp)}
           if (success)
             $result = $f(${combineResults(results_tmp)})
        """
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      results.append((result,Ident(TypeName("Any")),true))
      tree
    }

    def parseValue(a:c.Tree, value:c.Tree,typ:c.Tree,results:ListBuffer[Result]) : c.Tree = {
      val result = TermName(c.freshName)
      val results_tmp = new ListBuffer[Result]()
      val tree = q"""
          ${parseRuleContent(a,results_tmp)}
           if (success)
             $result = $value
        """
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      results.append((result,Ident(TypeName(typ.toString)),true))//TODO à modifier
      tree
    }

    def parseFilter(a:c.Tree, f:c.Tree,typ:c.Tree,results:ListBuffer[Result]) : c.Tree = {
      val result = TermName(c.freshName)
      val results_tmp = new ListBuffer[Result]()
      val tree = input.mark{rollback =>
        q"""
          ${parseRuleContent(a,results_tmp)}
           if (success && $f(${combineResults(results_tmp)}))
             $result = ${combineResults(results_tmp)}
           else {
            success = false
            msg = "incorrect result for 'rule' at filter('rule') at " + ${input.pos}
            ${rollback}
           }
        """
      }
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      results.append((result,Ident(TypeName("Any")),true))
      tree
    }


    def parseElem(a:c.Tree,d:c.Tree,results:ListBuffer[Result]):c.Tree = {
      val result = TermName(c.freshName)
      results.append((result,Ident(TypeName(d.toString)),true))  //TODO check d.toString
      q"""
        if (${input.currentInput} == $a){
          $result = ${input.currentInput}
          ${input.advance}
          success = true
         }
         else {
            success = false
            msg = "expected '" + $a + "', got '" + ${input.currentInput} + "' at " + ${input.pos}  }
          """
    }

    def parseRange(a:c.Tree,b:c.Tree,d:c.Tree,results:ListBuffer[Result]): c.Tree = {
      val result = TermName(c.freshName)
      results.append((result,Ident(TypeName(d.toString)),true))  //TODO check d.toString
      q"""
        if (${input.currentInput} >= $a && ${input.currentInput} <= $b){
          $result = ${input.currentInput}
          ${input.advance}
          success = true
         }
         else {
            success = false
            msg = "expected in range ('" + $a + "', '" + $b + "'), got '" + ${input.currentInput} + "' at " + ${input.pos}  }
          """
    }

    def parseThen(a:c.Tree,b:c.Tree,results:ListBuffer[Result]): c.Tree = {
      q"""
          ${parseRuleContent(a,results)}
          if (success) {
            ${parseRuleContent(b,results)}
          }
       """
    }

    def parseThenRight(a:c.Tree,b:c.Tree,results:ListBuffer[Result]): c.Tree = {
      val results_tmp = new ListBuffer[Result]()
      val tree = q"""
          ${parseRuleContent(a,results_tmp)}
          if (success) {
            ${parseRuleContent(b,results)}
          }
       """
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      tree
    }

    def parseThenLeft(a:c.Tree,b:c.Tree,results:ListBuffer[Result]): c.Tree = {
      val results_tmp = new ListBuffer[Result]()
      val tree = q"""
          ${parseRuleContent(a,results)}
          if (success) {
            ${parseRuleContent(b,results_tmp)}
          }
       """
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      tree
    }

    def parseOr(a:c.Tree,b:c.Tree,results:ListBuffer[Result]): c.Tree = {
      val result = TermName(c.freshName)
      var results_tmp1 = new ListBuffer[Result]()
      var results_tmp2 = new ListBuffer[Result]()
      val tree = input.mark{ rollback =>
        q"""
          ${parseRuleContent(a,results_tmp1)}
          if (!success) {
            ${rollback}
            ${parseRuleContent(b,results_tmp2)}
            if (success)
              $result = ${combineResults(results_tmp2)}
          }
          else {
            $result = ${combineResults(results_tmp1)}
          }
        """
      }
      results_tmp1 = results_tmp1.map(x => (x._1,x._2,false))
      results_tmp2 = results_tmp2.map(x => (x._1,x._2,false))
      results.append((result,Ident(TypeName("Any")),true))
      results.appendAll(results_tmp1)
      results.appendAll(results_tmp2)
      tree
    }

    def parseRuleCall(ruleCall:TermName,results:ListBuffer[Result]): c.Tree = {
      val callResult = TermName(c.freshName)
      val result = TermName(c.freshName)
      results.append((result,Ident(TypeName("Any")),true))
      q"""
        val $callResult = ${ruleCall}(input)
        success = $callResult.success
        if (success){
          ${input.advanceTo(q"$callResult.inputPos")}
          $result = $callResult.result
         }
        else
          msg = $callResult.msg
        """
    }


    def parsePhrase(a:c.Tree,results:ListBuffer[Result]): c.Tree = {
      q"""
        ${parseRuleContent(a,results)}
        if (success) {
          if (!${input.isEOI}){
            success = false
            msg = "not all the input is consummed, at pos " + ${input.pos}
          }
        }
      """
    }

    def parseNot(a:c.Tree,results:ListBuffer[Result]): c.Tree = {
      var results_tmp = new ListBuffer[Result]()
      val tree =  input.mark{ rollback =>
        q"""
         ${parseRuleContent(a,results_tmp)}
         if (success) {
          success = false
          msg = "not parser expected failure at " + ${input.pos}
         }
         else {
          success = true
         }
         ${rollback}
       """
      }
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      tree
    }

    def parseGuard(a:c.Tree,results:ListBuffer[Result]): c.Tree = {
      var results_tmp = new ListBuffer[Result]()
      val tree =  input.mark{ rollback =>
        q"""
         ${parseRuleContent(a,results_tmp)}
         ${rollback}
       """
      }
      results.appendAll(results_tmp.map(x => (x._1,x._2,false)))
      tree
    }

    /**
     * Switch function which transform a parser combinator style into an imperative style
     * @param rule The code to be transformed
     * @param results The results variable name which will be generated in the transformed code
     * @return The expanded code
     */
    def parseRuleContent(rule:c.Tree,results:ListBuffer[Result]):c.Tree = rule match{
      case q"FastParsers.toElem[$d]($a)" =>
        parseElem(a,d,results)
      case q"FastParsers.range[$d]($a,$b)" =>
        parseRange(a,b,d,results)
      case q"$a ~[$d] $b" =>
        parseThen(a,b,results)
      case q"$a ~>[$d] $b" =>
        parseThenRight(a,b,results)
      case q"$a <~[$d] $b" =>
        parseThenLeft(a,b,results)
      case q"$a || $b" =>
        parseOr(a,b,results)
      case q"$a | $b" =>
        parseOr(a,b,results)
      case q"$a.rep($min,$max)" =>
        parseRep(a,min,max,results)
      case q"$a.repFold[..$d]($init)($f)" =>
        parseRepFold(a,init,f,results)
      case q"FastParsers.repFold[..$d]($a)($init)($f)" =>
        parseRepFold(a,init,f,results)
      case q"$a?" =>
        parseRep(a,q"0",q"1",results)
      case q"$a+" =>
        parseRep(a,q"1",q"-1",results)
      case q"$a*" =>
        parseRep(a,q"0",q"-1",results)
      case q"FastParsers.rep[$d]($a,$min,$max)" =>
        parseRep(a,min,max,results)
      case q"FastParsers.rep1[$d]($a)" =>
        parseRep(a,q"1",q"-1",results)
      case q"FastParsers.opt[$d]($a)" =>
        parseRep(a,q"0",q"1",results)
      case q"""${ruleCall : TermName}""" =>
        parseRuleCall(ruleCall,results)
      case q"$a map[$d] ($f)" =>
        parseMap(a,f,results)
      case q"$a ^^ [$d]($f)" =>
        parseMap(a,f,results)
      case q"$a ^^^ [$d]($f)" =>
        parseValue(a,f,d,results)
      case q"$a filter [$d]($f)" =>
        parseFilter(a,f,d,results)
      case q"FastParsers.phrase[$d]($a)" =>
        parsePhrase(a,results)
      case q"FastParsers.not[$d]($a)" =>
        parseNot(a,results)
      case q"FastParsers.guard[$d]($a)" =>
        parseGuard(a,results)
      case _ => q"""println(show(reify("rule error")))"""
    }

    /**
     * Expand each rule in a imperative style without considering other rules (i.e def rule2 = rule1 is not expanded to the code of rule1)
     * @return An HashMap containing (rulename, corresponging code)
     */
    def createBasicStructure = {
      val rulesMap = new HashMap[String,c.Tree]()
      rules match {
        case q"{..$body}" =>
          body.foreach (_ match {
            case q"def $name:$_ = $b" =>
              val TermName(nameString) = name
              val in = (nameString, parseRule(b))
              rulesMap += in
            //case q""  =>
            case q"()" =>
            case x => c.abort(c.enclosingPosition, "body must only contain rule definition with the following form : def ruleName = body : " + x)
          })
          c.Expr(q"""println(..${rulesMap.keys})""")
        case _ =>
          c.abort(c.enclosingPosition, "ill-formed body, cannot be empty")//TODO can be empty ?
      }
      rulesMap
    }

    def replaceInRules(rulesMap : HashMap[String,c.Tree]) = {
      val map = new HashMap[String,c.Tree]()
      for (k <- rulesMap.keys)  {
        val term = TermName(k)
        val ruleCode = q"""
        var input = i
        var success = false
        var msg = ""
        ${rulesMap(k)}
        """
        //map += ((k,q"def $term(i:Reader[Char]) = println(show(reify($ruleCode)))"))
        map += ((k,q"def $term(i:${input.inputType}) = $ruleCode"))
      }
      map
    }

    /**
      *
      * @param map Contain the rules name and their corresponding code
      * @return An AnyRef class containing where all rules are transformed in their expanded form
      */
    def createFastParser(map : HashMap[String,c.Tree]) = {
      val anon = TypeName(c.freshName)
      val dmmy = TermName(c.freshName)//no joke : see http://stackoverflow.com/questions/14370842/getting-a-structural-type-with-an-anonymous-classs-methods-from-a-macro


      val methods = map.values
      //create the final parser object which will be used
      q"""
        class $anon {
            import scala.collection.mutable.ListBuffer
           ..$methods
        }
        val $dmmy = 0
        new $anon
      """
    }

    val rulesMap = createBasicStructure
    val finalRulesMap = replaceInRules(rulesMap)
    val tree = createFastParser(finalRulesMap)
    c.Expr(tree)
  }
}