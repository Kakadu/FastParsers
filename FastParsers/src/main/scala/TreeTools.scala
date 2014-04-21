import scala.reflect.macros.whitebox.Context

/**
 * Created by Eric on 21.04.14.
 */
trait TreeTools {
  val c: Context
  import c.universe._

  def substituteSymbol(symbol: Symbol, value: c.Tree => c.Tree, in: c.Tree) = new Transformer {
    override def transform(tree: c.Tree): c.Tree =
      if (tree.symbol == symbol) value(tree)
      else  super.transform(tree)
  }.transform(in)

  def substituteName(name: TermName, value: c.Tree => c.Tree, in: c.Tree) = new Transformer {
    override def transform(tree: c.Tree): c.Tree = tree match {
      case t: TermName => c.abort(c.enclosingPosition,show("sads")); value(tree)
      case _ => super.transform(tree)
    }
  }.transform(in)

  def getInnerTypeOf[T : TypeTag](typ: c.Type): Option[c.Type] = typ match {
    case TypeRef(_, _, List(z)) if typ <:< typeOf[T] => Some(z)
    case _ => None
  }

}