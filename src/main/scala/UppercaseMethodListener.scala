import com.racerdfix.antlr.Java8BaseListener
import com.racerdfix.antlr.Java8Parser
import org.antlr.v4.runtime.tree.TerminalNode
import java.util
import java.util.{ArrayList, Collections, List}


class UppercaseMethodListener extends Java8BaseListener {
  private val errors = new util.ArrayList[String]

  override def enterMethodDeclarator(ctx: Java8Parser.MethodDeclaratorContext): Unit = {
    val node = ctx.Identifier
    val methodName = node.getText
    if (Character.isUpperCase(methodName.charAt(0))) errors.add(String.format("Method %s is uppercased!", methodName))
  }

  def getErrors: util.List[String] = Collections.unmodifiableList(errors)
}
