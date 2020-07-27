import java.util
import java.util.Collections

import com.racerdfix.antlr.{Java8BaseVisitor, Java8Parser}


class UppercaseMethodVisitor extends Java8BaseVisitor[Unit] {
  private val errors = new util.ArrayList[String]

  override def visitMethodDeclarator(ctx: Java8Parser.MethodDeclaratorContext): Unit = {
    val node = ctx.Identifier
    val methodName = node.getText

    println("==========================================")
    println("toString:" + node.toString)
    println("getText:" + node.getText)
    println("LineNo:" + ctx.getStart.getLine)

    if (Character.isUpperCase(methodName.charAt(0))) errors.add(String.format("Method %s is uppercased!", methodName))
  }

  def getErrors: util.List[String] = Collections.unmodifiableList(errors)
}
