package br.unb.cic.oberon.interpreter
import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}
import br.unb.cic.oberon.ir.ast._
import br.unb.cic.oberon.environment.Environment
import br.unb.cic.oberon.stdlib.StandardLibrary
import br.unb.cic.oberon.util.Values
import br.unb.cic.oberon.visitor.OberonVisitorAdapter
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.language.{existentials, postfixOps}

/**
 * This interpreter only runs tests
 */
class TestInterpreter extends OberonVisitorAdapter {

  type T = Unit

  var env = new Environment[Expression]()

  var printStream: PrintStream = new PrintStream(System.out)

  override def visit(module: OberonModule): Unit = {
    // set up global declarations
    module.tests.foreach(t => t.accept(this))

    // executes tests
    module.tests.foreach(t =>
      if (t.modifier == "TEST") (callTest(t.name), env = env.pop)
    )
  }

  override def visit(stmt: Statement): Unit = {
    // we pattern match on the statement to check which assert it is
    stmt match{
      case AssertTrueStmt(exp: Expression) =>
        if (!evalCondition(exp)) throw new Exception("Exception thrown from assert true")

      case AssertError() =>
        throw new Exception("Exception thrown from assert error")

      case AssertNotEqualStmt(left, right) =>
        if (left == right) throw new Exception("Exception thrown from assert not equal")

      case AssertEqualStmt(left, right) =>
        if (left != right) throw new Exception("Exception thrown from assert equal")
    }
  }

  def callTest(name: String): Unit = {
    val test = env.findTest(name)
    updateEnvironmentWithTest(test)
    test.stmt.accept(this)
  }

  def updateEnvironmentWithTest(test: Test): Unit = {
    env = env.push() // indicates a test.

    test.constants.foreach(c => env = env.setLocalVariable(c.name, c.exp))
    test.variables.foreach(v => env = env.setLocalVariable(v.name, Undef()))
  }

  def setTestEnvironment() = {
    printStream = new PrintStream(new NullPrintStream())
  }

  def evalCondition(expression: Expression): Boolean = {
    val evalVisitor = new EvalExpressionVisitor(this)
    expression.accept(evalVisitor).asInstanceOf[BoolValue].value
  }

  class EvalExpressionVisitor(val interpreter: TestInterpreter) extends OberonVisitorAdapter {
    type T = Expression
    
}