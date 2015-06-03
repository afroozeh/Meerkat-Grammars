package benchmark

import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._

object JavaBenchmark {
	
  import grammar.java.specification.Syntax._
  import grammar.java.specification.charlevel.Lexicals._
  
  
  def main(args: Array[String]): Unit = {
    val f = "/Users/aliafroozeh/Test.java"
    val input: String = scala.io.Source.fromFile(f).mkString
    
    parse(input, start(CompilationUnit))
  }
  
}