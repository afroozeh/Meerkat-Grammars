package benchmark

import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._
import org.meerkat.util._
import org.meerkat.benchmark.MeerkatBenchmark
import java.io.File
import grammar.java.Specification
import grammar.java.lexicals.CharLevel
import grammar.java.lexicals.Regex
import org.meerkat.util.visualization._
import grammar.java.Natural

object JavaBenchmark {
	  
  
  def main(args: Array[String]): Unit = {
    
    val grammar = new Natural with CharLevel {
      implicit val L = layout(Layout) 
    }
    
    val startSymbol = start(grammar.CompilationUnit)(grammar.L)

    MeerkatBenchmark(startSymbol, "/Users/aliafroozeh/corpus/Java/jdk1.7.0_60-b19".load("java")).run
    
//    val input = Input("123")
//    val t = parse(grammar.FloatingPointLiteral, input)
//    visualize(t.right.get.sppf, input)
    
//    val l = List(new File("/Users/aliafroozeh/Test.java"))
//    MeerkatBenchmark(startSymbol, l).run
  }
  
}