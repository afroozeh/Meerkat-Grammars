package benchmark

import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._
import org.meerkat.util._
import org.meerkat.benchmark.MeerkatBenchmark
import java.io.File
import org.meerkat.util.visualization._

object JavaBenchmark {
  
  import grammar.java._
  import grammar.java.lexicals._
  
  def main(args: Array[String]): Unit = {
    
    val grammar = new Specification with CharLevel {
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

object GammaBenchmark {
  
  def getInput(l: Int): Input = Input((1 to l).foldLeft("")((s, _) => s + "b"))
  
  def main(args: Array[String]): Unit = {
    import grammar.gamma.Gamma3._
    
    for (i <- 1 to 10) {
      parse(S, getInput(300)) match {
        case Right(x) => println(x.stat)
        case Left(x)  => println("Parse error")
      }
    }
  }
  
} 
