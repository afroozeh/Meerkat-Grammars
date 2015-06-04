package benchmark

import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._
import org.meerkat.util._
import org.meerkat.benchmark.MeerkatBenchmark
import java.io.File

object JavaBenchmark {
	
  import grammar.java.Specification._
  import grammar.java.lexicals.CharLevel._
  
  
  def main(args: Array[String]): Unit = {
    MeerkatBenchmark(start(CompilationUnit), "/Users/aliafroozeh/corpus/Java/jdk1.7.0_60-b19".load("java")).run
    
//      println(parse(HexIntegerLiteral, "0xFFFFL"))    
    
//    val l = List(new File("/Users/aliafroozeh/corpus/Java/jdk1.7.0_60-b19/src/com/sun/corba/se/impl/corba/TCUtility.java"))
//    MeerkatBenchmark(start(CompilationUnit), l).run
  }
  
}