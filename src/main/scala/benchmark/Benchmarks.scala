package benchmark

import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._
import org.meerkat.util._
import org.meerkat.benchmark.MeerkatBenchmark

object JavaBenchmark {
	
  import grammar.java.specification.Syntax._
  import grammar.java.specification.charlevel.Lexicals._
  
  
  def main(args: Array[String]): Unit = {
    MeerkatBenchmark(start(CompilationUnit), "/Users/aliafroozeh/corpus/Java/jdk1.7.0_60-b19".load("java")).run
  }
  
}