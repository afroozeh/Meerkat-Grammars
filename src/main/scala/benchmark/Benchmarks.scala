/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package benchmark

import org.meerkat.parsers._
import org.meerkat.parsers.Parsers._
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
