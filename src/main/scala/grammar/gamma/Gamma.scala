package grammar.gamma

import org.meerkat.Syntax._
import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._

object Gamma3 {
  
  val S: Nonterminal = 
  syn ( S ~~ S ~~ S
      | S ~~ S
      | "b"
      )
}