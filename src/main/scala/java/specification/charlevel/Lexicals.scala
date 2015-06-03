package java.specification

import org.meerkat.Syntax._
import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._
//import org.meerkat.util.RegularExpression._

object Lexicals {
  
      val WhiteSpace = 
        syn( "a" | "a" )
    
//    val UnicodeInputCharacter = 
//    syn ( UnicodeEscape 
//        | RawInputCharacter
//        )
    
    val u = 
    syn ( 'u' )
    
//    val UnicodeEscape = 
//    syn ( '\\' ~~ u.++ ~~ HexDigit ~~ HexDigit ~~ HexDigit ~~ HexDigit )
    
    val RawInputCharacter = 
    syn ( "[^\\]".r  // Todo: Add a not combiantor 
        | "\\".!>>("""[u\\]""".r) 
        | "\\\\" 
        )
    
//    val InputCharacter = 
//    syn ( UnicodeInputCharacter.\('\r', '\n') | '\u0000')
    

    
//    val LineTerminator = 
//    syn ( '\r' | '\n' )
//    
//    val Comment = 
//    syn ( TraditionalComment 
//        | EndOfLineComment
//        )
//    
//    val TraditionalComment= 
//    syn ( "/*" ~~ CommentTail )
//    
//    val EndOfLineComment= 
//    syn ( "//" ~~ InputCharacter.**.!>>(""".""".r) )
//    
//    val CommentTail: Nonterminal = 
//    syn ( '*' ~~ CommentTailStar 
//        | NotStar ~~ CommentTail 
//        )
//    
//    val CommentTailStar= 
//    syn ( '/' 
//        | '*' ~~ CommentTailStar 
//        | NotStarNotSlash ~~ CommentTail
//        )
//    
//    val NotStar= 
//    syn ( InputCharacter.\('*') 
//        | LineTerminator
//        )
//    
//    val NotStarNotSlash= 
//    syn ( InputCharacter.\('*', '/') 
//        | LineTerminator
//        )
//    
//    val Identifier= 
//    syn ( IdentifierChars.\(Keyword).!>>("""[$0-9A-Z_a-z]""".r).!<<("""[$0-9A-Z_a-z]""".r) )
//    
//    val IdentifierChars: Nonterminal = 
//    syn ( JavaLetter 
//        | IdentifierChars ~~ JavaLetterOrDigit
//        )
//    
//    val JavaLetter= 
//    syn ( "$_a-zA-Z".r )
//    
//    val JavaLetterOrDigit= 
//    syn ( "$_a-zA-Z0-9" )
//    
//    
//    val DecimalIntegerLiteral= 
//    syn ( DecimalNumeral ~~ IntegerTypeSuffix.? )
//    
//    val HexIntegerLiteral= 
//    syn ( HexNumeral ~~ IntegerTypeSuffix.? )
//    
//    val OctalIntegerLiteral= 
//    syn ( OctalNumeral ~~ IntegerTypeSuffix.? )
//    
//    val BinaryIntegerLiteral= 
//    syn ( BinaryNumeral ~~ IntegerTypeSuffix.? )
//    
//    val IntegerTypeSuffix= 
//    syn ( 'l' | 'L')
//    
//    val underscore= 
//    syn ( '_' )
//    
//    val DecimalNumeral= 
//    syn( '0' 
//       | NonZeroDigit ~~ Digits.? 
//       | NonZeroDigit ~~ underscore.++ ~~ Digits
//       )
//    
//    val Digits= 
//    syn ( Digit 
//        | Digit ~~ DigitOrUnderscore.** ~~ Digit
//        )
//    
//    val Digit= 
//    syn ( '0' 
//        | NonZeroDigit
//        )
//    
//    val NonZeroDigit= 
//    syn ( '1'--'9' )
//    
//    val DigitOrUnderscore= 
//    syn ( Digit 
//        | '_'
//        )
//    
//    val HexNumeral= 
//    syn ( '0' ~~ 'x' ~~ HexDigits 
//        | '0' ~~ 'X' ~~ HexDigits
//        )
//    
//    val HexDigits= 
//    syn ( HexDigit 
//        | HexDigit ~~ HexDigitOrUnderscore.** ~~ HexDigit
//        )
//    
//    val HexDigit= 
//    syn ( "0-9a-fA-F".r )
//    
//    val HexDigitOrUnderscore= 
//    syn ( HexDigit 
//        | '_'
//        )
//    
//    val OctalNumeral= 
//    syn ( '0' ~~ OctalDigits 
//        | '0' ~~ underscore.++ ~~ OctalDigits
//        )
//    
//    val OctalDigits= 
//    syn ( OctalDigit 
//        | OctalDigit ~~ OctalDigitOrUnderscore.** ~~ OctalDigit
//        )
//    
//    val OctalDigit= 
//    syn ( "[0-7]".r )
//    
//    val OctalDigitOrUnderscore= 
//    syn ( OctalDigit 
//        | '_'
//        )
//    
//    val BinaryNumeral= 
//    syn ( '0' ~~ 'b' ~~ BinaryDigits 
//        | '0' ~~ 'B' ~~ BinaryDigits
//        )
//    
//    val BinaryDigits= 
//    syn ( BinaryDigit 
//        | BinaryDigit ~~ BinaryDigitOrUnderscore.** ~~ BinaryDigit
//        )
//    
//    val BinaryDigit= 
//    syn ( '0' | '1' )
//    
//    val BinaryDigitOrUnderscore= 
//    syn ( BinaryDigit 
//        | '_'
//        )
//    
//    val DecimalFloatingPointLiteral = 
//    syn ( Digits ~~ '.' ~~ Digits.? ~~ ExponentPart.? ~~ FloatTypeSuffix.? 
//        | '.' ~~ Digits ~~ ExponentPart.? ~~ FloatTypeSuffix.? 
//        | Digits ~~ ExponentPart 
//        | Digits ~~ FloatTypeSuffix 
//        | Digits ~~ ExponentPart ~~ FloatTypeSuffix
//        )
//    
//    val ExponentPart = 
//    syn (ExponentIndicator ~~ SignedInteger )
//    
//    val ExponentIndicator = 
//    syn ( charClass('e', 'E')() )
//    
//    val SignedInteger= 
//    syn (  Sign.? ~~ Digits )
//    
//    val Sign = 
//    syn ( '+' | '-' )
//    
//    val FloatTypeSuffix= 
//    syn ( 'f' | 'F' | 'd' | 'D')
//    
//    val HexadecimalFloatingPointLiteral= 
//    syn ( HexSignificand ~~ BinaryExponent ~~ FloatTypeSuffix.? )
//    
//    val HexSignificand= 
//    syn ( HexNumeral 
//        | HexNumeral ~~ '.' 
//        | '0' ~~ 'x' ~~ HexDigits.? ~~ '.' ~~ HexDigits 
//        | '0' ~~ 'X' ~~ HexDigits.? ~~ '.' ~~ HexDigits
//        )
//    
//    val BinaryExponent= 
//    syn ( BinaryExponentIndicator ~~ SignedInteger )
//    
//    val BinaryExponentIndicator= 
//    syn ( 'p' | 'P' )
//    
//    val BooleanLiteral= 
//    syn ( "true" 
//        | "false"
//        )
//    
//    val CharacterLiteral= 
//    syn ( '\'' ~~ SingleCharacter ~~ '\'' 
//        | '\'' ~~ EscapeSequence ~~ '\''
//        )
//    
//    val SingleCharacter= 
//    syn ( InputCharacter.\(''', '\\') )
//    
//    val StringLiteral= 
//    syn ( '"' ~~ StringCharacter.** ~~ '"' )
//    
//    val StringCharacter= 
//    syn ( InputCharacter.\('"', '\\') 
//        | EscapeSequence
//        )
//    
//    val EscapeSequence= 
//    syn ( Backslash ~~ 'b' 
//        | Backslash ~~ 't' 
//        | Backslash ~~ 'n' 
//        | Backslash ~~ 'f' 
//        | Backslash ~~ 'r' 
//        | Backslash ~~ '"' 
//        | Backslash ~~ '\'' 
//        | '\\' ~~ u.++ ~~ "005" ~~ charClass('c', 'C')() ~~ '\\' ~~ u.++ ~~ "005" ~~ charClass('c', 'C')() 
//        | OctalEscape
//        )
//    
//    val Backslash= 
//    syn ( '\\' ~~ u.++ ~~ "005" ~~ ('c' | 'C').gr 
//        | '\\'
//        )
//    
//    val OctalEscape= 
//    syn ( '\\' ~~ OctalDigit.!>>("""[0-7]""".r) 
//        | '\\' ~~ OctalDigit ~~ OctalDigit.!>>("""[0-7]""".r) 
//        | '\\' ~~ ZeroToThree ~~ OctalDigit ~~ OctalDigit
//        )
//    
//    val ZeroToThree = 
//    syn ( '0' | '1' | '2' | '3' )
//    
//    val NullLiteral = 
//    syn ( "null" )
//    
//    
////    val Keyword:Regex = """abstract|continue|for|new|switch|assert|default|if|package|synchronized|boolean|do|goto|private|this|break|double|implements|protected|throw|byte|else|import|public|throws|case|enum|instanceof|return|transient|catch|extends|int|short|try|char|final|interface|static|void|class|finally|long|strictfp|volatile|const|float|native|super|while|true|false|null""".r
//      
//    val Keyword = {
//      val s: scala.collection.mutable.Set[String] = new java.util.HashSet[String]()
//      val l = List( "abstract", "continue", "for", "new", "switch", "assert", "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", "this", "break", "double", "implements", "protected", "throw", "byte", "else", "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", "void", "class", "finally", "long", "strictfp", "volatile", "const", "float", "native", "super", "while", "true", "false", "null")
//      for (k <- l) {
//        s += k
//      }
//      s
//    }
//    
//    // (WhiteSpace | Comment)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//";
//    override val Layout= 
// syn( 
//    "L" ::= ((Comment | WhiteSpace).gr.**).!>>("\t", "\n", "\r", "\f", " ", "/*", "//")

  
}