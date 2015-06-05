package grammar.java.lexicals

import org.meerkat.Syntax._
import org.meerkat.tmp._
import org.meerkat.tmp.Parsers._
import scala.collection.JavaConversions._
import grammar.java.Lexicals

trait CharLevel extends Lexicals {
  
    val UnicodeInputCharacter = 
    syn ( UnicodeEscape 
        | RawInputCharacter
        )
    
    val u = 
    syn ( "u" )
    
    val UnicodeEscape = 
    syn ( "\\" ~~ u.++ ~~ HexDigit ~~ HexDigit ~~ HexDigit ~~ HexDigit )
    
    val RawInputCharacter = 
    syn ( """[^\\]""".r  // Todo: Add a not combiantor 
        | "\\".!>>("""[u\\]""".r) 
        | "\\\\" 
        )
    
    val InputCharacter = 
    syn ( UnicodeInputCharacter.\("\r", "\n") | "\u0000")
    
    val LineTerminator = 
    syn ( "\r" | "\n" )
    
    val Comment = 
    syn ( TraditionalComment 
        | EndOfLineComment
        )
    
    val TraditionalComment= 
    syn ( "/*" ~~ CommentTail )
    
    val EndOfLineComment= 
    syn ( "//" ~~ InputCharacter.**.!>>(""".""".r) )
    
    val CommentTail: Nonterminal = 
    syn ( "*" ~~ CommentTailStar 
        | NotStar ~~ CommentTail 
        )
    
    val CommentTailStar: Nonterminal = 
    syn ( "/" 
        | "*" ~~ CommentTailStar 
        | NotStarNotSlash ~~ CommentTail
        )
    
    val NotStar= 
    syn ( InputCharacter.\('*') 
        | LineTerminator
        )
    
    val NotStarNotSlash= 
    syn ( InputCharacter.\("*", "/") 
        | LineTerminator
        )
    
    val Identifier: Nonterminal = 
    syn ( IdentifierChars.\(Keyword).!>>("""[$0-9A-Z_a-z]""".r).!<<("""[$0-9A-Z_a-z]""".r) )
    
    val IdentifierChars: Nonterminal = 
    syn ( JavaLetter 
        | IdentifierChars ~~ JavaLetterOrDigit
        )
    
    val JavaLetter = 
    syn ( "[$_a-zA-Z]".r )
    
    val JavaLetterOrDigit = 
    syn ( "[$_a-zA-Z0-9]".r )
    
    val DecimalIntegerLiteral = 
    syn ( DecimalNumeral ~~ IntegerTypeSuffix.? )
    
    val HexIntegerLiteral = 
    syn ( HexNumeral ~~ IntegerTypeSuffix.? )
    
    val OctalIntegerLiteral = 
    syn ( OctalNumeral ~~ IntegerTypeSuffix.? )
    
    val BinaryIntegerLiteral = 
    syn ( BinaryNumeral ~~ IntegerTypeSuffix.? )
    
    val IntegerTypeSuffix = 
    syn ( "l" | "L")
    
    val underscore = 
    syn ( "_" )
    
    val DecimalNumeral = 
    syn( "0" 
       | NonZeroDigit ~~ Digits.? 
       | NonZeroDigit ~~ underscore.++ ~~ Digits
       )
    
    val Digits = 
    syn ( Digit 
        | Digit ~~ DigitOrUnderscore.** ~~ Digit
        )
    
    val Digit = 
    syn ( "0" 
        | NonZeroDigit
        )
    
    val NonZeroDigit = 
    syn ( "[1-9]".r )
    
    val DigitOrUnderscore = 
    syn ( Digit 
        | "_"
        )
    
    val HexNumeral = 
    syn ( "0" ~~ "x" ~~ HexDigits 
        | "0" ~~ "X" ~~ HexDigits
        )
    
    val HexDigits = 
    syn ( HexDigit 
        | HexDigit ~~ HexDigitOrUnderscore.** ~~ HexDigit
        )
    
    val HexDigit = 
    syn ( "[0-9a-fA-F]".r )
    
    val HexDigitOrUnderscore = 
    syn ( HexDigit 
        | "_"
        )
    
    val OctalNumeral = 
    syn ( "0" ~~ OctalDigits 
        | "0" ~~ underscore.++ ~~ OctalDigits
        )
    
    val OctalDigits = 
    syn ( OctalDigit 
        | OctalDigit ~~ OctalDigitOrUnderscore.** ~~ OctalDigit
        )
    
    val OctalDigit = 
    syn ( "[0-7]".r )
    
    val OctalDigitOrUnderscore = 
    syn ( OctalDigit 
        | "_"
        )
    
    val BinaryNumeral = 
    syn ( "0" ~~ "b" ~~ BinaryDigits 
        | "0" ~~ "B" ~~ BinaryDigits
        )
    
    val BinaryDigits = 
    syn ( BinaryDigit 
        | BinaryDigit ~~ BinaryDigitOrUnderscore.** ~~ BinaryDigit
        )
    
    val BinaryDigit = 
    syn ( "0" | "1" )
    
    val BinaryDigitOrUnderscore = 
    syn ( BinaryDigit 
        | "_"
        )
    
    val DecimalFloatingPointLiteral = 
    syn ( Digits ~~ "." ~~ Digits.? ~~ ExponentPart.? ~~ FloatTypeSuffix.? 
        | "." ~~ Digits ~~ ExponentPart.? ~~ FloatTypeSuffix.? 
        | Digits ~~ ExponentPart 
        | Digits ~~ FloatTypeSuffix 
        | Digits ~~ ExponentPart ~~ FloatTypeSuffix
        )
    
    val ExponentPart = 
    syn (ExponentIndicator ~~ SignedInteger )
    
    val ExponentIndicator = 
    syn ( "e" | "E" )
    
    val SignedInteger = 
    syn (  Sign.? ~~ Digits )
    
    val Sign = 
    syn ( "+" | "-" )
    
    val FloatTypeSuffix = 
    syn ( "f" | "F" | "d" | "D")
    
    val HexadecimalFloatingPointLiteral = 
    syn ( HexSignificand ~~ BinaryExponent ~~ FloatTypeSuffix.? )
    
    val HexSignificand = 
    syn ( HexNumeral 
        | HexNumeral ~~ "." 
        | "0" ~~ "x" ~~ HexDigits.? ~~ "." ~~ HexDigits 
        | "0" ~~ "X" ~~ HexDigits.? ~~ "." ~~ HexDigits
        )
    
    val BinaryExponent = 
    syn ( BinaryExponentIndicator ~~ SignedInteger )
    
    val BinaryExponentIndicator = 
    syn ( "p" | "P" )
    
    val BooleanLiteral = 
    syn ( "true" 
        | "false"
        )
    
    val CharacterLiteral = 
    syn ( "'" ~~ SingleCharacter ~~ "'" 
        | "'" ~~ EscapeSequence ~~ "'"
        )
    
    val SingleCharacter = 
    syn ( InputCharacter.\("'", "\\") )
    
    val StringLiteral = 
    syn ( "\"" ~~ StringCharacter.** ~~ "\"" )
    
    val StringCharacter = 
    syn ( InputCharacter.\("\"", "\\") 
        | EscapeSequence
        )
    
    val EscapeSequence = 
    syn ( Backslash ~~ "b" 
        | Backslash ~~ "t" 
        | Backslash ~~ "n" 
        | Backslash ~~ "f" 
        | Backslash ~~ "r" 
        | Backslash ~~ "\"" 
        | Backslash ~~ "'" 
        | "\\" ~~ u.++ ~~ "005" ~~ ("c" | "C").! ~~ "\\" ~~ u.++ ~~ "005" ~~ ("c" | "C").!
        | OctalEscape
        )
    
    val Backslash = 
    syn ( "\\" ~~ u.++ ~~ "005" ~~ ("c" | "C").!
        | "\\"
        )
    
    val OctalEscape = 
    syn ( "\\" ~~ OctalDigit.!>>("[0-7]".r) 
        | "\\" ~~ OctalDigit ~~ OctalDigit.!>>("[0-7]".r) 
        | "\\" ~~ ZeroToThree ~~ OctalDigit ~~ OctalDigit
        )
    
    val ZeroToThree = 
    syn ( "0" | "1" | "2" | "3" )
    
    val NullLiteral = 
    syn ( "null" )
    
    val WhiteSpace = 
    syn (" " | "\t" | "\n" | "\r" | "\f" | "\u0a1a")

    
    // (WhiteSpace | Comment)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//";
    val Layout: Nonterminal = 
    syn (((Comment | WhiteSpace).!.**).!>>("\t", "\n", "\r", "\f", " ", "/*", "//"))

}