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

package grammar.java.lexicals

import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.meerkat.parsers.Parsers._
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