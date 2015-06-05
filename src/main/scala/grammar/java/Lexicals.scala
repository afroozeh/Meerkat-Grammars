package grammar.java

import org.meerkat.tmp.Parsers
import scala.collection.JavaConversions._
import scala.collection.mutable._
import java.util.Arrays

trait Lexicals {
  
  type T = Parsers.Symbol { type Value = org.meerkat.tmp.NoValue }
  
  def JavaLetter: T
  def Digit: T
  def Identifier: T
  def IntegerTypeSuffix: T
  def Digits: T
  def NonZeroDigit: T
  def DecimalNumeral: T
  def DecimalIntegerLiteral: T
  def HexDigits: T
  def HexNumeral: T
  def HexIntegerLiteral: T
  def OctalDigits: T
  def OctalNumeral: T
  def OctalIntegerLiteral: T
  def BinaryDigits: T
  def BinaryNumeral: T
  def BinaryIntegerLiteral: T
  def Sign: T
  def SignedInteger: T
  def ExponentIndicator: T
  def ExponentPart: T
  def FloatTypeSuffix: T
  def DecimalFloatingPointLiteral: T
  def HexSignificand: T
  def BinaryExponentIndicator: T
  def BinaryExponent: T
  def HexadecimalFloatingPointLiteral: T
  def EscapeSequence: T
  def OctalEscape: T
  def SingleCharacter: T
  def CharacterLiteral: T
  def StringCharacter: T
  def StringLiteral: T
  def BooleanLiteral: T
  def NullLiteral: T
  
  def Comment: T
  def WhiteSpace: T
  def Layout: T
  
  def Keyword: Set[String] = 
    new java.util.HashSet[String](Arrays.asList("abstract", "continue", "for", "new", "switch"
    , "assert", "default", "if", "package", "synchronized"
    , "boolean", "do", "goto", "private", "this", "break"
    , "double", "implements", "protected", "throw"
    , "byte", "else", "import", "public", "throws"
    , "case", "enum", "instanceof", "return", "transient"
    , "catch", "extends", "int", "short", "try"
    , "char", "final", "interface", "static"
    , "void", "class", "finally", "long", "strictfp"
    , "volatile", "const", "float", "native", "super"
    , "while", "true", "false", "null"))

}