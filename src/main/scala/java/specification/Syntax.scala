///*
// * Copyright (c) 2014 CWI. All rights reserved.
// * 
// * Authors:
// *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
// *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
// */
//package org.meerkat.java7
//
//import org.meerkat.Syntax._
//import org.meerkat.tmp._
//import org.meerkat.tmp.Parsers._
//import org.meerkat.tmp.OperatorParsers._
//import org.meerkat.tmp.DefaultLayout._
//
///**
// * 
// * The grammar of Java 7 
// * 
// */
//object JavaCharlevel {
//  
//    import charlevel.Lexicals._
//  
//    val Type =  
//    syn ( PrimitiveType 
//        | ReferenceType 
//        )
//  
//    val PrimitiveType = 
//    syn ( "byte" 
//        | "short" 
//        | "char" 
//        | "int" 
//        | "long" 
//        | "float"
//        | "double" 
//        | "boolean" 
//        )
//    
//    val ReferenceType = 
//    syn ( TypeDeclSpecifier ~ TypeArguments.? 
//        | ArrayType 
//        )
//
//    val ReferenceTypeNonArrayType = 
//    syn ( TypeDeclSpecifier ~ TypeArguments.? )
//    
//    val TypeList =  
//    syn ( Type.+(',') )
//
//    val TypeName =  
//    syn ( QualifiedIdentifier )
//
//    val TypeVariable =  
//    syn ( Identifier )
//
//    val ArrayType =  
//    syn ( Type ~ '[' ~ ']' )
//
//    val TypeParameters = 
//    syn ( '<' ~ TypeParameter.+(',') ~ '>' )
//    
//    val TypeParameter =
//    syn ( TypeVariable ~ TypeBound.? )
//    
//    val TypeBound = 
//    syn ( "extends" ~ ReferenceType.+("&") )
//    
//    val TypeArguments =
//    syn ( '<' ~ TypeArgument.+(',') ~ '>' )
//    
//    val TypeArgument = 
//    syn ( Type 
//        | '?' ~ (("extends" | "super").gr ~ Type).gr.?
//        )
//    
//    val QualifiedIdentifier = 
//    syn ( Identifier.+(".") )
//    
//    val QualifiedIdentifierList = 
//    syn ( QualifiedIdentifier.+(',') )
//    
//    val CompilationUnit = 
//    syn ( PackageDeclaration.? ~ ImportDeclaration.* ~ TypeDeclaration.* )
//    
//    val PackageDeclaration = 
//    syn ( Annotation.* ~ "package" ~ QualifiedIdentifier ~ ";" )
//    
//    val ImportDeclaration = 
//    syn ( "import" ~ "static".? ~ Identifier.+(".") ~ ('.' ~ '*').gr.? ~ ';' )
//    
//    val TypeDeclaration = 
//    syn ( ClassDeclaration 
//        | InterfaceDeclaration 
//        | ';'
//        )
//    
//    val ClassDeclaration = 
//    syn ( NormalClassDeclaration 
//        | EnumDeclaration
//        )
//    
//    val NormalClassDeclaration = 
//    syn ( ClassModifier.* ~ "class" ~ Identifier ~ TypeParameters.? ~ ("extends" ~ Type).gr.? ~ ("implements" ~ TypeList).gr.? ~ ClassBody )
//    
//    val ClassModifier = 
//    syn ( Annotation 
//        | "public" 
//        | "protected" 
//        | "private" 
//        | "abstract" 
//        | "static" 
//        | "final" 
//        | "strictfp"
//        )
//    
//    val ClassBody = 
//    syn ( '{' ~ ClassBodyDeclaration.* ~ '}' )
//    
//    val ClassBodyDeclaration = 
//    syn ( ClassMemberDeclaration 
//        | InstanceInitializer 
//        | StaticInitializer 
//        | ConstructorDeclaration
//        )
//    
//    val InstanceInitializer = 
//    syn ( Block )
//    
//    val StaticInitializer = 
//    syn ( "static" ~ Block )
//    
//    val ConstructorDeclaration = 
//    syn ( ConstructorModifier.* ~ ConstructorDeclarator ~ Throws.? ~ ConstructorBody )
//    
//    val ConstructorModifier = 
//    syn ( Annotation 
//        | "public" 
//        | "protected" 
//        | "private"
//        )
//    
//    val ConstructorDeclarator = 
//    syn ( TypeParameters.? ~ Identifier ~ '(' ~ FormalParameterList.? ~ ')' )
//    
//    val ConstructorBody = 
//    syn ( '{' ~ ExplicitConstructorInvocation.? ~ BlockStatement.* ~ '}' )
//    
//    val ExplicitConstructorInvocation = 
//    syn ( NonWildTypeArguments.? ~ "this" ~ '(' ~ ArgumentList.? ~ ')' ~ ';' 
//        | NonWildTypeArguments.? ~ "super" ~ '(' ~ ArgumentList.? ~ ")" ~ ';' 
//        | Primary ~ '.' ~ NonWildTypeArguments.? ~ "super" ~ '(' ~ ArgumentList.? ~ ')' ~ ';'
//        )
//    
//    val NonWildTypeArguments = 
//    syn ( '<' ~ ReferenceType.+(',') ~ '>' )
//    
//    val ClassMemberDeclaration = 
//    syn ( FieldDeclaration 
//        | MethodDeclaration 
//        | ClassDeclaration 
//        | InterfaceDeclaration 
//        | ';'
//        )
//    
//    val InterfaceDeclaration =
//    syn ( NormalInterfaceDeclaration 
//        | AnnotationTypeDeclaration
//        )
//    
//    val NormalInterfaceDeclaration = 
//    syn ( InterfaceModifier.* ~ "interface" ~ Identifier ~ TypeParameters.? ~ ("extends" ~ TypeList).gr.? ~ InterfaceBody )
//    
//    val InterfaceModifier = 
//    syn ( Annotation 
//        | "public" 
//        | "protected" 
//        | "private" 
//        | "abstract" 
//        | "static" 
//        | "strictfp"
//        )
//    
//    val InterfaceBody = 
//    syn ( "{" ~ InterfaceMemberDeclaration.* ~ "}" )
//    
//    val InterfaceMemberDeclaration = 
//    syn ( ConstantDeclaration 
//        | AbstractMethodDeclaration 
//        | ClassDeclaration 
//        | InterfaceDeclaration 
//        | ';'
//        )
//    
//    val ConstantDeclaration = 
//    syn ( ConstantModifier.* ~ Type ~ VariableDeclarators ~ ';' )
//    
//    val ConstantModifier = 
//    syn ( Annotation 
//        | "public" 
//        | "static" 
//        | "final"
//        )
//    
//    val AbstractMethodDeclaration = 
//    syn ( AbstractMethodModifier.* ~ TypeParameters.? ~ Result ~ MethodDeclarator ~ Throws.? ~ ';' )
//    
//    val AbstractMethodModifier = 
//    syn ( Annotation 
//        | "public" 
//        | "abstract"
//        )
//    
//    val AnnotationTypeDeclaration = 
//    syn ( InterfaceModifier.* ~ '@' ~ "interface" ~ Identifier ~ AnnotationTypeBody )
//    
//    val AnnotationTypeBody = 
//    syn ( '{' ~ AnnotationTypeElementDeclaration.* ~ '}' )
//    
//    val AnnotationTypeElementDeclaration = 
//    syn ( AbstractMethodModifier.* ~ Type ~ Identifier ~ '(' ~ ')' ~ ('[' ~ ']').gr.* ~ DefaultValue.? ~ ';' 
//        | ConstantDeclaration 
//        | ClassDeclaration 
//        | InterfaceDeclaration 
//        | AnnotationTypeDeclaration 
//        | ';'
//        )
//    
//    val DefaultValue =  
//    syn ( "default" ~ ElementValue )
//    
//    val FieldDeclaration =  
//    syn ( FieldModifier.* ~ Type ~ VariableDeclarators ~ ';' )
//    
//    val FieldModifier = 
//    syn ( Annotation 
//        | "public" 
//        | "protected" 
//        | "private" 
//        | "static" 
//        | "final" 
//        | "transient" 
//        | "volatile"
//        )
//    
//    val VariableDeclarators = 
//    syn ( VariableDeclarator.+(',') )
//    
//    val VariableDeclarator = 
//    syn ( VariableDeclaratorId ~ ('=' ~ VariableInitializer).gr.? )
//    
//    val VariableDeclaratorId = 
//    syn ( Identifier ~ ('[' ~ ']').gr.* )
//    
//    val VariableInitializer = 
//    syn ( ArrayInitializer 
//        | Expression 
//        )
//    
//    val ArrayInitializer = 
//    syn ( '{' ~ VariableInitializer.*(terminal(',')) ~ ','.? ~ '}' )
//    
//    val MethodDeclaration = 
//    syn ( MethodHeader ~ MethodBody )
//    
//    val MethodHeader = 
//    syn ( MethodModifier.* ~ TypeParameters.? ~ Result ~ MethodDeclarator ~ Throws.? )
//    
//    val MethodDeclarator = 
//    syn ( Identifier ~ '(' ~ FormalParameterList.? ~ ')' 
//        | MethodDeclarator ~ '[' ~ ']'
//        )
//    
//    val FormalParameterList = 
//    syn ( (FormalParameter ~ ',').gr.* ~ LastFormalParameter )
//    
//    val FormalParameter =
//    syn ( VariableModifier.* ~ Type ~ VariableDeclaratorId )
//    
//    val VariableModifier =
//    syn ( "final" 
//        | Annotation
//        )
//    
//    val LastFormalParameter = 
//    syn ( VariableModifier.* ~ Type ~ "..." ~ VariableDeclaratorId
//        | FormalParameter
//        )
//    
//    val MethodModifier =  
//    syn ( Annotation 
//        | "public" 
//        | "protected" 
//        | "private" 
//        | "abstract" 
//        | "static" 
//        | "final" 
//        | "synchronized" 
//        | "native" 
//        | "strictfp"
//        )
//    
//    val Result = 
//    syn ( Type 
//        | "void"
//        )
//    
//    val Throws = 
//    syn ( "throws" ~ ExceptionType.+(',') )
//    
//    val ExceptionType = 
//    syn ( TypeName )
//    
//    val MethodBody =
//    syn ( Block 
//        | ';'
//        )
//    
//    val Annotation = 
//    syn ( '@' ~ TypeName ~ '(' ~ ElementValuePair.*(terminal(',')) ~ ')' 
//        | '@' ~ TypeName ~ ('(' ~ ElementValue ~ ')').gr.?
//        )
//    
//    val ElementValuePair = 
//    syn ( Identifier ~ '=' ~ ElementValue )
//    
//    val ElementValue = 
//    syn ( ConditionalExpression 
//        | Annotation 
//        | ElementValueArrayInitializer
//        )
//    
//    val ElementValueArrayInitializer = 
//    syn ( '{' ~ ElementValues.? ~ ','.? ~ '}' )
//    
//    val ElementValues = 
//    syn ( ElementValue.+(',') )
//    
//    val EnumDeclaration = 
//    syn ( ClassModifier.* ~ "enum" ~ Identifier ~ ("implements" ~ TypeList).gr.? ~ EnumBody )
//    
//    val EnumBody =
//    syn ( "{" ~ EnumConstant.*(terminal(',')) ~ ",".? ~ EnumBodyDeclarations.? ~ "}" )
//    
//    val EnumConstant = 
//    syn ( Annotation.* ~ Identifier ~ Arguments.? ~ ClassBody.? )
//    
//    val Arguments = 
//    syn ( "(" ~ ArgumentList.? ~ ")" )
//    
//    val EnumBodyDeclarations = 
//    syn ( ";" ~ ClassBodyDeclaration.* )
//    
//    val Block = 
//    syn ( "{" ~ BlockStatement.* ~ "}" )
//    
//    val BlockStatement = 
//    syn ( LocalVariableDeclarationStatement 
//        | ClassDeclaration 
//        | Statement
//        )
//    
//    val LocalVariableDeclarationStatement = 
//    syn ( VariableModifier.* ~ Type ~ VariableDeclarators ~ ";" )
//    
//    val Statement = 
//    syn ( StatementWithoutTrailingSubstatement 
//        | Identifier ~ ":" ~ Statement 
//        | "if" ~ "(" ~ Expression ~ ")" ~ Statement 
//        | "if" ~ "(" ~ Expression ~ ")" ~ StatementNoShortIf ~ "else" ~ Statement 
//        | "while" ~ "(" ~ Expression ~ ")" ~ Statement 
//        | ForStatement
//        )
//    
//    val StatementWithoutTrailingSubstatement = 
//    syn ( Block 
//        | ";" 
//        | StatementExpression ~ ";" 
//        | "assert" ~ Expression ~ (":" ~ Expression).gr.? ~ ";" 
//        | "switch" ~ "(" ~ Expression ~ ")" ~ "{" ~ SwitchBlockStatementGroup.* ~ SwitchLabel.* ~ "}" 
//        | "do" ~ Statement ~ "while" ~ "(" ~ Expression ~ ")" ~ ";" 
//        | "break" ~ Identifier.? ~ ";" 
//        | "continue" ~ Identifier.? ~ ";" 
//        | "return" ~ Expression.? ~ ";" 
//        | "synchronized" ~ "(" ~ Expression ~ ")" ~ Block 
//        | "throw" ~ Expression ~ ";" 
//        | "try" ~ Block ~ (CatchClause.+ | (CatchClause.* ~ Finally).gr).gr 
//        | "try" ~ ResourceSpecification ~ Block ~ CatchClause.* ~ Finally.?
//        )
//    
//    val StatementNoShortIf = 
//    syn ( StatementWithoutTrailingSubstatement 
//        | Identifier ~ ":" ~ StatementNoShortIf 
//        | "if" ~ "(" ~ Expression ~ ")" ~ StatementNoShortIf ~ "else" ~ StatementNoShortIf 
//        | "while" ~ "(" ~ Expression ~ ")" ~ StatementNoShortIf 
//        | "for" ~ "(" ~ ForInit.? ~ ";" ~ Expression.? ~ ";" ~ ForUpdate.? ~ ")" ~ StatementNoShortIf
//        )
//    
//    val ForStatement = 
//    syn ( "for" ~ "(" ~ ForInit.? ~ ";" ~ Expression.? ~ ";" ~ ForUpdate.? ~ ")" ~ Statement 
//        | "for" ~ "(" ~ FormalParameter ~ ":" ~ Expression ~ ")" ~ Statement
//        )
//    
//    val StatementExpression =
//    syn ( Assignment 
//        | PreIncrementExpression 
//        | PreDecrementExpression 
//        | PostIncrementExpression 
//        | PostDecrementExpression 
//        | MethodInvocation
//        | ClassInstanceCreationExpression
//        )
//    
//    val CatchClause = 
//    syn ( "catch" ~ "(" ~ VariableModifier.* ~ CatchType ~ Identifier ~ ")" ~ Block )
//    
//    val CatchType = 
//    syn ( QualifiedIdentifier.+("|") )
//    
//    val Finally = 
//    syn ( "finally" ~ Block )
//    
//    val ResourceSpecification = 
//    syn ( "(" ~ Resources ~ ";".? ~ ")" )
//    
//    val Resources = 
//    syn ( Resource.+(";") )
//    
//    val Resource = 
//    syn ( VariableModifier.* ~ ReferenceType ~ VariableDeclaratorId ~ "=" ~ Expression )
//    
//    val SwitchBlockStatementGroup = 
//    syn ( SwitchLabel.+ ~ BlockStatement.+ )
//    
//    val SwitchLabel = 
//    syn ( "case" ~ ConstantExpression ~ ":" 
//        | "default" ~ ":"
//        )
//    
//    val LocalVariableDeclaration =  
//    syn ( VariableModifier.* ~ Type ~ VariableDeclarator.+(',') )
//    
//    val ForInit = 
//    syn ( StatementExpression.+(',') 
//        | LocalVariableDeclaration
//        )
//    
//    val ForUpdate = 
//    syn ( StatementExpression.+(',') )
//    
//    val Primary = 
//    syn ( PrimaryNoNewArray 
//        | ArrayCreationExpression
//        )
//    
//    val PrimaryNoNewArray =  
//    syn ( Literal 
//        | Type ~ "." ~ "class" 
//    		| "void" ~ "." ~ "class" 
//    		| "this" 
//    		| ClassName ~ "." ~ "this" 
//    		| "(" ~ Expression ~ ")" 
//    		| ClassInstanceCreationExpression 
//    		| FieldAccess 
//    		| MethodInvocation 
//    		| ArrayAccess
//    		)
//    
//    val Literal = 
//    syn ( IntegerLiteral 
//        | FloatingPointLiteral 
//        | BooleanLiteral 
//        | CharacterLiteral 
//        | StringLiteral 
//        | NullLiteral
//        )
//    
//    val IntegerLiteral = 
//    syn ( DecimalIntegerLiteral.!>>('.') 
//        | HexIntegerLiteral.!>>('.') 
//        | OctalIntegerLiteral 
//        | BinaryIntegerLiteral
//        )
//    
//    val FloatingPointLiteral = 
//    syn ( DecimalFloatingPointLiteral 
//        | HexadecimalFloatingPointLiteral
//        )
//    
//    val ClassInstanceCreationExpression = 
//    syn ( "new" ~ TypeArguments.? ~ TypeDeclSpecifier ~ TypeArgumentsOrDiamond.? ~ "(" ~ ArgumentList.? ~ ")" ~ ClassBody.? 
//        | (Primary | QualifiedIdentifier).gr ~ "." ~ "new" ~ TypeArguments.? ~ Identifier ~ TypeArgumentsOrDiamond.? ~ "(" ~ ArgumentList.? ~ ")" ~ ClassBody.?
//        )
//    
//    val TypeArgumentsOrDiamond = 
//    syn ( "<" ~ ">" 
//        | TypeArguments
//        )
//    
//    val ArgumentList =
//    syn (Expression.+(','))
//    
//    val ArrayCreationExpression = 
//    syn ( "new" ~ (PrimitiveType | ReferenceType).gr ~ DimExpr.+ ~ ("[" ~ "]").gr.* 
//        | "new" ~ (PrimitiveType | ReferenceTypeNonArrayType).gr ~ ("[" ~ "]").gr.+ ~ ArrayInitializer
//        )
//    
//    val DimExpr = 
//    syn ("[" ~ Expression ~ "]")
//    
//    val FieldAccess = 
//    syn ( Primary ~ "." ~ Identifier 
//        | "super" ~ "." ~ Identifier 
//        | ClassName ~ "." ~ "super" ~ "." ~ Identifier
//        )
//    
//    val MethodInvocation = 
//    syn ( MethodName ~ "(" ~ ArgumentList.? ~ ")" 
//        | Primary ~ "." ~ NonWildTypeArguments.? ~ Identifier ~ "(" ~ ArgumentList.? ~ ")" 
//        | "super" ~ "." ~ NonWildTypeArguments.? ~ Identifier ~ "(" ~ ArgumentList.? ~ ")" 
//        | ClassName ~ "." ~ "super" ~ "." ~ NonWildTypeArguments.? ~ Identifier ~ "(" ~ ArgumentList.? ~ ")" 
//        | TypeName ~ "." ~ NonWildTypeArguments ~ Identifier ~ "(" ~ ArgumentList.? ~ ")"
//        )
//    
//    val ArrayAccess = 
//    syn ( ExpressionName ~ "[" ~ Expression ~ "]" 
//        | PrimaryNoNewArray ~ "[" ~ Expression ~ "]"
//        )
//    
//    val PostfixExpression =
//    syn ( Primary
//        | ExpressionName 
//        | PostIncrementExpression 
//        | PostDecrementExpression
//        )
//    
//    val PostIncrementExpression =
//    syn ( PostfixExpression ~ "++" )
//    
//    val PostDecrementExpression = 
//    syn ( PostfixExpression ~ "--" )
//    
//    val UnaryExpression = 
//    syn ( PreIncrementExpression 
//        | PreDecrementExpression 
//        | '+'.!>>('+') ~ UnaryExpression 
//        | '-'.!>>('-') ~ UnaryExpression 
//        | UnaryExpressionNotPlusMinus
//        )
//    
//    val PreIncrementExpression = 
//    syn ( "++" ~ UnaryExpression )
//    
//    val PreDecrementExpression = 
//    syn ( "--" ~ UnaryExpression )
//    
//    val UnaryExpressionNotPlusMinus = 
//    syn (  PostfixExpression 
//        | '~' ~ UnaryExpression 
//        | '!' ~ UnaryExpression 
//        | CastExpression
//        )
//    
//    val CastExpression = 
//    syn ( '(' ~ PrimitiveType ~ ')' ~ UnaryExpression 
//        | '(' ~ ReferenceType ~ ')' ~ UnaryExpressionNotPlusMinus
//        )
//    
//    val MultiplicativeExpression =
//    syn ( UnaryExpression 
//    	  | MultiplicativeExpression ~ '*' ~ UnaryExpression 
//    		| MultiplicativeExpression ~ '/' ~ UnaryExpression 
//    		| MultiplicativeExpression ~ '%' ~ UnaryExpression
//    		)
//    
//    val AdditiveExpression = 
//    syn ( MultiplicativeExpression 
//    	  | AdditiveExpression ~ '+'.!>>('+') ~ MultiplicativeExpression 
//    		| AdditiveExpression ~ '-'.!>>('-') ~ MultiplicativeExpression
//    		)
//    
//    val ShiftExpression = 
//    syn ( AdditiveExpression 
//    		| ShiftExpression ~ "<<" ~ AdditiveExpression 
//    		| ShiftExpression ~ ">>" ~ AdditiveExpression 
//    		| ShiftExpression ~ ">>>" ~ AdditiveExpression
//    	  )
//    
//    val RelationalExpression = 
//    syn ( ShiftExpression 
//    		| RelationalExpression ~ "<" ~ ShiftExpression 
//    		| RelationalExpression ~ ">" ~ ShiftExpression 
//    		| RelationalExpression ~ "<=" ~ ShiftExpression 
//    		| RelationalExpression ~ ">=" ~ ShiftExpression 
//    		| RelationalExpression ~ "instanceof" ~ ReferenceType
//    		)
//    
//    val EqualityExpression = syn
//        ( RelationalExpression 
//        | EqualityExpression ~ "==" ~ RelationalExpression 
//      	| EqualityExpression ~ "!=" ~ RelationalExpression
//      	)
//    
//    val AndExpression = syn
//        ( EqualityExpression 
//    	  | AndExpression ~ '&' ~ EqualityExpression
//    	  )
//    
//    val ExclusiveOrExpression = 
//    syn ( AndExpression 
//    	  | ExclusiveOrExpression ~ '^' ~ AndExpression
//    	  )
//    
//    val InclusiveOrExpression = 
//    syn ( ExclusiveOrExpression 
//    	  | InclusiveOrExpression ~ '|' ~ ExclusiveOrExpression
//    	  )
//    
//    val ConditionalAndExpression = 
//    syn ( InclusiveOrExpression 
//    	  | ConditionalAndExpression ~ "&&" ~ InclusiveOrExpression
//    	  )
//    
//    val ConditionalOrExpression = 
//    syn ( ConditionalAndExpression 
//    	  | ConditionalOrExpression ~ "||" ~ ConditionalAndExpression
//    		)
//    
//    val ConditionalExpression = 
//    syn ( ConditionalOrExpression 
//  		  | ConditionalOrExpression ~ '?' ~ Expression ~ ':' ~ ConditionalExpression
//    	  )
//    
//    val AssignmentExpression = 
//    syn ( ConditionalExpression 
//    	  | Assignment
//    	  )
//    
//    val Assignment = 
//    syn ( LeftHandSide ~ AssignmentOperator ~ AssignmentExpression )
//    
//    val LeftHandSide = 
//    syn ( ExpressionName 
//    		| '(' ~ LeftHandSide ~ ')' 
//    		| FieldAccess 
//    		| ArrayAccess
//    		)
//    
//    val AssignmentOperator = 
//    syn ( "=" 
//        | "+=" 
//        | "-=" 
//        | "*=" 
//        | "/=" 
//        | "&=" 
//        | "|=" 
//        | "^=" 
//        | "%=" 
//        | "<<=" 
//        | ">>=" 
//        | ">>>="
//        )
//    
//    val Expression = 
//    syn ( AssignmentExpression )
//    
//    val ConstantExpression = 
//    syn ( Expression )
//    
//    val ClassName = 
//    syn ( QualifiedIdentifier )
//    
//    val ExpressionName = 
//    syn ( QualifiedIdentifier )
//    
//    val MethodName = 
//    syn ( QualifiedIdentifier )
//    
//    val TypeDeclSpecifier = 
//    syn ( Identifier ~ (TypeArguments.? ~ '.' ~ Identifier).gr.* )
//    
//    val SuperSuffix = 
//    syn ( Arguments 
//        | '.' ~ Identifier ~ Arguments.?
//        )
//    
//    val ExplicitGenericInvocationSuffix = 
//    syn ( "super" ~ SuperSuffix 
//        | Identifier ~ Arguments
//        )
//  
////    
////    
////  import org.meerkat.util.Configuration._
////  
////  def main(args: Array[String]) {
////     val input = scala.io.Source.fromFile("test-files/test.java").mkString              
////     parse(input, start(CompilationUnit), ALL_PARSES, TESTING)
////  }
//  
//}
