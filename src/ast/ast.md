# トレイト
## Prog: Node + Statement
* プログラムをパースしたときに一番でかいやつ？
* box_clone_prog <- 本当にひどい

## Exp: Node + Expression
* Node+Expressionを実装してるのが多くて、これをまとめるため
* box_clone_exp <- 本当にひどい

# Node
* みんな実装してる。
* fn token_literal(&self) -> String;
* fn string(&self) -> String;

# Statement
* 文
* LetStatement, ReturnStatement, ExpressionStatementが実装.
* monkeyは文(Statemetn)と式(Expression)と思いきや、式文もある
* fn as_any(&self) -> &dyn Any <- 本当にひどい

# Expression
* 式
* monkeyはほとんどが式か式文
* 殆どが実装
* fn as_any(&self) -> &dyn Any;  <- 本当にひどい
* fn box_clone_expression(&self) -> Box<dyn Expression>; <- 本当にひどい

# AST
## LetStatement
* トレイト: Prog Node Statement
* メンバー: Token Identifier expression(trait Exp)

## ReturnStatement
* トレイト: Prog Node Statement
* メンバー: Token return_value(trait Exp)

## ExpressionStatement
* トレイト: Prog Statement Node
* メンバー: Token expression(trait Exp)

## PrefixExpression
* トレイト: Exp Node Expression
* メンバー: Token Operator(Token) right(trait Exp)

## InfixExpression
* トレイト: Exp Node Expression
* メンバー: Token, Operator(Token), Left(trait Exp) Right(trait Exp)

## IntegerLiteral
* トレイト: Exp Node Expression
* メンバー: Token

# Identifier
* トレイト: Exp Node Expression
* メンバー: Token

## Boolean
* トレイト: Exp Node Expression
* メンバー: Token

## IfExpression
* トレイト: Exp Node Expression
* メンバー: Token, Condition(Exp) Consequence(BlockStatement) Alternative(BlockStatement)

## BlockStatement
* トレイト: Exp Node Expression
* メンバー: Token Statements(Vec<Box<dyn Prog>>)

## FunctionLiteral
* トレイト: Exp Node Expression
* メンバー: Token, Parameters(Vec<Box<dyn Exp>>) Body(BlockStatement)

## CallExpression
* トレイト: Exp Node Expression
* メンバー: Token, Function(trait Exp) arguments(Vec<Box<dyn Exp>>)
