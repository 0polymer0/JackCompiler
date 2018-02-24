# JackCompiler
Jack Compiler in Haskell

The Jack programming language is a Java like, context free grammer,
used to teach students how to implement compilers. It is introduced in 
"Elements of Computing Systems" by Noam Nisan and Shimon Schocken. As an
experiment I wanted to see how difficult it was to implement this in Haskell.

Here is the grammer for the language:
```
Lexical elements:
keyword:  'class' | 'constructor' | 'function' | 'method' | 'field' |
          'method'| 'field' | 'static' | 'var' | 'int' | 'char' |
          'boolean' | 'void' | 'true' | 'false' | 'null' | 'this' |
          'let' | 'do' | 'if' | 'else' | 'while' | 'return

symbol:   '{' | '}' | '(' | ')' | '[' | ']' | '.' | ',' | ';' | '+' |
          '-' | '*' | '/' | '&' | '|' | '<' | '>' | '=' | '~'

integerConstant: 0 ... 32767

StringConstant: '"' (Unicode)* '"'

identifier: (alpha|'_')(alpha|digit|'_')*

Program Structure:

class:  'class' className '{' classVarDec* subroutineDec* '}'
classVarDec:  ('static' | 'field') type varName (',' varName)* ';'
type: 'int' | 'char' | 'boolean' | className
subroutineDec:  ('constructor' | 'function' | 'method') ('void' | type) subroutineName '(' parameterList ')' subroutineBody
parameterList:  ((type varName) (',' type varName)*)?
subroutineBody:  '{' varDec* statements '}'
varDec:  'var' type varName (',' varName)* ';'
className:  identifier
subroutineName:  identifier
varName:  identifier

Statements:

statement:  letStatement | ifStatement | whileStatement | doStatement | returnStatement
letStatement:  'let' varName ('[' expression ']')? '=' expression ';'
ifStatement:  'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
whileStatement:  'while' '(' expression ')' '{' statements '}'
doStatement:  'do' subroutineCall ';'
ReturnStatement: 'return' expression? ';'

Expressions:

expression:  term (op term)*
term:  integerConstant | stringConstant | keywordConstant | varName |
       varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
subroutineCall:  subroutineName '(' expressionList ')' | (className | varName) '.' subroutineName '(' expressionList ')'
expressionList:  (expression (',' expression)* )?
op:  '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
unaryOp:  '-' | '~'
KeywordConstant:  'true' | 'false' | 'null' | 'this'
```