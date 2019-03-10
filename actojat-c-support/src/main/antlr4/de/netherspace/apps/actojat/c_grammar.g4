// This context-free grammar describes a subset of GCC's C syntax.
grammar c_grammar;


// The grammar's start symbol:
program             //: statements
                    : functionlist
                    | imports functionlist
//                  | imports globalvariables functionlist
                    ;

// ################ The set of variables ################
imports             : importheader+
                    ;

functionlist        : functiondeclr+
                    ;

importheader        : INCLUDEKWRD OPENINGANGLEQUOTE FILEID CLOSINGANGLEQUOTE
                    | INCLUDEKWRD QUOTATIONMARK FILEID QUOTATIONMARK
                    ;

//TODO: recognize non-primitive and n-dimensional types!
functiondeclr       : PRIMITIVETYPE ID OPENINGPARENTHESIS parameterlist CLOSINGPARENTHESIS block
//                  | NDIMTYPE ID OPENINGPARENTHESIS parameterlist CLOSINGPARENTHESIS block
                    ;

block               : OPENINGCURLYBRACKET expressionlist CLOSINGCURLYBRACKET
                    ;

expression          : lhs ASSIGNMENTOP rhs SEMICOLON
                    | functioncall SEMICOLON
                    ;

functioncall        : ID OPENINGPARENTHESIS argumentlist CLOSINGPARENTHESIS
                    | ID OPENINGPARENTHESIS CLOSINGPARENTHESIS
                    ;

expressionlist      : expression*
                    ;

parameterlist       : (PRIMITIVETYPE ID)*
                    ;

argumentlist        : argument (COMMA argument)+
                    | argument
                    ;

argument            : STRING //e.g. bla("Hello World");
//                  | ID //e.g. blubb(tigerente);
                    ;

lhs                 : ID
                    ;

rhs                 : ID operand ID
//TODO: composite expressions!
                    | ID
                    ;

operand             : PLUSSIGN
                    | MINUSSIGN
                    | ASTERISK
                    | SLASH
                    ;


// ################ The set of terminals ################

WHITESPACE          : [ \t]+ -> channel(HIDDEN)
                    ;

LINEBREAK           : [\r\n]+ -> skip
                    ;

DOT                 : '.'
                    ;

COMMA               : ','
                    ;

QUOTATIONMARK       : '"'
                    ;

SINGLEQUOTE         : '\''
                    ;

OPENINGANGLEQUOTE   : '<'
                    ;

CLOSINGANGLEQUOTE   : '>'
                    ;

OPENINGPARENTHESIS  : '('
                    ;

CLOSINGPARENTHESIS  : ')'
                    ;

OPENINGBRACKET      : '['
                    ;

CLOSINGBRACKET      : ']'
                    ;

OPENINGCURLYBRACKET : '{'
                    ;

CLOSINGCURLYBRACKET : '}'
                    ;

ASSIGNMENTOP        : '='
                    ;

SEMICOLON           : ';'
                    ;

MINUSSIGN           : '-'
                    ;

PLUSSIGN            : '+'
                    ;

ASTERISK            : '*'
                    ;

SLASH               : '/'
                    ;

PRIMITIVETYPE       : 'int'
                    | 'void'
                    ;

INCLUDEKWRD         : '#include'
                    ;

FILEID              : (ALLCHARS | DIGIT)+ DOT CHARACTER+
                    ;

STRING              : QUOTATIONMARK (CHARACTER | DIGIT | WHITESPACE)+ QUOTATIONMARK
                    ;

ID                  : CHARACTER+
                    ;



// fragments, which are part of the grammar but NOT actual terminals:
fragment DIGIT      : [0-9]
                    ;

fragment CHARACTER  : [a-zA-Z]
                    ;

fragment ALLCHARS   : ([a-zA-Z] | '-' | '/' | '_' )
                    ;
