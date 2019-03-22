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

importheader        : INCLUDEKWRD LESSER FILEID GREATER
                    | INCLUDEKWRD QUOTATIONMARK FILEID QUOTATIONMARK
                    ;

//TODO: recognize non-primitive and n-dimensional types!
functiondeclr       : primitivetype ID OPENINGPARENTHESIS parameterlist CLOSINGPARENTHESIS block
//                  | NDIMTYPE ID OPENINGPARENTHESIS parameterlist CLOSINGPARENTHESIS block
                    ;

primitivetype       : (INT | VOID)
                    ;

parameterlist       : (primitivetype ID)*
                    | VOID
                    ;

block               : OPENINGCURLYBRACKET expressionlist CLOSINGCURLYBRACKET
                    ;

expressionlist      : expression*
                    ;

expression          : assignment SEMICOLON
                    | functioncall SEMICOLON
                    | returnstatement SEMICOLON
                    | forloop
                    ;

assignment          : lhs ASSIGNMENTOP rhs
                    ;

functioncall        : ID OPENINGPARENTHESIS argumentlist CLOSINGPARENTHESIS
                    | ID OPENINGPARENTHESIS CLOSINGPARENTHESIS
                    ;

returnstatement     : RETURN rhs
                    | RETURN
                    ;

forloop             : FOR OPENINGPARENTHESIS assignment SEMICOLON condition SEMICOLON incrementstatement CLOSINGPARENTHESIS block
                    | FOR OPENINGPARENTHESIS rhs SEMICOLON condition SEMICOLON incrementstatement CLOSINGPARENTHESIS block
                    ;

condition           : (NUMBER | ID) comparisonoperator (NUMBER | ID) // TODO: could be any kind of value, not only a number!
                    ;

incrementstatement  : ID '++'
                    | ID '--'
                    ;

argumentlist        : argument (COMMA argument)+
                    | argument
                    ;

argument            : STRING //e.g. bla("Hello World");
//                  | ID //e.g. blubb(tigerente);
                    ;

lhs                 : variabledecl
                    | ID
                    ;

rhs                 : (NUMBER | ID) operand (NUMBER | ID) //TODO: composite expressions!
                    | (NUMBER | ID)
                    ;

variabledecl        : primitivetype ID // TODO: allow all possible types...
                    ;

operand             : PLUSSIGN
                    | MINUSSIGN
                    | ASTERISK
                    | SLASH
                    ;

comparisonoperator  : LESSER
                    | GREATER
                    | EQUALS
                    ;


// ################ The set of terminals ################

INCLUDEKWRD         : '#include'
                    ;

RETURN              : 'return'
                    ;

VOID                : 'void'
                    ;

INT                 : 'int'
                    ;

FOR                 : 'for'
                    ;

EQUALS              : '=='
                    ;

DOT                 : '.'
                    ;

COMMA               : ','
                    ;

QUOTATIONMARK       : '"'
                    ;

SINGLEQUOTE         : '\''
                    ;

LESSER              : '<'
                    ;

GREATER             : '>'
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

// identifiers are matched last:
FILEID              : (ALLCHARS | DIGIT)+ DOT CHARACTER+
                    ;

STRING              : QUOTATIONMARK (CHARACTER | DIGIT | WHITESPACE)+ QUOTATIONMARK
                    ;

NUMBER              : DIGIT+
                    ;

ID                  : CHARACTER+
                    ;

// auxiliary terminals:

WHITESPACE          : [ \t]+ -> channel(HIDDEN)
                    ;

LINEBREAK           : [\r\n]+ -> skip
                    ;


// fragments, which are part of the grammar but NOT actual terminals:
fragment DIGIT      : [0-9]
                    ;

fragment CHARACTER  : [a-zA-Z]
                    ;

fragment ALLCHARS   : ([a-zA-Z] | '-' | '/' | '_' )
                    ;
