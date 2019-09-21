// This context-free grammar describes a subset of the COBOL85 syntax.
grammar cobol_grammar;


// The grammar's start symbol:
program                 : division+
                        ;



// ################ The set of variables ################

division                : identificationdivision
                        | environmentdivision
                        | datadivision
                        | proceduredivision
                        ;


// "IDENTIFICATION DIVISION."
identificationdivision  : IDENTIFICATION DIVISION DOT identstatements*
                        | IDENTIFICATION DIVISION DOT
                        ;

identstatements         : programidstatement
                        | authorstatement
                        | datewrittenstatement
                        ;

programidstatement      : PROGRAMID DOT ID DOT
                        ;

authorstatement         : AUTHOR DOT ID DOT
                        ;

datewrittenstatement    : DATEWRITTEN DOT ID DOT
                        ;


// "ENVIRONMENT DIVISION."
environmentdivision     : ENVIRONMENT DIVISION DOT configurationsection inputoutputsection
                        | ENVIRONMENT DIVISION DOT inputoutputsection configurationsection
                        | ENVIRONMENT DIVISION DOT configurationsection
                        | ENVIRONMENT DIVISION DOT inputoutputsection
                        | ENVIRONMENT DIVISION DOT
                        ;

configurationsection    : SPECIALNAMES specialnamesparagraph
                        ;

inputoutputsection      : FILECONTROL filecontrolparagraph
                        ;

specialnamesparagraph   : decimalpointspec
                        | symboliccharsspec
                        ;

decimalpointspec        : DECIMALPOINT IS ID DOT
                        ;

symboliccharsspec       : SYMBOLIC CHARACTERS ID* DOT
                        ;

filecontrolparagraph    : 'SELECT ... TO'
                        ; // TODO: fix this rule!


// "DATA DIVISION."
datadivision            : DATA DIVISION DOT (filesection | workingstoragesection | linkagesection | reportsection)+
                        | DATA DIVISION DOT
                        ; // TODO: the above rule is not the right one!

filesection             : FILE SECTION DOT
                        // TODO: "FD ..."
                        // TODO: "COPY ..."
                        ;

workingstoragesection   //: WORKINGSTORAGE SECTION DOT (datadecl | importcopyfile)+
                        : WORKINGSTORAGE SECTION DOT (importcopyfile)+
                        | WORKINGSTORAGE SECTION DOT (datadeclaration)+ // TODO: data decl. AND imports possible!
                        | WORKINGSTORAGE SECTION DOT
                        ; // TODO: fix the (A|B)+ rule!

linkagesection          : LINKAGE SECTION DOT
                        // TODO: ...
                        ;

reportsection           : REPORT SECTION DOT
                        // TODO: ...
                        ;

importcopyfile          : COPY QUOTATIONMARK FILEID QUOTATIONMARK DOT
                        ;

// TODO: are tailing imports parts of the COBOL85 standard or a MF COBOL gimmick?
tailingimports          : importcopyfile+
                        ;

datadeclaration         : datahierarchylevel ID datatype
                        ;

datahierarchylevel      : NUMBER
                        ;

datatype                : PIC size VALUE initialvalue DOT
                        | PIC size DOT // TODO: is this valid COBOL?
                        ;

size                    : NUMBER
                        ;

initialvalue            : NUMBER
                        ;

// "PROCEDURE DIVISION."
proceduredivision       : PROCEDURE DIVISION USING ID DOT (paragraph | section)+
                        | PROCEDURE DIVISION DOT (paragraph | section)+
                        ;

section                 : sectiondecl paragraph+
                        ;

paragraph               : ID DOT sentence+  // TODO: can both statements and sentences appear in one block?
                        ;

statements              : statement+
                        ;

sentence                : statements DOT
                        ;

sectionend              : SECTIONNAME DOT EXIT PROGRAM DOT
                        | SECTIONNAME DOT EXIT DOT
                        ;

sectiondecl             : SECTIONNAME SECTION
                        ;

statement               : performtimes
                        | performuntil
                        | performvarying
                        | performsinglefunction
                        | displayvalue
                        | stopoperation
                        | ifthenelse
                        //: operation operand+ // TODO: erase? not needed?
                        ;

performtimes            : PERFORM counter TIMES statementsorsentences ENDPERFORM
                        | PERFORM blockname counter TIMES
                        ;

blockname               : ID
                        ;

counter                 : (NUMBER | ID)
                        ;

performuntil            // TODO: multiple blockname calls, but not as inline body:
                        //: PERFORM throughblocknamelist WITH TEST BEFORE UNTIL condition
                        //| PERFORM throughblocknamelist WITH TEST AFTER UNTIL condition
                        //| PERFORM throughblocknamelist UNTIL condition
                        : PERFORM blockname WITH TEST BEFORE UNTIL condition // TODO: -: +| !
                        | PERFORM blockname WITH TEST AFTER UNTIL condition
                        | PERFORM blockname UNTIL condition
                        // TODO: no blockname given, but an inline body instead:
                        //| PERFORM WITH TEST BEFORE UNTIL condition statementsorsentences ENDPERFORM
                        //| PERFORM WITH TEST AFTER UNTIL condition statementsorsentences ENDPERFORM
                        //| PERFORM UNTIL condition statementsorsentences ENDPERFORM
                        ;

throughblocknamelist    : ((THRU | THROUGH) blockname)+
                        ;

performvarying          : PERFORM ID VARYING (NUMBER | ID) FROM
                        ;

performsinglefunction   : PERFORM ID
                        ;

stopoperation           : STOP RUN
                        ;

displayvalue            : DISPLAY STRINGVALUE+
                        ;

ifthenelse              : IF condition thenblock elseblock ENDIF
                        | IF condition thenblock ENDIF
                        ;

thenblock               : THEN statementsorsentences
                        ;

elseblock               : ELSE statementsorsentences
                        ;

statementsorsentences   : (statements | sentence+) // TODO: can both statements and sentences appear in one block?
                        ;

condition               : relationcondition
                        // TODO: | classcondition
                        // TODO: | signcondition
                        ;

relationcondition       : compval IS* comparisonoperator compval
                        ;

compval                 : arithmeticexpression
                        | NUMBER
                        | ID
                        ;

arithmeticexpression    : OPENINGPARENTHESIS (NUMBER | ID) arithmeticoperator compval CLOSINGPARENTHESIS
                        | (NUMBER | ID) arithmeticoperator compval
                        // TODO: this rules is not corecct:
                        // TODO: a) right now, no expressions (apart from mere numbers and IDs) are allowed
                        // TODO:    on the left hand side. This is due to a left-recursion problem and will
                        // TODO:    probably require some non-trivial tweeking of the grammer (i.e. the
                        // TODO:    condition rules specificly)
                        // TODO: b) the way paranthesis are handled is faulty: right now, one can only place
                        // TODO:    exactly one opening- and closing paranthesis or none at all. But paranthesis
                        // TODO:    should be handled more (((felxibble))) !
                        ;

arithmeticoperator      : <assoc=right>POWERSYMBOL // 2^3^4=2^81
                        | MULTIPLYSYMBOL
                        | DIVIDESYMBOL
                        | ADDSYMBOL
                        | SUBTRACTSYMBOL
                        ;

// there's only a finite set of possible combinations of conditional keywords and literals:
comparisonoperator      : greaterthanorequalto
                        | lessthanorequalto
                        | notgreaterthan
                        | notlessthan
                        | notequalto
                        | greaterthan
                        | lessthan
                        | equalto
                        | notlessersign
                        | notgreatersign
                        | notequalsign
                        | LESSEROREQUALSIGN
                        | GREATEROREQUALSIGN
                        | LESSERSIGN
                        | GREATERSIGN
                        | EQUALSIGN
                        ;

greaterthanorequalto    : GREATER THAN OR EQUAL TO
                        ;

lessthanorequalto       : LESS THAN OR EQUAL TO
                        ;

notgreaterthan          : NOT GREATER THAN
                        ;

notlessthan             : NOT LESS THAN
                        ;

notequalto              : NOT EQUAL TO
                        ;

greaterthan             : GREATER THAN
                        ;

lessthan                : LESS THAN
                        ;

equalto                 : EQUAL TO
                        ;

notlessersign           : NOT LESSERSIGN
                        ;

notgreatersign          : NOT GREATERSIGN
                        ;

notequalsign            : NOT EQUALSIGN
                        ;

// TODO: is the generic "operation operand+" rule (see above!) unnecessary?
operation               : SUBTRACT
                        | DISPLAY
                        | PERFORM
                        | COMPUTE
                        | MOVE
                        | STOP
                        ;

operand                 : TIMES
                        | UNTIL
                        | RUN
                        | STRINGVALUE
                        | ID
                        ;


// ################ The set of terminals ################

WORKINGSTORAGE          : 'WORKING-STORAGE'
                        ;

IDENTIFICATION          : 'IDENTIFICATION'
                        ;

STARTSECT               : 'START-SECTION'
                        ;

DECIMALPOINT            : 'DECIMAL-POINT'
                        ;

SPECIALNAMES            : 'SPECIAL-NAMES'
                        ;

FILECONTROL             : 'FILE-CONTROL'
                        ;

DATEWRITTEN             : 'DATE-WRITTEN'
                        ;

ENVIRONMENT             : 'ENVIRONMENT'
                        ;

ENDPERFORM              : 'END-PERFORM'
                        ;

PROGRAMID               : 'PROGRAM-ID'
                        ;

CHARACTERS              : 'CHARACTERS'
                        ;

PROCEDURE               : 'PROCEDURE'
                        ;

SUBTRACT                : 'SUBTRACT'
                        ;

DIVISION                : 'DIVISION'
                        ;

SYMBOLIC                : 'SYMBOLIC'
                        ;

SECTION                 : 'SECTION'
                        ;

LINKAGE                 : 'LINKAGE'
                        ;

PROGRAM                 : 'PROGRAM'
                        ;

GREATER                 : 'GREATER'
                        ;

DISPLAY                 : 'DISPLAY'
                        ;

PERFORM                 : 'PERFORM'
                        ;

COMPUTE                 : 'COMPUTE'
                        ;

VARYING                 : 'VARYING'
                        ;

THROUGH                 : 'THROUGH'
                        ;

ENDIF                   : 'END-IF'
                        ;

AUTHOR                  : 'AUTHOR'
                        ;

BEFORE                  : 'BEFORE'
                        ;

REPORT                  : 'REPORT'
                        ;

USING                   : 'USING'
                        ;

UNTIL                   : 'UNTIL'
                        ;

VALUE                   : 'VALUE'
                        ;

TIMES                   : 'TIMES'
                        ;

AFTER                   : 'AFTER'
                        ;

EQUAL                   : 'EQUAL'
                        ;

LESS                    : 'LESS'
                        ;

THRU                    : 'THRU'
                        ;

THAN                    : 'THAN'
                        ;

EXIT                    : 'EXIT'
                        ;

WITH                    : 'WITH'
                        ;

DATA                    : 'DATA'
                        ;

STOP                    : 'STOP'
                        ;

ELSE                    : 'ELSE'
                        ;

FILE                    : 'FILE'
                        ;

TEST                    : 'TEST'
                        ;

THEN                    : 'THEN'
                        ;

FROM                    : 'FROM'
                        ;

PIC                     : 'PIC'
                        ;

NOT                     : 'NOT'
                        ;

RUN                     : 'RUN'
                        ;

COPY                    : 'copy ' // TODO: set this (lowercase == uppercase) in the parser implementation itself!
                        | 'COPY '
                        ;

IF                      : 'IF'
                        ;

IS                      : 'IS'
                        ;

MOVE                    : 'MV'
                        ;

OR                      : 'OR'
                        ;

TO                      : 'TO'
                        ;

STRINGVALUE             : QUOTATIONMARK (ALLCHARS | DIGIT)+ QUOTATIONMARK
                        ;

LESSEROREQUALSIGN       : '<='
                        ;

GREATEROREQUALSIGN      : '>='
                        ;

POWERSYMBOL             : '**'
                        ;

OPENINGPARENTHESIS      : '('
                        ;

CLOSINGPARENTHESIS      : ')'
                        ;

EQUALSIGN               : '='
                        ;

DOT                     : '.'
                        ;

QUOTATIONMARK           : '"'
                        ;

LESSERSIGN              : '<'
                        ;

GREATERSIGN             : '>'
                        ;

MULTIPLYSYMBOL          : '*'
                        ;

DIVIDESYMBOL            : '/'
                        ;

ADDSYMBOL               : '+'
                        ;

SUBTRACTSYMBOL          : '-'
                        ;

// identifiers are matched last:
SECTIONNAME             : DIGIT+ ('-' (CHARACTER+))+
                        ;

FILEID                  : (ALLCHARS | DIGIT)+ DOT CHARACTER+
                        ;

NUMBER                  : DIGIT+
                        ;

ID                      : ALLCHARS+
                        ;

// auxiliary terminals:
PLACEHOLDER             : CHARACTER* ; // TODO: erase!!!

WHITESPACE              : [ \t]+ -> channel(HIDDEN)
                        ;

LINEBREAK               : [\r\n]+ -> skip
                        ;



// fragments, which are part of the grammar but NOT actual terminals:
fragment DIGIT          : [0-9]
                        ;

fragment CHARACTER      : [a-zA-Z]
                        ;

fragment ALLCHARS       : ([a-zA-Z] | '-' | '/' | '_' | '!')
                        ;
