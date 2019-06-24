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

performuntil            : PERFORM ID WITH TEST BEFORE UNTIL condition
                        | PERFORM ID WITH TEST AFTER UNTIL condition
                        | PERFORM ID UNTIL condition
                        ;

performvarying          : PERFORM ID VARYING (NUMBER | ID) FROM
                        ;

performsinglefunction   : PERFORM ID
                        ;

condition               : ID comparisonoperator NUMBER // TODO: fix this! it's just a placeholder!
                        ;

comparisonoperator      : NOT LESS THAN
                        | NOT GREATER THAN
                        | LESS THAN
                        | GREATER THAN
                        | LESSEROREQUAL
                        | GREATEROREQUAL
                        | EQUAL
                        | LESSERSIGN
                        | GREATERSIGN
                        ;

displayvalue            : DISPLAY STRINGVALUE+
                        ;

stopoperation           : STOP RUN
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

LESS                    : 'LESS'
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

STRINGVALUE             : QUOTATIONMARK ALLCHARS+ QUOTATIONMARK
                        ;

EQUAL                   : '=='
                        ;

LESSEROREQUAL           : '<='
                        ;

GREATEROREQUAL          : '=>'
                        ;

DOT                     : '.'
                        ;

QUOTATIONMARK           : '"'
                        ;

LESSERSIGN              : '<'
                        ;

GREATERSIGN             : '>'
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
