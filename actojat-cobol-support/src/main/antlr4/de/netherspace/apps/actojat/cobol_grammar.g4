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

paragraph               : ID DOT sentence+
                        ;

sentence                : statement+ DOT
                        ;

sectionend              : SECTIONNAME DOT EXIT PROGRAM DOT
                        | SECTIONNAME DOT EXIT DOT
                        ;

sectiondecl             : SECTIONNAME SECTION
                        ;

block                   : STARTSECT DOT
//                      | STARTSECT statements DOT
                        ;

statement               : performtimes
                        | performuntil
                        | performvarying
                        | performsinglefunction
                        | displayvalue
                        | stopoperation
                        //: operation operand+
                        ;

performtimes            : PERFORM blockname counter TIMES // ID = FunctionName
                        ;

blockname               : ID
                        ;

counter                 : (NUMBER | ID)
                        ;

performuntil            : PERFORM ID UNTIL condition // ID = FunctionName
                        ;

performvarying          : PERFORM ID VARYING (NUMBER | ID) FROM // ID = FunctionName
                        ;

performsinglefunction   : PERFORM ID // ID = FunctionName
                        ;

condition               : PLACEHOLDER // TODO!
                        ;

displayvalue            : DISPLAY STRINGVALUE+
                        ;

stopoperation           : STOP RUN
                        ;

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

DISPLAY                 : 'DISPLAY'
                        ;

PERFORM                 : 'PERFORM'
                        ;

COMPUTE                 : 'COMPUTE'
                        ;

VARYING                 : 'VARYING'
                        ;

AUTHOR                  : 'AUTHOR'
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

EXIT                    : 'EXIT'
                        ;

DATA                    : 'DATA'
                        ;

STOP                    : 'STOP'
                        ;

FILE                    : 'FILE'
                        ;

FROM                    : 'FROM'
                        ;

PIC                     : 'PIC'
                        ;

RUN                     : 'RUN'
                        ;

COPY                    : 'copy '
                        | 'COPY '
                        ;

IS                      : 'IS'
                        ;

MOVE                    : 'MV'
                        ;

STRINGVALUE             : QUOTATIONMARK ALLCHARS+ QUOTATIONMARK
                        ;

DOT                     : '.'
                        ;

QUOTATIONMARK           : '"'
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
PLACEHOLDER             : CHARACTER* ; // TODO: loeschen!!!!

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
