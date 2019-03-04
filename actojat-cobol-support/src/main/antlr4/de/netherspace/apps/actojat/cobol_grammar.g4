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
identificationdivision  : IDENTDIV DOT identstatements*
                        | IDENTDIV DOT
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
environmentdivision     : ENVIRONMENTDIV DOT configurationsection inputoutputsection
                        | ENVIRONMENTDIV DOT inputoutputsection configurationsection
                        | ENVIRONMENTDIV DOT configurationsection
                        | ENVIRONMENTDIV DOT inputoutputsection
                        | ENVIRONMENTDIV DOT
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

symboliccharsspec       : SYMBOLICCHARS ID* DOT
                        ;

filecontrolparagraph    : 'SELECT ... TO'
                        ; // TODO: fix this rule!


// "DATA DIVISION."
datadivision            : DATADIV DOT (filesection | workingstoragesection | linkagesection | reportsection)+
                        | DATADIV DOT
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

datadeclaration         : NUMBER ID PIC NUMBER VALUE NUMBER DOT // TODO: fix this rule (2nd and 3rd 'NUMBER' are placeholders only)!
                        ;


// "PROCEDURE DIVISION."
proceduredivision       : PROCEDUREDIV USING ID DOT (paragraph | section)+
                        | PROCEDUREDIV DOT (paragraph | section)+
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

statement               : operation operand+
                        ;

operation               : SUBTRACT
                        | DISPLAY
                        | PERFORM
                        | COMPUTE
                        | MOVE
                        | STOP
                        ;

operand                 : TIMES
                        | RUN
                        | STRINGVALUE
                        | ID
                        // TODO: ...
                        ;


// ################ The set of terminals ################

IDENTDIV                : 'IDENTIFICATION DIVISION'
                        ;

ENVIRONMENTDIV          : 'ENVIRONMENT DIVISION'
                        ;

SYMBOLICCHARS           : 'SYMBOLIC CHARACTERS'
                        ;

PROCEDUREDIV            : 'PROCEDURE DIVISION'
                        ;

WORKINGSTORAGE          : 'WORKING-STORAGE'
                        ;

STARTSECT               : 'START-SECTION.'
                        ;

DATADIV                 : 'DATA DIVISION'
                        ;

DECIMALPOINT            : 'DECIMAL-POINT'
                        ;

SPECIALNAMES            : 'SPECIAL-NAMES'
                        ;

FILECONTROL             : 'FILE-CONTROL'
                        ;

DATEWRITTEN             : 'DATE-WRITTEN'
                        ;

PROGRAMID               : 'PROGRAM-ID'
                        ;

SUBTRACT                : 'SUBTRACT'
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

AUTHOR                  : 'AUTHOR'
                        ;

REPORT                  : 'REPORT'
                        ;

USING                   : 'USING'
                        ;

VALUE                   : 'VALUE'
                        ;

TIMES                   : 'TIMES'
                        ;

EXIT                    : 'EXIT'
                        ;

STOP                    : 'STOP'
                        ;

FILE                    : 'FILE'
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
