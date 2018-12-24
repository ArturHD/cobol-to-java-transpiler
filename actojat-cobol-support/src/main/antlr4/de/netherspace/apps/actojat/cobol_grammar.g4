// This context-free grammar describes a subset of the COBOL 2002 syntax.
grammar cobol_grammar;


// The grammar's start symbol:
program				//: statements
					: imports sectionlist tailingimports
					| imports sectionlist
					| sectionlist
//					| imports globalvariables sectionlist
					;



// The set of variables:
imports				: WORKINGSTORAGEKWRD SECTIONKWRD importcopyfile*
					;

sectionlist			: PROCEDUREDIVKWRD ID DOT section+
					;

importcopyfile		: IMPORTKEYWORD QUOTATIONMARK FILEID QUOTATIONMARK DOT
					;

tailingimports		: importcopyfile+
					;

section				: sectiondecl block sectionend
					;

sectionend			: SECTIONNAME DOT EXITKWRD PROGRAMKWRD DOT
					| SECTIONNAME DOT EXITKWRD DOT
					;

sectiondecl			: SECTIONNAME SECTIONKWRD
					;

block				: STARTSECTKWRD DOT
//					| STARTSECTKWRD statements DOT
					;

statements			: PLACEHOLDER+
					;

//...




// The set of terminals:
STARTSECTKWRD		: 'START-SECTION.'
					;

SECTIONKWRD			: ' SECTION.'
					;

WORKINGSTORAGEKWRD	: 'WORKING-STORAGE'
					;

PROCEDUREDIVKWRD	: 'PROCEDURE DIVISION USING'
					;

PROGRAMKWRD			: 'PROGRAM'
					;

EXITKWRD			: 'EXIT'
					;

DOT					: '.'
					;

QUOTATIONMARK		: '"'
					;

PRIMITIVETYPE		: 'int'
					| 'void'
					;
					
IMPORTKEYWORD		: 'copy '
					| 'COPY '
					;

SECTIONNAME			: DIGIT+ ('-' (CHARACTER+))+
					;

FILEID				: (ALLCHARS | DIGIT)+ DOT CHARACTER+
					;

ID					: ALLCHARS+
					;

PLACEHOLDER			: CHARACTER* ; //loeschen!!!!

WHITESPACE			: [ \t]+ -> channel(HIDDEN)
					;

LINEBREAK			: [\r\n]+ -> skip
					;



// Fragments which are part of the grammar but NOT actual terminals:
fragment DIGIT		: [0-9]
					;

fragment CHARACTER	: [a-zA-Z]
					;

fragment ALLCHARS	: ([a-zA-Z] | '-' | '/' | '_' )
					;
