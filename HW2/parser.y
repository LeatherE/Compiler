%{

#include <stdio.h>
#include <stdlib.h>

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */

%}

%token OPENPARE   /*(*/
%token CLOSEPARE  /*)*/
%token COMMA
%token SEMICOLON
%token OPENBRA    /*[*/
%token CLOSEBRA   /*]*/
%token OPENBBRA   /*{*/
%token CLOSEBBRA  /*}*/
%token ASSIGN
%token READ PRINT
%token BOOLEAN
%token WHILE DO FOR
%token IF ELSE
%token TRUE FALSE
%token CONST
%token BOOL
%token VOID
%token FLOAT INT DOUBLE STRING
%token CONTINUE BREAK
%token RETURN

%token INTEGERNUM
%token FLOATNUM
%token SCIENTIFICNUM
%token ASTRING
%token ID

%token PLUS   /* +  */
%token MINUS  /* -  */
%token MULTI  /* *  */
%token DIV    /* /  */
%token MOD    /* %  */
%token LESS   /* <  */
%token LESSEQ /* <= */
%token NOTEQ  /* != */
%token LARGE  /* >  */
%token LARGEEQ/* >  */
%token EQ     /* == */
%token ANDAND /* && */
%token OROR   /* || */
%token EXCLAM /* !  */

%left OROR
%left ANDAND
%left EXCLAM
%left LESS
%left LARGE
%left LESSEQ
%left LARGEEQ
%left EQ
%left NOTEQ
%left PLUS
%left MINUS
%left MULTI
%left DIV
%left MOD
%right ASSIGN

%left HIGH_P

%%

program : declaration_list func_def decl_and_def_list
	      ;

decl_and_def_list	: decl_and_def_list func_declaration
			            | decl_and_def_list var_declaration
			            | decl_and_def_list const_declaration
			            | decl_and_def_list func_def
                  |
			            ;

declaration_list : declaration_list const_declaration
                 | declaration_list var_declaration
                 | declaration_list func_declaration
                 | 
                 ;
                 
func_declaration : type ID OPENPARE argument_list CLOSEPARE SEMICOLON
           | procedure_declaration
           ;

procedure_declaration : VOID ID OPENPARE argument_list CLOSEPARE SEMICOLON
               ;

func_def : type ID OPENPARE argument_list CLOSEPARE compound
           | procedure_def
           ;
           
procedure_def : VOID ID OPENPARE argument_list CLOSEPARE compound
                     ;
                     
argument_list : nonEmptyArgumentList
              |
              ;
nonEmptyArgumentList : nonEmptyArgumentList COMMA type id_no_ini
                     | type id_no_ini
                     ;
               
type : INT
     | float_type
     | STRING
     | bool_type
     ; 
     
float_type : FLOAT
           | DOUBLE
           ;
           
bool_type : BOOL
          | BOOLEAN
          ;
                      
var_declaration : type id_list SEMICOLON
         ;
         
id_list : id_list COMMA id
        | id
        ;
        
id : id_no_ini
   | id_with_ini
   ;

        
id_no_ini : ID
          | ID array
          ;
          
id_with_ini : ID ASSIGN expr
            | ID array ASSIGN ini_array
            ; 
   
ini_array : OPENBBRA expr_list CLOSEBBRA
          ;
array : array OPENBRA INTEGERNUM CLOSEBRA
      | OPENBRA INTEGERNUM CLOSEBRA
      ;
      
const_declaration : CONST type const_list SEMICOLON
           ;

const_list : const_list COMMA const
           | const
           ;

const : ID ASSIGN literal_const
      ;

literal_const : INTEGERNUM
              | ASTRING
              | FLOATNUM
              | SCIENTIFICNUM
              | TRUE
              | FALSE
                 ;

statement : compound 
          | simple
          | conditional
          | while
          | for
          | jump
          ;

compound : OPENBBRA compound_content CLOSEBBRA
         ;

compound_content : compound_content const_declaration
                 | compound_content var_declaration
                 | compound_content statement
                 |
                 ;
                 
simple : simple_content SEMICOLON
       ;


simple_content : variable_reference ASSIGN expr
               | PRINT expr
               | READ variable_reference
               | expr
               ;
               
variable_reference : ID 
                   | array_reference
                   ;

array_reference : ID array_reference_square
                ;

array_reference_square : array_reference_square OPENBRA expr CLOSEBRA
                     | OPENBRA expr CLOSEBRA
                     ;
                     
expr : expr OROR expr
     | expr ANDAND expr
     | expr LARGE expr
     | expr LARGEEQ expr
     | expr LESS expr
     | expr LESSEQ expr
     | expr EQ expr
     | expr NOTEQ expr
     | expr PLUS expr
     | expr MINUS expr
     | expr MOD expr
     | expr MULTI expr
     | expr DIV expr           
     | EXCLAM expr
     | MINUS expr %prec MULTI
     | OPENPARE expr CLOSEPARE %prec MULTI
     | literal_const
     | variable_reference
     | function_invocation
     ;
           
function_invocation : ID OPENPARE expr_list CLOSEPARE
                    ;
                    
expr_list : nonEmptyexprList
          |
          ;

nonEmptyexprList : nonEmptyexprList COMMA expr
                 | expr
                 ;
                     
conditional : IF OPENPARE expr CLOSEPARE compound ELSE compound 
            | IF OPENPARE expr CLOSEPARE compound
            ;
            
while : WHILE OPENPARE expr CLOSEPARE compound
      | DO compound WHILE OPENPARE expr CLOSEPARE SEMICOLON
      ;

for : FOR OPENPARE ini_col_inc_expr SEMICOLON ini_col_inc_expr SEMICOLON ini_col_inc_expr CLOSEPARE compound
    ;

ini_col_inc_expr : ID ASSIGN expr
                       | expr
                       ;

jump : RETURN expr SEMICOLON
     | BREAK SEMICOLON
     | CONTINUE SEMICOLON
     ;
     
%%

int yyerror( char *msg )
{
  fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
  fprintf( stderr, "|--------------------------------------------------------------------------\n" );
  exit(-1);
}
int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}
	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();
 
	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}
