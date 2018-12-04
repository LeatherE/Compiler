%{
#define LIST { strncat(buf, yytext, sizeof(buf) - strlen(buf) - 1); }
#define token(t) { LIST; if (Opt_Token) printf("<%s>\n", #t); }
#define tokenString(t, s) { LIST; if (Opt_Token) printf("<%s:%s>\n", t, s); }
#define MAX_LINE_LENGTH 257
#define MAX_ID_LENGTH 257
int Opt_Source = 1;
int Opt_Token = 1;
int Opt_Statistic = 1;
int linenum = 1;
char buf[MAX_LINE_LENGTH];
char str_temp[MAX_LINE_LENGTH];
void print();

char Cstor[257][257];
int  Nstor[257] = {0};
int   count = 0;
%}

%s PRAGMA_COMMET
%s COMMENT

digit         [0-9]
nonzero_digit [1-9]
letter        [a-zA-Z]
space         [ \t]
double_quote  [\"]

%%






"(" {
tokenString("delim", "(");
}
")" {
tokenString("delim", ")");
}
"," {
tokenString("delim", ",");
}
";" {
tokenString("delim", ";");
}
"[" {
tokenString("delim", "[");
}
"]" {
tokenString("delim", "]");
}
"{" {
tokenString("delim", "{");
}
"}" {
tokenString("delim", "}");
}
"break" {
tokenString("KW", "break");
}
"continue" {
tokenString("KW", "continue");
}
"while" {
tokenString("KW", "while");
}
"do" {
tokenString("KW", "do");
}
"if" {
tokenString("KW", "if");
}
"else" {
tokenString("KW", "else");
}
"true" {
tokenString("KW", "true");
}
"false" {
tokenString("KW", "false");
}
"for" {
tokenString("KW", "for");
}
"int" {
tokenString("KW", "int");
}
"print" {
tokenString("KW", "print");
}
"const" {
tokenString("KW", "const");
}
"read" {
tokenString("KW", "read");
}
"boolean" {
tokenString("KW", "boolean");
}
"bool" {
tokenString("KW", "bool");
}
"void" {
tokenString("KW", "void");
}
"float" {
tokenString("KW", "float");
}
"double" {
tokenString("KW", "double");
}
"string" {
tokenString("KW", "string");
}
"return" {
tokenString("KW", "return");
}

"+" {
token("+");
}
"-" {
token("-");
}
"*" {
token("*");
}
"/" {
token("/");
}
"%" {
token("%");
}
"=" {
token("=");
}
"<" {
token("<");
}
"<=" {
token("<=");
}
">=" {
token(">=");
}
"!=" {
token("!=");
}
"==" {
token("==");
}
"&&" {
token("&&");
}
"||" {
token("||");
}
"!" {
token("!");
}
">" {
token(">");
}

{letter}({letter}|{digit})* {
    tokenString("id", yytext);
    
    int k = 0;
    if(Nstor[0] == 0){
        int pp = 0;
        for(pp = 0;pp<yyleng;pp++){
            Cstor[0][pp] = yytext[pp];
        }
        Nstor[0]++;
    }
    else{
      for(k = 0;k<257; k++){
        int mm = 0;
        int comflag = 0;
        for(mm = 0;mm<yyleng;mm++){
          if(Cstor[k][mm] != yytext[mm]){
              break;
          }
          if(mm == (yyleng-1)){
              Nstor[k]++;
              comflag = 1;
          }
        }
        if(comflag == 1){
            break;
        }
        if((Nstor[k] == 0)){
            int pp = 0;
            for(pp = 0;pp<yyleng;pp++){
                Cstor[k][pp] = yytext[pp];
            }
            Nstor[k]++;
            break;
        }
      }
    }
}

0|{nonzero_digit}{digit}* {
    tokenString("integer", yytext);
}
(0|{nonzero_digit}{digit}*)\.[digit]* {
    tokenString("float", yytext);
}
{digit}+(\.{digit}+)?[Ee][\+\-]?{digit}+    {
    tokenString("scientific", yytext);
}


{space} {
    LIST;
}



<INITIAL>\"(([^\"]|\\\")*[^\\])?\" { 
    int i = 0;
    int al = 0;
    for(i = 0;i<yyleng;i++) {
      if((yytext[i] == '\\') && (yytext[i+1] == '\"')){
        str_temp[al] = yytext[i+1];
        i++;
        al++;
      }
      else if((yytext[i] == '\\') && (yytext[i+1] == '\\')){
        str_temp[al] = yytext[i+1];
        i++;
        al++;
      }
      else {
        str_temp[al] = yytext[i];
        al++;
      }
    }
    tokenString("string", str_temp);
    for(i = 0;i<al;i++){
      str_temp[i] = 0;
    }
}

<INITIAL>\/\/.*\n {
    LIST;
    if (Opt_Source)
        printf("%d:%s\n", linenum, buf);
    ++linenum;
    buf[0] = '\0';
}

<INITIAL>\/\* {
    LIST;
    BEGIN COMMENT;
}
<COMMENT>.  {
    LIST;
}
<COMMENT>\*\/ {
    LIST;
    BEGIN 0;
}

<INITIAL>^#pragma{space}*(source|token|statistic){space}*(on|off) {
    LIST;
    int j = 0;
    for(j = 0;j<yyleng;j++){
        if((yytext[j] == 's') && (yytext[j+1] == 'o')){
            for(j = j;j<yyleng;j++){
                if((yytext[j] == 'o') && (yytext[j+1] == 'n')){
                    Opt_Source = 1;
                }
                else if((yytext[j] == 'o') && (yytext[j+1] == 'f')){
                    Opt_Source = 0;
                }
            }
        }
        else if((yytext[j] == 't') && (yytext[j+1] == 'o')){
            for(j = j;j<yyleng;j++){
                if((yytext[j] == 'o') && (yytext[j+1] == 'n')){
                    Opt_Token = 1;
                }
                else if((yytext[j] == 'o') && (yytext[j+1] == 'f')){
                    Opt_Token = 0;
                }
            }
        }
        else if((yytext[j] == 's') && (yytext[j+1] == 't')){
            for(j = j;j<yyleng;j++){
                if((yytext[j] == 'o') && (yytext[j+1] == 'n')){
                    Opt_Statistic = 1;
                }
                else if((yytext[j] == 'o') && (yytext[j+1] == 'f')){
                    Opt_Statistic = 0;
                }
            }
        }
    }
    BEGIN PRAGMA_COMMET;
}

<PRAGMA_COMMET>\/\/.* {
    LIST;
    if (Opt_Source)
        printf("%d:%s\n", linenum, buf);
    ++linenum;
    buf[0] = '\0';
}
<PRAGMA_COMMET>\/\* {
    LIST;
    BEGIN COMMENT;
}


<PRAGMA_COMMET>\n {
    LIST;
    if (Opt_Source)
        printf("%d:%s\n", linenum, buf);
    ++linenum;
    buf[0] = '\0';
    BEGIN 0;
}

\n {
LIST;
if (Opt_Source)
printf("%d:%s\n", linenum, buf);
++linenum;
buf[0] =  '\0';
}

. { // Unrecognized character
fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
exit(1);
}




%%

int main( int argc, char **argv )
{
if (argc >= 2)
{
yyin = fopen( argv[1], "r" );
if ( NULL == yyin ) {
fprintf( stderr, "Cannot open: %s\n", argv[1] );
exit(-1);
}
}
else
{
yyin = stdin;
}
yylex();
if (Opt_Statistic)
{
// Print frequency table
printf("frequencies of identifiers:\n");
int ll = 0;
for(ll = 0;ll<257;ll++){
  
}
for(ll = 0;ll<257;ll++){
  if(Nstor[ll] != 0){
      printf("%s\t", Cstor[ll]);
      printf("%d\n", Nstor[ll]);
  }
  else{
      break;
  }
}
}
exit(0);
}
