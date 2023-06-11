 #include <string.h>
%%
"/*"([^*]|\*+[^*/])*\*+"/" {
    // printf("comment1: [%s]\n", yytext);
}
bool {
    //printf("BOOL: %s\n", yytext);
    return BOOL;
}
char {
    //printf("CHAR: %s\n", yytext);
    return CHAR;
}
int {
    //printf("INT: %s\n", yytext);
    return INT;
}
real {
    //printf("REAL: %s\n", yytext);
    return REAL;
}
string {
    //printf("STRING: %s\n", yytext);
    return STRING;
}
int\* {
    //printf("INTPTR: %s\n", yytext);
    return INTPTR;
}
char\* {
    //printf("CHARPTR: %s\n", yytext);
    return CHARPTR;
}
real\* {
    //printf("REALPTR: %s\n", yytext);
    return REALPTR;
}

while {
    //printf("WHILE: %s\n", yytext);
    return WHILE;
}
for {
    //printf("FOR: %s\n", yytext);
    return FOR;
}
var {
    //printf("VAR: %s\n", yytext);
    return VAR;
}
(true)|(false) {
    //printf("BOOLLIT: %s\n", yytext);
    yylval.rawTokenValue = strdup(yytext);
    return BOOLLIT;
}
arg-\> {
    //printf("PARAMGRH: %s\n", yytext);
    return PARAMGRH;
}
function {
    //printf("FUNCTION: %s\n", yytext);
    return FUNCTION;
}
return {
    //printf("RETURN: %s\n", yytext);
    return RETURN;
}
null {
    //printf("NULLLIT: %s\n", yytext);
    yylval.rawTokenValue = strdup("NULL");
    return NULLLIT;
}
void {
    //printf("VOID: %s\n", yytext);
    return VOID;
}
do {
    //printf("DO: %s\n", yytext);
    return DO;
}

if {
    //printf("IF: %s\n", yytext);
    return IF;
}
else {
    //printf("ELSE: %s\n", yytext);
    return ELSE;
}
"==" {
    //printf("EQ: %s\n", yytext);
    return EQ;
}
"!=" {
    //printf("NOTEQ: %s\n", yytext);
    return NOTEQ;
}
">" {
    //printf("GR: %s\n", yytext);
    return GT;
}
">=" {
    //printf("GREQ: %s\n", yytext);
    return GREQ;
}
"<" {
    //printf("LE: %s\n", yytext);
    return LT;
}
"<=" {
    //printf("LEEQ: %s\n", yytext);
    return LEEQ;
}
[a-zA-Z]+[a-zA-Z0-9_]* {
    //printf("ID: %s\n", yytext);
    yylval.rawTokenValue = strdup(yytext);
    return ID;
}
\'.\' {
    //printf("CHARLIT: %s\n", yytext);
    yylval.rawTokenValue = strdup(yytext);
    return CHARLIT;
}
\".*\" {
    //printf("STRINGLIT: %s\n", yytext);
    yylval.rawTokenValue = strdup(yytext);
    return STRINGLIT;
}
"&&" {
    //printf("AND: %s\n", yytext);
    return AND;
}
"||" {
    //printf("OR: %s\n", yytext);
    return OR;
}
"{" {
    //printf("BLOCK_START: %s\n", yytext);
    return BLOCK_START;
}
"}" {
    //printf("BLOCK_END: %s\n", yytext);
    return BLOCK_END;
}
([0-9]+)|(0[xX][0-9A-F]+) {
    //printf("INTLIT: %s\n", yytext);
    yylval.rawTokenValue = strdup(yytext);
    return INTLIT;
}
([0-9]+\.[0-9]+)([eE][-\+][0-9]+)? {
    //printf("REALLIT: %s\n", yytext);
    yylval.rawTokenValue = strdup(yytext);
    return REALLIT;
}
[ \t]+ { ; }
[\n] {
    yylineno++;
}
. {
    //printf("SYMBOL: %s\n", yytext);
    return yytext[0];
}
%%