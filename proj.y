%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAXS_SYMBOLS = 100;

int yylex();
void yyerror();
int yywrap();

typedef struct TreeNode {
    char* token;
    struct TreeNode* left;
    struct TreeNode* middle;
    struct TreeNode* right;
} TreeNode;

struct param {
    char** data;
    int size;
    int capacity;
};

struct Symbol {
    char* name;
    char* type;
    char* id;
    struct param* some_param;
    int scope; // 0 - global scope; 1 - local scope, etc
}symbolTable[200];

typedef struct {
    int* data;
    int size;
    int capacity;
}stack;

stack arr;
int scopeCounter = 0; 
int error = 0; // 0 - no error; 1 - error
int resultOfExpr = 1; 
int resultOfLogicExpr = 1; 
int countOfArgs = 0;

// for logic expression
char* tempType = ""; 
char* typeArrInt = "Not Found"; 
int countArr = 0;
char* currentOperation = "";
//-----------------

void printTree(TreeNode*, int);
void freeTree(TreeNode*);


void addSymbol(char* name, char* type, char* id, struct param* param, int scope);
int lookup(char* id); // return quantity of symbols with the same id
void printSymbolTable();
void startSemanticAnalis(TreeNode* tree, int num);
void deleteSymbolByScope(int scope);
struct Symbol* findSymbolById(char* id); 
void closeScope();

void addArr(TreeNode* tree, int num); // add array to symbol table

struct param* createParam(int capacity);
void addparam(struct param* param, char* element);
void addParamToSymbol(struct Symbol* sym, struct param* new_param);
char* getDataByIndex(struct param* p, int index);
void clearParam(struct param* p);
void printParamById(char* id);

int checkLogicExpr(TreeNode* tree, int num);
int checkDereferenced(TreeNode* tree);
int getSumOfArgs(TreeNode* tree, int num);
void hasMain();
int checkTypeInExpr(TreeNode* tree, int num);
int checkNULL(TreeNode* tree); // check if id didn't declared


char* getCharFromTree(TreeNode* tree, int deep, char* child); // get char from tree by deep and child   
void initStack(stack* arr);
void addToStack(stack* arr, int value);
int findInStack(stack* arr, int value);
void removeStack(stack* arr, int index);
void freeStack(stack* arr);
void printStack(stack* arr);
int getLastStack(stack* arr);
int getValueOfLastStack(stack *arr);


TreeNode* makeNode(char*, TreeNode*, TreeNode*, TreeNode*);
TreeNode* TREE;
%}

%union {
    char* rawTokenValue;
    struct TreeNode* node;
}

%type <node> program statement statement_list if_statement vardecl id cond block block_context expr expr_eq init_assign finish_assign assign LIT type while_loop for_setting for_loop do_loop iter varincr vardecr expr_lt expr_subt expr_mult expr_add expr_div expr_or expr_and expr_noteq expr_leeq expr_gt expr_greq action_signature actiondecl params varlist paramgr return_statement actioncall args varlist_initable arrlist_initiable arrdecl arrentry intlit expr_lengthof expr_deref expr_ref expr_enslosed expr_flipped expr_negated scope
%token BLOCK_START BLOCK_END BOOL CHAR INT REAL STRING EQ NOTEQ GT GREQ LT LEEQ AND OR RETURN VOID VAR PARAMGRH IF ELSE DO WHILE FOR FUNCTION INTPTR CHARPTR REALPTR
%token <rawTokenValue> INTLIT REALLIT BOOLLIT CHARLIT STRINGLIT NULLLIT ID
%type expr_lengthof
%left OR
%left AND
%left EQ NOTEQ
%left LT LEEQ GT GREQ
%left '+' '-'
%left '*' '/'
%left '!' '&'
%left '='
%left '(' ')'
%left ELSE

%%
program: statement_list { TREE = makeNode("program", $1, NULL, NULL);}
statement_list: statement statement_list { $$ = makeNode("statement_list", $1, $2, NULL); }
            | statement { $$ = $1; }
;
statement: block_context { $$ = $1; }
        | vardecl ';' { $$ = makeNode("statement", $1, NULL, NULL); }
        | arrdecl ';' { $$ = makeNode("statement", $1, NULL, NULL); }
        | assign ';' { $$ = makeNode("statement", $1, NULL, NULL); }
        | return_statement ';' { $$ = makeNode("statement", $1, NULL, NULL); }
        | expr ';' { $$ = makeNode("statement", $1, NULL, NULL); }
;
return_statement: RETURN expr {$$ = makeNode("return", $2, NULL, NULL); }
block_context: if_statement {$$ = $1; }
            | while_loop {$$ = $1; }
            | for_loop { $$ = $1; }
            | do_loop { $$ = $1; }
            | actiondecl { $$ = $1; }
 //           | BLOCK_START block BLOCK_END { $$ = makeNode("block", $2, NULL, NULL);}
            | scope { $$ = $1; }
;
scope: BLOCK_START block BLOCK_END { $$ = makeNode("scope", $2, NULL, NULL); } ;
if_statement: IF '(' cond ')' statement ELSE statement { 
                TreeNode* elseSection = makeNode("ELSE", $7, NULL, NULL);
                TreeNode* ifSection = makeNode("IF", $5, NULL, NULL);
                $$ = makeNode("if_statement", $3, ifSection, elseSection); 
            }
            | IF '(' cond ')' statement { 
                TreeNode* ifSection = makeNode("IF", $5, NULL, NULL);
                $$ = makeNode("if_statement", $3, ifSection, NULL); 
            }
;
while_loop: WHILE '(' cond ')' statement { $$ = makeNode("while_loop", $3, $5, NULL); } ;
for_loop: FOR for_setting block_context { 
        $$ = makeNode("for_loop", $2, $3, NULL); 
    }
;
for_setting: '(' assign ';' cond ';' iter ')' { $$ = makeNode("for_setting", $2, $4, $6); };
do_loop: DO block_context WHILE '(' cond ')' ';' { 
        $$ = makeNode("do_loop", $2, $5, NULL); 
    }
;
cond: expr { $$ = makeNode("condition", $1, NULL, NULL); };
iter: assign { $$ = $1; } 
    | varincr { $$ = makeNode("varincr", $1, NULL, NULL); } 
    | vardecr { $$ = makeNode("vardecr", $1, NULL, NULL); } 
;
varincr: id '+' '+' { $$ = $1; } ;
vardecr: id '-' '-' { $$ = $1; } ;
block: statement_list { $$ = $1; } 
    | { $$ = NULL; } 
;
assign: init_assign '=' finish_assign  { $$ = makeNode("assign", $1, $3, NULL); }
;
init_assign: id { $$ = $1; }
        | id '[' expr ']' { $$ = makeNode("array_member", $1, $3, NULL); }
        | expr_deref { $$ = $1; }
;
finish_assign: expr { $$ = $1; }
           | '&' arrentry { $$ = makeNode("referenced", $2, NULL, NULL); }
;
expr: LIT { $$ = $1; }
    | actioncall { $$ = $1; }
    | id { $$ = $1; }
    | arrentry { $$ = $1; }
    | expr_lengthof { $$ = $1; }
    | expr_subt { $$ = $1; }
    | expr_mult { $$ = $1; }
    | expr_add { $$ = $1; }
    | expr_div { $$ = $1; }
    | expr_or { $$ = $1; }
    | expr_and { $$ = $1; }
    | expr_eq { $$ = $1; }
    | expr_noteq { $$ = $1; }
    | expr_lt { $$ = $1; }
    | expr_leeq { $$ = $1; }
    | expr_gt { $$ = $1; }
    | expr_greq { $$ = $1; }
    | expr_enslosed { $$ = $1; }
    | expr_deref { $$ = $1; }
    | expr_flipped { $$ = $1; }
    | expr_negated { $$ = $1; }
    | expr_ref { $$ = $1; }
;
expr_greq: expr GREQ expr { $$ = makeNode("GREQ", $1, $3, NULL); };
expr_gt: expr GT expr { $$ = makeNode("GT", $1, $3, NULL); };
expr_leeq: expr LEEQ expr { $$ = makeNode("LEEQ", $1, $3, NULL); };
expr_lt: expr LT expr { $$ = makeNode("LT", $1, $3, NULL); };
expr_noteq: expr NOTEQ expr { $$ = makeNode("NOTEQ", $1, $3, NULL); };
expr_eq: expr EQ expr { $$ = makeNode("EQ", $1, $3, NULL); };
expr_or: expr OR expr { $$ = makeNode("OR", $1, $3, NULL); };
expr_and: expr AND expr { $$ = makeNode("AND", $1, $3, NULL); };
expr_add: expr '+' expr { $$ = makeNode("ADD", $1, $3, NULL); };
expr_subt: expr '-' expr { $$ = makeNode("SUB", $1, $3, NULL); };
expr_mult: expr '*' expr { $$ = makeNode("MULT", $1, $3, NULL); };
expr_div: expr '/' expr { $$ = makeNode("DIV", $1, $3, NULL); };
expr_enslosed: '(' expr ')' { $$ = makeNode("enclosed", $2, NULL, NULL); };
expr_deref: '*' id { $$ = makeNode("dereferenced", $2, NULL, NULL); } 
        | '*' expr_enslosed { $$ = makeNode("dereferenced", $2, NULL, NULL); } 
;
expr_flipped: '!' expr { $$ = makeNode("flipped", $2, NULL, NULL); } ;
expr_negated: '-' expr { $$ = makeNode("negated", $2, NULL, NULL); } ;
expr_ref: '&' id { $$ = makeNode("referenced", $2, NULL, NULL); } ;
expr_lengthof: '|' id '|' { $$ = makeNode("LENGTHOF", $2, NULL, NULL); };
LIT: intlit { $$ = $1; }
    | REALLIT { 
        TreeNode* value = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("REALLIT", value, NULL, NULL); 
    }
    | BOOLLIT { 
        TreeNode* value = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("BOOLLIT", value, NULL, NULL); 
    }
    | CHARLIT { 
        TreeNode* value = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("CHARLIT", value, NULL, NULL); 
    }
    | STRINGLIT { 
        TreeNode* value = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("STRINGLIT", value, NULL, NULL); 
    }
    | NULLLIT { 
        TreeNode* value = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("NULLLIT", value, NULL, NULL); 
    }
;
intlit: INTLIT { 
        TreeNode* value = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("INTLIT", value, NULL, NULL); 
    }
vardecl: VAR varlist_initable ':' type { $$ = makeNode("vardecl", $2, $4, NULL); };
varlist: id { $$ = makeNode("varlist", $1, NULL, NULL); } 
    | id ',' varlist { $$ = makeNode("varlist", $1, $3, NULL); }
;
varlist_initable: id '=' expr { $$ = makeNode("varlist", $1, $3, NULL); } 
    | id '=' expr ',' varlist_initable { $$ = makeNode("varlist", $1, $3, $5); }
    | id { $$ = makeNode("varlist", $1, NULL, NULL); } 
    | id ',' varlist_initable { $$ = makeNode("varlist", $1, $3, NULL); }
;
id: ID {
        TreeNode* idName = makeNode($1, NULL, NULL, NULL); 
        $$ = makeNode("id", idName, NULL, NULL); 
    }
;
arrdecl: type arrlist_initiable { $$ = makeNode("arrdecl", $1, $2, NULL); } ;
arrentry: id '[' expr ']' { $$ = makeNode("arrentry", $1, $3, NULL); } ;
arrlist_initiable: arrentry '=' expr { $$ = makeNode("arrentry_init", $1, $3, NULL); } 
                | arrentry '=' expr ',' arrlist_initiable { $$ = makeNode("arrentry_init", $1, $3, $5); } 
                | arrentry { $$ = makeNode("arrentry_init", $1, NULL, NULL); } 
                | arrentry ',' arrlist_initiable { $$ = makeNode("arrentry_init", $1, $3, NULL); } 
;
actiondecl: FUNCTION action_signature block_context {
    $$ = makeNode("funcdecl", $2, $3, NULL);
}
;
action_signature: id '(' params ')' ':' type { $$ = makeNode("action_signature", $1, $3, $6); };
actioncall: id '(' args ')' { $$ = makeNode("actioncall", $1, $3, NULL); };
type: BOOL {
        TreeNode* value = makeNode("BOOL", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | CHAR {
        TreeNode* value = makeNode("CHAR", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | INT {
        TreeNode* value = makeNode("INT", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | REAL {
        TreeNode* value = makeNode("REAL", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | STRING {
        TreeNode* value = makeNode("STRING", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | INTPTR {
        TreeNode* value = makeNode("INTPTR", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | CHARPTR {
        TreeNode* value = makeNode("CHARPTR", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | REALPTR {
        TreeNode* value = makeNode("REALPTR", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
    | VOID {
        TreeNode* value = makeNode("VOID", NULL, NULL, NULL); 
        $$ = makeNode("TYPE", value, NULL, NULL); 
    }
;
params: paramgr { $$ = makeNode("params", $1, NULL, NULL); }
        | paramgr ';' params { $$ = makeNode("params", $1, $3, NULL); }
        | { $$ = makeNode("params", NULL, NULL, NULL); }
;
paramgr: PARAMGRH varlist ':' type { $$ = makeNode("paramgroup", $2, $4, NULL); };
args: expr { $$ = makeNode("args", $1, NULL, NULL); }
    | expr ',' args { $$ = makeNode("args", $1, $3, NULL); }
    | { $$ = makeNode("args", NULL, NULL, NULL); }
;
%%

#include "lex.yy.c"

extern int yylineno;

int main() {
    int res = yyparse();
    initStack(&arr);
    
    startSemanticAnalis(TREE, 0);
    
    hasMain();
  
    deleteSymbolByScope(1);
    freeStack(&arr);
    if (error == 1) {
        printf("\nEnd with errors\n");
        exit(0);
    } else {
        printTree(TREE, 0);
        freeTree(TREE);
    }
    
	return res;
}

void yyerror() { 
    fprintf(stderr, "###Sintaxis error @ LINE %d\n", yylineno);
    fprintf(stderr, "\t### error in: \"%s\"\n", yytext);
}

int yywrap() {
    return 1;
}

void printTree(TreeNode *tree, int space) {
    if (tree) {
        for (int i = 0; i < space; i++) { printf("%d  ",space); }  
        printf("%s\n", tree->token);

        printTree(tree->left, space + 1);
        printTree(tree->middle, space + 1);
        printTree(tree->right, space + 1);
    }
}


char* currentPosition = "program";
char* lastSymb;
char* lastFunc;
char* typeOfParam;
int paramOpen = 0; 
int assignParam = 0;
char* nameCall;

void startSemanticAnalis(TreeNode* tree, int num) {
   
    
    if(tree) {
        lastSymb = tree->token;
   
        if (!strcmp(tree->token,"program")) {
            scopeCounter++;
            addToStack(&arr, num);
            currentPosition = "scope";
        } // add first scope

    if (!strcmp(tree->token,"action_signature")) {
        scopeCounter++;
        addToStack(&arr,num);
        currentPosition = "params";
        paramOpen = 1; 
    } else if (!strcmp(tree->token,"scope") && paramOpen == 0) {
        if (num == getValueOfLastStack(&arr)) {
            closeScope();
        }
        scopeCounter++;
        addToStack(&arr,num);
        currentPosition = "scope";
    } else if (!strcmp(tree->token,"scope")) {
        currentPosition = "scope";
        paramOpen = 0;
    } else if (!strcmp(tree->token,"statement") && num <= getValueOfLastStack(&arr)) {      
        closeScope();       
    } else if (!strcmp(tree->token,"statement_list") && num == getValueOfLastStack(&arr)) {      
        closeScope();       
    }

    if (num < getValueOfLastStack(&arr))  {
        while (num < getValueOfLastStack(&arr)) {                    
            closeScope();
        }  
    } 

    if (!strcmp(tree->token,"paramgroup")) {
        typeOfParam = getCharFromTree(tree, 1, "middle");
        if (paramOpen == 1) {
            struct param* newParam = createParam(1);
            struct Symbol* func = findSymbolById(lastFunc); 
            addParamToSymbol(func, newParam);
        }
        paramOpen = 2;
    }
    if (!strcmp(currentPosition, "params") && !strcmp(tree->token,"varlist")) {
        addSymbol("varlist", typeOfParam, tree->left->left->token, NULL , getLastStack(&arr));
        struct Symbol* func = findSymbolById(lastFunc);
        addparam(func->some_param, typeOfParam);
    }
   
    if (!strcmp(tree->token,"args") && findSymbolById(nameCall) == NULL) {
        error = 1; // if function dosen't declaration - error; 
    } else if (!strcmp(tree->token,"args") && tree->left == NULL) {       
        if (findSymbolById(nameCall)->some_param != NULL && findSymbolById(nameCall)->some_param->size != 0) {
            printf("\nSemantic error: function %s must has %d parametrs.", nameCall, findSymbolById(nameCall)->some_param->size);
            error = 1;
        }
    }  else if (!strcmp(tree->token,"args") ) {
        if (paramOpen == 0) {
             
            int sizeParams = findSymbolById(nameCall)->some_param->size;
            int sumOfArgs;
            countOfArgs = 0;
            
            sumOfArgs = getSumOfArgs(tree, num);
            if (sumOfArgs != sizeParams) {
                printf("\nSemantic error: incorrect number of argument in %s. Expected amount %d. Currently %d", nameCall, sizeParams, sumOfArgs);
                error = 1;
            }
        }
       
        if (!strcmp(tree->left->token, "ADD") || !strcmp(tree->left->token, "SUB") || !strcmp(tree->left->token, "MULT") || !strcmp(tree->left->token, "DIV")) {
            if (checkTypeInExpr(tree->left, num) == 0) {
                printf("\nExprexion with different types in function: %s", nameCall);
                resultOfExpr = 1;
                error = 1;
            }
            
        } else {
            char* currentTypeOfFunction = getDataByIndex(findSymbolById(nameCall)->some_param, paramOpen);
            char* currentTypeOfArg;
            if(!strcmp(tree->left->token, "id") && findSymbolById(tree->left->left->token) != NULL ) {
                currentTypeOfArg = findSymbolById(tree->left->left->token)->type;
            } 
            if (currentTypeOfFunction == NULL) {
                printf("\nSemantic error: call undeclared function or function without parametrs");
                error = 1;
            } else if(!strcmp(tree->left->token, "INTLIT")) {
                currentTypeOfArg = "INT";
            } else if (!strcmp(tree->left->token, "CHARLIT")) {
                currentTypeOfArg = "CHAR";
            } else if (!strcmp(tree->left->token, "STRINGLIT")) {
                currentTypeOfArg = "STRING";
            } else if (!strcmp(tree->left->token, "REALLIT")) {
                currentTypeOfArg = "REAL";
            }
            

            if (strcmp(currentTypeOfFunction, currentTypeOfArg) ) {
                printf("\nSemantic error: args type is not correct. Declaration type: %s, your type: %s", currentTypeOfFunction, currentTypeOfArg);
                error = 1;
            }
            paramOpen++; 
        }
    } // check parametrs in calling function
   

    if ((!strcmp(tree->token,"vardecl") && !strcmp(currentPosition, "scope"))) { 
        typeOfParam = getCharFromTree(tree, 1, "middle");
    }
    if(!strcmp(currentPosition, "scope") && !strcmp(tree->token,"varlist")) {
        if (findSymbolById(getCharFromTree(tree, 1, "left")) != NULL && findSymbolById(getCharFromTree(tree, 1, "left"))->scope == getLastStack(&arr)) {
                    printf("\nSemantic error: duplicate variable %s", getCharFromTree(tree, 1, "left"));
                    error = 1;
                } else {
                    addSymbol("varlist", typeOfParam, getCharFromTree(tree, 1, "left"), NULL, getLastStack(&arr));
                } 
    }
    if (!strcmp(tree->token,"arrdecl") && !strcmp(currentPosition, "scope")) {
        addArr(tree, num);
    } 
 
    if (!strcmp(tree->token,"dereferenced")) {
        int isDereferenced = checkDereferenced(tree);
        if (isDereferenced == 0) {
            printf("\nSemantic error: error in dereferenced: isn't pointer");
            error = 1;
        }
    }

    if(!strcmp(tree->token,"funcdecl")) { 
            if (!strcmp(getCharFromTree(tree, 2, "left"), "main")) {
                if (strcmp(getCharFromTree(tree->left, 1, "right"), "VOID")) {
                    printf("\nSemantic error: type of main function must be VOID\n");
                    error = 1;
                }
            } // main condition check
            if (findSymbolById(getCharFromTree(tree, 2, "left")) != NULL && findSymbolById(getCharFromTree(tree, 2, "left"))->scope == getLastStack(&arr)) {
                printf("\nSemantic error: duplicate fanction %s\n", getCharFromTree(tree, 2, "left"));
                error = 1;
            }            
            addSymbol(tree->token,getCharFromTree(tree->left, 1, "right"),getCharFromTree(tree, 2, "left"), NULL, getLastStack(&arr));
            lastFunc = getCharFromTree(tree, 2, "left");       
    } // function declaration
   
    if (!strcmp(tree->token,"actioncall") || !strcmp(tree->token,"assign")) {
            char* leftType;
            char* rightType = "Not found";

            if (strcmp(tree->left->token,"id") && strcmp(tree->left->token,"dereferenced") && strcmp(tree->left->token,"LENGTHOF")) {
                printf("\nSemantic error: left expression not declared: %s", tree->left->token);
                error = 1;
            }  else {
                if (!strcmp(tree->left->token,"dereferenced") || !strcmp(tree->left->token,"LENGTHOF")) {
                    nameCall = getCharFromTree(tree, 2 ,"left");
                } else{
                     nameCall = getCharFromTree(tree, 1 ,"left");
                }
               
                if (findSymbolById(nameCall) == NULL ) {
                    printf("\nSemantic error: %s not declared", nameCall);
                    error = 1;
                } else if (!strcmp(tree->token,"actioncall")) {
                    paramOpen = 0;
                } else if (!strcmp(tree->middle->token,"actioncall")) {
                    leftType = findSymbolById(nameCall)->type;
                    if (findSymbolById(tree->middle->left->left->token) == NULL) {
                        printf("\nSemantic error: function %s not declared", tree->middle->left->left->token);
                        error = 1;
                    } else {
                        rightType = findSymbolById(tree->middle->left->left->token)->type;    
                         if (strcmp(leftType, rightType)) {
                            printf("\nSemantic error: Type of right expression must be same with left %s expression. Left type %s, right type %s", nameCall, leftType, rightType);
                            error = 1;
                        }
                    } 
                } else {
                    leftType = findSymbolById(nameCall)->type;
                    if (!strcmp(tree->middle->token,"MULT") || !strcmp(tree->middle->token,"ADD") || !strcmp(tree->middle->token,"SUB") || !strcmp(tree->middle->token,"DIV") ) {
                        if (strcmp(leftType,"INT")) {
                            printf("\nSemantic error: Left variable %s must be INT.", nameCall);
                            error = 1;
                        } else {
                            int checkRight = checkTypeInExpr(tree->middle, num);
                            if (checkRight == 0) {
                                printf("\nSemantic error: right in assign %s exprexion with different types", nameCall);
                                error = 1;
                            }
                            resultOfExpr = 1;
                        }
                    } else if (!strcmp(tree->middle->token,"AND")  || !strcmp(tree->middle->token,"OR")  || !strcmp(tree->middle->token,"LT")  || !strcmp(tree->middle->token,"GT") || !strcmp(tree->middle->token,"EQ") || !strcmp(tree->middle->token,"NOTEQ") || !strcmp(tree->middle->token,"GREQ") || !strcmp(tree->middle->token,"LEEQ")) {
                        int logicExpr = checkLogicExpr(tree->middle, num);
                     
                        if (strcmp(leftType,"BOOL")) {
                            printf("\nSemantic error: Left variable in exprexion %s must has type BOOL", nameCall);
                            error = 1;
                        } else if (logicExpr == 0) {
                            printf("\nSemantic error: Right exprexion with different types. Left side is %s", nameCall);
                            error = 1;
                        }
                        resultOfLogicExpr = 1;
                      
                    } else if (!strcmp(tree->middle->token,"id")) {
                        if (checkNULL(tree->middle) == 1) {
                            rightType = findSymbolById(tree->middle->left->token)->type;
                            if (strcmp(leftType, rightType)) {
                                printf("\nSemantic error: Type of right expression must be same with left %s expression. Left type %s, right type %s", nameCall, leftType, rightType);
                                error = 1;
                            }
                        }
                        /* if (findSymbolById(tree->middle->left->token) == NULL) {
                            printf("\nSemantic error: %s not declared", tree->middle->left->token);
                            error = 1;
                        } else {
                             rightType = findSymbolById(tree->middle->left->token)->type;
                            if (strcmp(leftType, rightType)) {
                                printf("\n%s --------", tree->middle->left->token);
                                printf("\nSemantic error: Type of right expression must be same with left %s expression. Left type %s, right type %s", nameCall, leftType, rightType);
                                error = 1;
                            }
                        } */
                    }  else if (strcmp(tree->middle->token,"id")) {
                        if (!strcmp(tree->middle->token,"dereferenced")) {
                            if( !strcmp(tree->middle->left->token,"enclosed")) {    
                                rightType = findSymbolById(tree->middle->left->left->left->left->token)->type ;
                            } else {
                                rightType =  findSymbolById(getCharFromTree(tree, 1 ,"middle"))->type;
                            }
                        } else if (!strcmp(tree->middle->token,"LENGTHOF")) {
                            if (findSymbolById(getCharFromTree(tree, 2 ,"middle")) == NULL) {
                                printf("\nSemantic error: variable in | | not declared. Left side is %s", nameCall);
                                error = 1;
                                rightType = "Not found";
                            } else {
                                rightType =  findSymbolById(getCharFromTree(tree, 2 ,"middle"))->type;
                                if (strcmp(rightType,"STRING")) {
                                    printf("\nSemantic error: Type of right expression must be arr of CHAR left %s expression. Left type %s, right type %s", nameCall, leftType, rightType);
                                    error = 1;
                                    rightType = "Not found";
                                } else {
                                    rightType = "INT";
                                }
                            }
                        } else if (!strcmp(tree->middle->token, "flipped")) {
                            if (strcmp(getCharFromTree(tree, 1 ,"middle"), "ID")) {
                                rightType = getCharFromTree(tree, 1 ,"middle");
                            } else if (findSymbolById(getCharFromTree(tree, 2 ,"middle")) == NULL) {
                                printf("\nSemantic error: variable in | | not declared. Left side is %s", nameCall);
                                error = 1;
                                rightType = "Not found";
                            } else {
                                rightType =  findSymbolById(getCharFromTree(tree, 2 ,"middle"))->type;
                            }
                            if (!strcmp(rightType, "BOOLLIT")) {
                                rightType = "BOOL";
                            }
                        }

                        if (!strcmp("INTLIT", tree->middle->token)) {
                        rightType = "INT";
                        } else if (!strcmp("CHARLIT", tree->middle->token)) {
                        rightType = "CHAR";
                        } else if (!strcmp("BOOLLIT", tree->middle->token)) {
                        rightType = "BOOL";
                        } else if (!strcmp("REALLIT", tree->middle->token)) {
                        rightType = "REAL";
                        } else if (!strcmp("STRINGLIT", tree->middle->token)) {
                        rightType = "STRING";
                        } else if (!strcmp("referenced", tree->middle->token)) {
                            if (!strcmp("id", tree->middle->left->token) && strcmp("arrentry", tree->middle->left->token) ) {

                                if (checkNULL(tree->middle->left) == 1) {
                                    rightType = findSymbolById(tree->middle->left->left->token)->type;// adress
                                    if (!strcmp("BOOL", rightType) || !strcmp("VOID", rightType)) {
                                        printf("\nSemantic error: type & is incorrect");
                                        error = 1;
                                    }
                                }
                                
                            } else if (!strcmp("arrentry", tree->middle->left->token)) {
                                rightType = "CHAR";

                            } else {
                                printf("\nSemantic error: & without ID %s", tree->middle->left->token);
                                error = 1;
                            }
                        } else if (!strcmp("arrentry",  tree->middle->token))  {
                            rightType = "CHAR";
                        }

                        if ((!strcmp(leftType, "INTPTR") && !strcmp(rightType, "INT")) || (!strcmp(leftType, "CHARPTR") && !strcmp(rightType, "CHAR")) || (!strcmp(leftType, "REALPTR") && !strcmp(rightType, "REAL"))) {
                           
                        } else if ((!strcmp(leftType, "INT") && !strcmp(rightType, "INTPTR")) || (!strcmp(leftType, "CHAR") && !strcmp(rightType, "CHARPTR")) || (!strcmp(leftType, "REAL") && !strcmp(rightType, "REALPTR"))) {

                        } else if (strcmp(leftType, rightType)) {
                            printf("\nSemantic error: Type of right expression must be same with left %s expression. Left type %s, right type %s", nameCall, leftType, rightType);
                            error = 1;
                        }
                    }
                    
                }
            }
    } // check if fanction or variable has been declaration

    if (!strcmp(tree->token,"if_statement") || !strcmp(tree->token,"while_loop") || !strcmp(tree->token,"for_setting")) {
        tempType = "";
        int types = checkLogicExpr(tree->left, num);

        resultOfLogicExpr = 1;
        if (types == 0) {
            printf("\nSemantic error: types in condition are incorrect");
            error = 1;
        }
    } // check a cycle
   
     if (!strcmp(tree->token,"return")) {        
        struct Symbol* currentFunc;    
        currentFunc = findSymbolById(lastFunc); 
        char* currentVar = "";
        
        if (!strcmp("id", tree->left->token)) {
            if(checkNULL(tree->left) == 1) {
                currentVar = findSymbolById(tree->left->left->token)->type;
            }
            
            if (strcmp(currentFunc->type, currentVar)) {
                printf("\nSemantic error: return type must be %s, current type is %s\n", currentFunc->type, currentVar);
                 error = 1;
            } 
        
         } else if (strcmp("id", tree->left->token)) {

            if (!strcmp("INTLIT", tree->left->token)) {
                currentVar = "INT";
            } else if (!strcmp("CHARLIT", tree->left->token)) {
                currentVar = "CHAR";
            } else if (!strcmp("BOOLLIT", tree->left->token)) {
                currentVar = "BOOL";
            } else if (!strcmp("REALLIT", tree->left->token)) {
                currentVar = "REAL";
            } else if (!strcmp("STRINGLIT", tree->left->token)) {
                currentVar = "STRING";
            }

            if (strcmp(currentFunc->type, currentVar)) {
                printf("\nSemantic error: return type must be %s, current type is %s\n", currentFunc->type, currentVar);
                error = 1;
            }
        }
        if (!strcmp("STRING", currentVar)) {
             printf("\nSemantic error: return can't be STRING");
            error = 1;
        }
    }   
      
        startSemanticAnalis(tree->left, num + 1);    
        startSemanticAnalis(tree->middle, num + 1);      
        startSemanticAnalis(tree->right, num + 1); 
    } 
}

char* getCharFromTree(TreeNode* tree, int deep, char* child) {
    TreeNode *find = tree; 
    if (!strcmp(child,"left")) {
        find = tree->left;
    } else if (!strcmp(child,"right")) {
        find = tree->right;
    } else if (!strcmp(child,"middle")) {
        find = tree->middle;
    } else {
        printf("\nError in parametr child in getCharFromTree");
    }

    for (int i=0; i<deep; i++) {
        find = find->left; 
    }

    return find->token;  
} 

// sementic fanctions
void hasMain() {
    if (!lookup("main")) {
       printf("\nSemantic error: programm must has main function"); 
       error = 1;
    } else if (lookup("main") > 1) {
        printf("\nSemantic error: the main function must be unique");
        error = 1;
    } 
}
// end semantic functions

// symbol table functions
void closeScope() {
    deleteSymbolByScope(scopeCounter);
    removeStack(&arr, scopeCounter-1);
    scopeCounter--;
}

void addSymbol(char* name, char* type, char* id, struct param* param, int scope) {
    // Find the first empty slot in the table
    int i = 0;
    while (symbolTable[i].name != NULL && i < 200) {
        i++;
    }
    // If the table is full, return an error
    if (i == 200) {
        printf("\nError: Symbol table is full\n");
        return;
    }
    // Add the symbol to the table
    symbolTable[i].name = name;
    symbolTable[i].type = type;
    symbolTable[i].id = id;
    symbolTable[i].some_param = param; 
    symbolTable[i].scope = scope;
}

int lookup(char* id) {
    int scope = 0;
    int counter = 0;
    for(int i=0;i<100;i++) {


        if (symbolTable[i].id == NULL) {
            break;
        } else if(!strcmp(id,symbolTable[i].id)) {
            counter++;
            scope = symbolTable[i].scope;
        } 
    }

    if (scope > 0) {
        return counter;
    } else {
        return 0;
    }
}

struct Symbol* findSymbolById(char* id) {
    int last_index = -1;
    for(int i=0;i<100;i++) {
        if (symbolTable[i].id == NULL) {
            break;
        } else if(!strcmp(id, symbolTable[i].id)) {
            last_index = i;
        } 
    }
    if (last_index != -1) {
        return &symbolTable[last_index];
    } else {
        return NULL;
    }
}

void deleteSymbolByScope(int scope) {
    int i, j;
    for (i = 0; i < 200; i++) {
        if (symbolTable[i].scope == scope) {
            
            for (j = i; j < 199; j++) {
                symbolTable[j] = symbolTable[j+1];
            }
           
            symbolTable[199].name = NULL;
            symbolTable[199].type = '\0';
            symbolTable[199].id = '\0';
       
            symbolTable[199].some_param = NULL;
            symbolTable[199].scope = -1;
           
            i--;
        }
    }
}

void addParamToSymbol(struct Symbol* sym, struct param* new_param) {
    sym->some_param = new_param;
}

void printSymbolTable() {
    printf("\nName\tType\tID\tScope\n");
    for (int i = 0; i < 200; i++) {
        if (symbolTable[i].name != NULL) {
            printf("%s\t%s\t%s\t%d\n", symbolTable[i].name, symbolTable[i].type, symbolTable[i].id,  symbolTable[i].scope);
        }
    }
}
// end symbol table functions

TreeNode* makeNode(char *token, TreeNode *left, TreeNode *middle, TreeNode *right){
    TreeNode *newnode = (TreeNode*)malloc(sizeof(TreeNode));
    char *newtoken = (char*)malloc(sizeof(token) + 1);

    strcpy(newtoken, token);
    newnode->token = newtoken;

    newnode->left = left;
    newnode->right = right;
    newnode->middle = middle;

    return newnode;
}

void freeTree(TreeNode* node) {
    if (node == NULL) { return; }

    free(node->token);

    freeTree(node->left);
    freeTree(node->middle);
    freeTree(node->right);

    free(node);
}

//--------------------stack functions 
void initStack(stack* arr) {
    arr->data = NULL;
    arr->size = 0;
    arr->capacity = 0;
}

void addToStack(stack* arr, int value){
    if (arr->size == arr->capacity) {
        int new_capacity = arr->capacity == 0 ? 1 : arr->capacity * 2;
        int* new_data = (int*)realloc(arr->data, new_capacity * sizeof(int));
        if (new_data == NULL) {
            printf("\nError: out of memory\n");
            exit(1);
        }
        arr->data = new_data;
        arr->capacity = new_capacity;
    }
    arr->data[arr->size++] = value;
}

int findInStack(stack* arr, int value) {
    for (int i = 0; i < arr->size; i++) {
        if (arr->data[i] == value) {
            return i;
        }
    }
    return -1;
}

void removeStack(stack* arr, int index) {
    if (index < 0 || index >= arr->size) {
        printf("\nError: index out of range\n");
        exit(1);
    }
    for (int i = index + 1; i < arr->size; i++) {
        arr->data[i - 1] = arr->data[i];
    }
    arr->size--;
    if (arr->capacity > 0 && arr->size * 4 <= arr->capacity) {
        int new_capacity = arr->capacity / 2;
        int* new_data = (int*)realloc(arr->data, new_capacity * sizeof(int));
        if (new_data == NULL) {
            printf("\nError: out of memory\n");
            exit(1);
        }
        arr->data = new_data;
        arr->capacity = new_capacity;
    }
}

void freeStack(stack* arr) {
    free(arr->data);
    arr->data = NULL;
    arr->size = 0;
    arr->capacity = 0;
}

void printStack(stack* arr) {
    printf("[");
    for (int i = 0; i < arr->size; i++) {
        printf("%d", arr->data[i]);
        if (i < arr->size - 1) {
            printf(", ");
        }
    }
    printf("]\n");
}

int getLastStack(stack* arr) {
    return  arr->size;
}

int getValueOfLastStack(stack *arr) {
    return arr->data[arr->size - 1];
}

//--------------------------- ent stack functions
struct param* createParam(int capacity) {
    struct param* new_param = (struct param*) malloc(sizeof(struct param));
    new_param->data = (char**) malloc(capacity * sizeof(int));
    new_param->size = 0;
    new_param->capacity = capacity;
    return new_param;
}

void addparam(struct param* param, char* element) {
    if (param->size == param->capacity) {
        param->capacity *= 2;
        param->data = (char**) realloc(param->data, param->capacity * sizeof(char*));
    }
    param->data[param->size++] = element;
}


void printParamById(char* id) {
    struct Symbol* s = findSymbolById(id);
    if (s == NULL) {
        printf("\nSymbol with id %s not found\n", id);
        return;
    }
    if (s->some_param == NULL) {
        printf("\nSymbol with id %s has no parameters\n", id);
        return;
    }
    printf("\nParameters of symbol with id %s:\n", id);
    for (int i = 0; i < s->some_param->size; i++) {
        printf("%s", s->some_param->data[i]);
    }
    printf("\n");
}

void addNewParamById(char* id) {
    struct Symbol* s = findSymbolById(id);

    if (s == NULL) {
        printf("\nSymbol with id %s not found\n", id);
        return;
    }
    if (s->some_param == NULL) {
        printf("\nSymbol with id %s has no parameters\n", id);
        return;
    }
    printf("\nParameters of symbol with id %s:\n", id);
    for (int i = 0; i < s->some_param->size; i++) {
        printf("%s ", s->some_param->data[i]);
    }
    printf("\n");
}

void clearParam(struct param* p) {
    free(p->data); 
    p->data = NULL; 
    p->size = 0; 
    p->capacity = 0; 
}

char* getDataByIndex(struct param* p, int index) {
    if (index < 0 || index >= p->size) {
        return NULL; 
    }
    return p->data[index];
}

// Semantic functions
int checkTypeInExpr(TreeNode* tree, int num) {

    if (tree) {
        if (!strcmp(tree->token, "id")) {
            if (findSymbolById(tree->left->token) == NULL) {
                printf("\nSemantic error: %s not declared", tree->left->token);
                resultOfExpr = 0;
                return resultOfExpr; 
            } else if (strcmp(findSymbolById(tree->left->token)->type, "INT")) {
                resultOfExpr = 0;
                return resultOfExpr; 
            }
        } else if (!strcmp(tree->token, "CHARLIT") || !strcmp(tree->token, "STRINGLIT") || !strcmp(tree->token, "NULLLIT") ||  !strcmp(tree->token, "BOOLLIT")) {
            resultOfExpr = 0;
            return resultOfExpr; 
        } else {
        
        checkTypeInExpr(tree->left, num + 1);    
        checkTypeInExpr(tree->middle, num + 1);      
        checkTypeInExpr(tree->right, num + 1); 
        return resultOfExpr;
    }
    } else {
        return resultOfExpr;
    }
}

int checkLogicExpr(TreeNode* tree, int num) {
    if (tree) {
        char* newType = "";
        if (!strcmp(tree->token, "enclosed")) {
            if (!strcmp(tree->left->token,"AND")  || !strcmp(tree->left->token,"OR")  || !strcmp(tree->left->token,"LT")  || !strcmp(tree->left->token,"GT") || !strcmp(tree->left->token,"EQ") || !strcmp(tree->left->token,"NOTEQ") || !strcmp(tree->left->token,"GREQ") || !strcmp(tree->left->token,"LEEQ")) {
                 tempType = "";
            } else {
                printf("\nSemantic error: expression isn't boolean");
                resultOfLogicExpr = 0;
                return resultOfLogicExpr;
            }
            typeArrInt = "Not Found"; // 
        }
        if(!strcmp(tree->token, "LENGTHOF")) {
            typeArrInt = "LENGTHOF";
        }
        if(!strcmp(tree->token, "arrentry")) {
            typeArrInt = "arrentry";
        }
        if (!strcmp(tree->token, "id")) {
            if (findSymbolById(tree->left->token) == NULL) {
                printf("\nSemantic error: %s not declared", tree->left->token);
                resultOfLogicExpr = 0;
                return resultOfLogicExpr;
            } else {
                if (!strcmp(typeArrInt, "LENGTHOF")) {
                    if (strcmp("STRING", findSymbolById(tree->left->token)->type)) {
                        printf("\nSemantic error: %s must has type String", tree->left->token);
                        typeArrInt = "STRING";
                        resultOfLogicExpr = 0;
                        return resultOfLogicExpr;
                    } else {
                        newType = "INT";
                    }
                    
                } else if (!strcmp(typeArrInt, "arrentry")) {
                    
                    if (strcmp("STRING", findSymbolById(tree->left->token)->type)) {
                          printf("\nSemantic error: %s must has type String", tree->left->token);
                            resultOfLogicExpr = 0;
                            return resultOfLogicExpr;
                    } else if (!strcmp("STRING", findSymbolById(tree->left->token)->type)) {
                            typeArrInt = "arrentryFirst"; 
                    }
                } else if (!strcmp(typeArrInt, "arrentryFirst")) {
                        if (strcmp("INT", findSymbolById(tree->left->token)->type)) {
                          printf("\nSemantic error: %s must has type INT", tree->left->token);
                            resultOfLogicExpr = 0;
                            return resultOfLogicExpr;
                    } else if (!strcmp("INT", findSymbolById(tree->left->token)->type)) {
                            typeArrInt = "arrentry"; 
                    }
                }
                else {
                    newType = findSymbolById(tree->left->token)->type;
                }
                typeArrInt = "STRING";
                
            } 
        } else if (!strcmp(tree->token, "CHARLIT") || !strcmp(tree->token, "STRINGLIT") || !strcmp(tree->token, "NULLLIT") ||  !strcmp(tree->token, "BOOLLIT") || !strcmp(tree->token, "INTLIT") ) {
        
            if (!strcmp("INTLIT", tree->token)) {
                newType = "INT";
            } else if (!strcmp("CHARLIT", tree->token)) {
                newType = "CHAR";
            } else if (!strcmp("BOOLLIT", tree->token)) {
                newType = "BOOL";
            } else if (!strcmp("REALLIT", tree->token) ) {
                newType = "REAL";
            } else if (!strcmp("STRINGLIT", tree->token)) {
                newType = "STRING";
            } else if (!strcmp("NULLLIT", tree->token)) {
                newType = "NULL";
            } else if (!strcmp("BOOLLIT", tree->token)) {
                newType = "BOOL";
            } 
        }
        if (!strcmp("REALPTR", newType)) {
            newType = "REAL";
        } else if (!strcmp("CHARPTR", newType)) {
             newType = "CHAR";
        } else if (!strcmp("INTPTR", newType)) {
            newType = "INT";
        }

        if (strlen(tempType) == 0 ) {
            tempType = newType;
        } 


         if (!strcmp(tree->token, "OR") || !strcmp(tree->token, "AND")) {
            currentOperation = "ONLYLOGIC";
        } else if (!strcmp(tree->token, "GREQ") || !strcmp(tree->token, "GT") || !strcmp(tree->token, "LT") || !strcmp(tree->token, "LEEQ") ) {
            currentOperation = "INTREAL";
        } else if (!strcmp(tree->token, "EQ") || !strcmp(tree->token, "NOTEQ")) {
            currentOperation = "ALL";
        }// chek which type need

        if (strlen(currentOperation) != 0 && !strcmp(currentOperation, "ONLYLOGIC")) {
            if (strlen(newType) != 0 && (strcmp("BOOL", newType) || strcmp("BOOL", tempType))) {
                printf("\nSemantic error: condition must be BOOL");
                resultOfLogicExpr = 0;
                return resultOfLogicExpr;
            }
        } else if (strlen(currentOperation) != 0 && !strcmp(currentOperation, "INTREAL")) {
            if (strlen(newType) != 0 && (!strcmp("BOOL", newType) || !strcmp("BOOL", tempType) || !strcmp("CHAR", newType) || !strcmp("CHAR", tempType) || !strcmp("STRING", newType) || !strcmp("STRING", tempType) || !strcmp("NULL", newType) || !strcmp("NULL", tempType))) {
                printf("\nSemantic error: condition must be INT or REAL %s %s", tree->left->token, newType);
                resultOfLogicExpr = 0;
                return resultOfLogicExpr;
            }
        } else if (strlen(newType) != 0 && strcmp(tempType, newType)){
            if (strlen(newType) != 0) {
                tempType = newType;
            }
            printf("\nSemantic error: different type in condition, type - %s, symbol - %s", newType , tree->left->token);
            resultOfLogicExpr = 0;
            return resultOfLogicExpr;
        } 
       
        if (strlen(newType) != 0) {
            tempType = newType;
        }
        
        checkLogicExpr(tree->left, num + 1);    
        checkLogicExpr(tree->middle, num + 1);      
        checkLogicExpr(tree->right, num + 1); 
        
        return resultOfLogicExpr;
    } else {
        return resultOfLogicExpr;
    }
}

int checkDereferenced(TreeNode* tree) {
    if (!strcmp(tree->left->token, "id")) {
            if (findSymbolById(tree->left->left->token) == NULL) {
                printf("\nSemantic error: %s didn't declaration", tree->left->left->token);
                return 0;
            } else {
                char* type = findSymbolById(tree->left->left->token)->type;
                if (!strcmp("REALPTR", type) || !strcmp("CHARPTR", type) || !strcmp("INTPTR", type) ) {
                    return 1;
                } else {
                    return 0;
                }
            }
    } else if (!strcmp(tree->left->token, "enclosed")){
        if (findSymbolById(tree->left->left->left->left->token) == NULL) {
                printf("\nSemantic error: %s not declared", tree->left->left->left->left->token);
                return 0;
            } else {
                char* type = findSymbolById(tree->left->left->left->left->token)->type;
                if (!strcmp("REALPTR", type) || !strcmp("CHARPTR", type) || !strcmp("INTPTR", type) ) {
                    return 1;
                } else {
                    return 0;
                }
            }
    } else 
      {
        return 0;
    }
}

int getSumOfArgs(TreeNode* tree, int num) {
    if(tree) {
       
        if (!strcmp(tree->token, "args")) {
            countOfArgs++;
        }
        getSumOfArgs(tree->left, num + 1);    
        getSumOfArgs(tree->middle, num + 1);      
        getSumOfArgs(tree->right, num + 1);

        return countOfArgs; 
    }
}

void addArr(TreeNode* tree, int num) {
    if (tree) {

        if (!strcmp(tree->token,"TYPE")) {
            typeOfParam = getCharFromTree(tree, 0, "left");
            if (strcmp(typeOfParam,"STRING")) {
                printf("\nSemantic error: [] can be only with string. Current type is: %s", typeOfParam);
                error = 1;
            }
        }

        if (!strcmp(tree->token, "arrentry")) {
            if (findSymbolById(getCharFromTree(tree, 1, "left")) != NULL && findSymbolById(getCharFromTree(tree, 1, "left"))->scope == getLastStack(&arr)) {
                printf("\nSemantic error: duplicate variable %s", getCharFromTree(tree, 0, "left"));
                error = 1;
            } else if (!strcmp(tree->middle->token, "INTLIT") || !strcmp(tree->middle->token, "INT")) {
                addSymbol("string", typeOfParam, getCharFromTree(tree, 1, "left"), NULL, getLastStack(&arr));
            } else {
                printf("\nSemantic error: incorrect type index of string %s", getCharFromTree(tree, 0, "left"));
                error = 1;
            }
        }
        
        addArr(tree->left, num + 1);
        addArr(tree->middle, num + 1);
        addArr(tree->right, num + 1);
    }
} 

int checkNULL(TreeNode* tree) {
    if (findSymbolById(tree->left->token) == NULL) {
        printf("\nSemantic error: variable %s not declared", tree->left->token);
        error = 1;
        return 0;
    } else {
        return 1;
    }
}