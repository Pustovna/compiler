This is two parts of a compiler for a C/C++ like language. 
Implemented syntactic and semantic analysis. 
Language description is available in file Language. 


Command to start. 
lex proj.l && yacc proj.y -Wcounterexamples && cc -o proj y.tab.c -ll -Ly && ./proj<test.txt
