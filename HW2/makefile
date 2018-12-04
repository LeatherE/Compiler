LEX = lex
LEXFN = lex.l
YACC = yacc
YACCFN = parser.y
CC = gcc
LY = -ly -ll
PARSER = parser
LEXYY = lex.yy.c
YACCTAB = y.tab.c

parser: lex yacc
	$(CC) -o $(PARSER) $(LEXYY) $(YACCTAB) $(LY)
lex:
	$(LEX) $(LEXFN)

yacc:
	$(YACC) -d -v $(YACCFN)
