/* File parser.mly */
%{
  open A6
%}

%token PLUS MINUS MULTIPLY DIVIDE CUT FAIL MOD POWER LEFTBRACKET RIGHTBRACKET EQUAL NOTEQUAL GT LT GE LE IFF SEMICOLON COMMA PERIOD LEFTSQUARE RIGHTSQUARE PIPE EOF
%token <string> NUM
%token <bool> BOOL
%token <string> VARIABLE
%token <string> ID
%start main
%type <(A6.term*(A6.term list)) list> main
%%

main:
	clauseList PERIOD {$1}
	| SEMICOLON {[(Node(Sym "semicolon",[]),[])]}
	| EOF {[(Node(Sym "file_end",[]),[])]};
clauseList:
	clause {[$1]}
	|clause COMMA clauseList {$1::$3}
clause:
	atom {($1,[])}
	| atom IFF body {($1,$3)};
atom:
	ID {Node(Sym $1,[])}
	|CUT {Cut}
	|FAIL {Fail}
	|ID LEFTBRACKET termList RIGHTBRACKET {Node(Sym $1,$3)};
/*	|LP atom RP {$2}   why to add this? */
termList:
	term {[$1]}
	|term COMMA termList {$1::$3};
term:
	ID {Node(Sym $1,[])}
	|VARIABLE {V(Var($1))}
	|ID LEFTBRACKET termList RIGHTBRACKET {Node(Sym $1,$3)}
body:
	atom {[$1]}
	|atom COMMA body {$1::$3}