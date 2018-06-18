(* File lexer.mll *)
{
	open Parser
}

rule token = parse
	| [' ' '\t' '\n'] {token lexbuf} (*skip blanks*)
	(*| "0" as n {NUM(n)}*)
	(*| '-'? ['1'-'9'] ['0'-'9']* as n {NUM(n)} *)
	| '+' {PLUS}
	| '-' {MINUS}
	| '*' {MULTIPLY}
	| '/' {DIVIDE}
	| '%' {MOD}
	| '^' {POWER}
	| '(' {LEFTBRACKET}
	| ')' {RIGHTBRACKET}
	| 'T' {BOOL(true)}
	| 'F' {BOOL(false)}
	| '=' {EQUAL}
	| "=/=" {NOTEQUAL}
	| '<' {LT}
	| '>' {GT}
	| "<=" {LE}
	| ">=" {GE}
	| "fail" {FAIL}
	| ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']* as ided {VARIABLE (ided)}
	| ['0'] ['a'-'z' 'A'-'Z' '_' ''']* as ided {ID (ided)}
	| ['a'-'z' '1'-'9' '[' ']' '|'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '''  '[' ']' '|']* as ided {ID (ided)}
	| ":-" {IFF}
	| ';' {SEMICOLON}
	| ',' {COMMA}
	| '.' {PERIOD}
	| '[' {LEFTSQUARE}
	| ']' {RIGHTSQUARE}
	| '|' {PIPE}
	| '!' {CUT}
	| _ { token lexbuf }

	| eof {EOF}