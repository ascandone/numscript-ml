%{
open Ast
%}

/* ----- NEW TOKENS ----- */
%token SEND SAVE VARS SOURCE DESTINATION
%token EQ STAR
%token <string> IDENTIFIER

/* ----- EXISTING TOKENS ----- */
%token <string> VARIABLE
%token <int> INT
%token <int> PERC
%token PLUS MINUS DIV
%token LBRACKET RBRACKET LBRACE RBRACE LPAREN RPAREN
%token MAX FROM TO REMAINING KEPT
%token EOF

/* Precedences */
%left PLUS MINUS
%left DIV
%nonassoc UMINUS

/* ----- ENTRY POINTS ----- */
%start <Ast.program> parse_program
%%

/* ----- PROGRAM & STATEMENTS ----- */

parse_program:
  | p = program EOF { p }

program:
  | vars = vars_opt stmts = list(statement)
      { { vars; statements = stmts } }

vars_opt:
  | /* empty */ 
      { [] }
  | VARS LBRACE vars = list(var_declaration) RBRACE 
      { vars }

var_declaration:
  | typ = IDENTIFIER name = VARIABLE origin = option(var_origin)
      { { typ; name; value = origin } }

var_origin:
  | EQ e = expr 
      { e }

statement:
  /* send [Asset 100] (source = ... destination = ...) */
  | SEND e = expr LPAREN SOURCE EQ s = source DESTINATION EQ d = dest RPAREN
      { StmtSend { monetary = e; source = s; destination = d } }
      
  /* send [Asset *] (source = ... destination = ...) */
  | SEND LBRACKET asset = expr STAR RBRACKET LPAREN SOURCE EQ s = source DESTINATION EQ d = dest RPAREN
      { StmtSendAll { asset; source = s; destination = d } }
      
  /* save [Asset 100] from @account */
  | SAVE e = expr FROM acc = expr
      { Save { monetary = e; account = acc } }


/* ----- EXPRESSIONS (Same as before) ----- */
expr:
  | v = VARIABLE { ExprVar v }
  | i = INT { ExprInt i }
  | p = PERC { ExprPerc p }
  | MINUS e = expr %prec UMINUS { ExprInfix (Sub, ExprInt 0, e) }
  | e1 = expr PLUS e2 = expr { ExprInfix (Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { ExprInfix (Sub, e1, e2) }
  | e1 = expr DIV e2 = expr { ExprInfix (Div, e1, e2) }
  | LBRACKET e1 = expr e2 = expr RBRACKET { ExprMonetaryLit (e1, e2) }
  | LPAREN e = expr RPAREN { e }


/* ----- SOURCES (Same as before) ----- */
source:
  | e = expr { SrcAccount e }
  | MAX cap = expr FROM src = source { SrcMax (cap, src) }
  | LBRACE srcs = list(source) RBRACE { SrcInorder srcs }
  | LBRACE allots = nonempty_list(allotment_clause_src) RBRACE { SrcAllotment allots }

allotment_clause_src:
  | e = expr FROM src = source { (e, src) }


/* ----- DESTINATIONS (Same as before) ----- */
dest:
  | e = expr { DestAccount e }
  | LBRACE allots = nonempty_list(allotment_clause_dest) RBRACE { DestAllotment allots }
  | LBRACE clauses = list(dest_inorder_clause) REMAINING kd = kept_or_dest RBRACE { DestInorder (clauses, kd) }

allotment_clause_dest:
  | e = expr kd = kept_or_dest { (e, kd) }

kept_or_dest:
  | TO d = dest { Dest d }
  | KEPT { Kept }

dest_inorder_clause:
  | MAX cap = expr kd = kept_or_dest { { cap; dest = kd } }