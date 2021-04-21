// parser.mly
+
%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

(*%start <Absyn.lfundec> program*)
%start <Absyn.lfundecs> program 

%nonassoc IN ELSE
%nonassoc LT
%left PLUS

%%

program:
| x=nonempty_list(fundec) EOF       { $loc , x }                              // regra 1 

exps:
| x=separated_nonempty_list(COMMA, exp) { x }    

exp:
| x=LITINT                          { $loc , Absyn.IntExp x }
| x=exp op=operator y=exp           { $loc , Absyn.OpExp (op, x, y) }
| x=ID LPAREN y=exps RPAREN         { $loc , Absyn.FuncCallExp (x,y) } 
| LET x=ID EQ y=exp IN z=exp        { $loc , Absyn.DecExp (x, y, z) }                 
| IF x=exp THEN y=exp ELSE z=exp    { $loc , Absyn.CondExp (x, y, z) } 
| x=ID }

%inline operator:
| PLUS { Absyn.Plus }
| LT   { Absyn.LT }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }
