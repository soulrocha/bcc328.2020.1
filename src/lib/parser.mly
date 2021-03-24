// parser.mly

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

%start <Absyn.lfundec> program

%nonassoc LT
%left PLUS

%%

program:
| x=fundec EOF        { x }

exp:
| x=LITINT         { $loc , Absyn.IntExp x }
| x=exp PLUS y=exp { $loc , Absyn.OpExp (Absyn.Plus, x, y) }
| x=exp LT y=exp   { $loc , Absyn.OpExp (Absyn.LT, x, y) }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=typeid                  { [x] }
| x=typeid COMMA xs=typeids { x::xs }
