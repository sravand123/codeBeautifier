structure Ast = struct

datatype Binop = Plus | Minus | Mul | Div
datatype Comp  = LESS | EQUAL | GREAT |LEQ | GEQ
datatype Expr  = VAR of string
		|Const 	of int
		|FOR of (Expr*Expr*Expr*Expr)
		|LET of (Dec list*Expr list)
		|ASS of (Expr*Expr)
		|ARRAY of (Expr *Expr*Expr)
		|RECORD of (Expr * Expr list)
		|WHILE of (Expr*Expr)
		|COMP of (Expr*Comp*Expr)
        |IF    of (Expr*Expr )
		|NIL
		|BREAK
		|LISTEXPR of (Expr list)
		|METHOD of (Expr*Expr*Expr list)
		|NEW of string
		|FUNCTION of (Expr*Expr list)
		|STRING of string
		|IFELSE of(Expr*Expr*Expr)
		|DOT of (Expr*Expr)
		|BOX of (Expr * Expr)
		|Op of Expr*Binop*Expr
		|NEGEXP of Expr
		|METHODCLASS of (Expr*(Expr*Expr)list*Expr)
		|METHODCLASSTYPE of (Expr*(Expr*Expr)list*Expr *Expr)
	and Dec  = VARDEC of Expr * Expr
		|VARDECTYPE of Expr *Expr *Expr
		|IMPORT of string
		|CLASSTYPE of (Expr *(Expr*Expr)list )
		|CLASSEXTENDSTYPE of (Expr *Expr*(Expr*Expr)list )
		|FUNDEC of (Expr * (Expr*Expr)list*Expr)
		|FUNDECTYPE of (Expr * (Expr*Expr)list*Expr*Expr)
		|PRIMITIVEDEC of (Expr * (Expr*Expr)list)
		|PRIMITIVEDECTYPE of (Expr * (Expr*Expr)list *Expr)
		|TYPEDEC of (Expr * Expr)
		|TYPEDECFIELDS of (Expr * (Expr*Expr)list)
		|TYPEARRAYDEC of (Expr*Expr)
		|CLASSDEC of (Expr*Expr list)
		|CLASSEXTENDSDEC of (Expr*Expr *Expr list)
datatype program = foo of Expr
		  | bar of Dec list
		
fun If x y = IF (x,y);
fun ifelse x y z = IFELSE (x,y,z);
fun ass x y = ASS (x,y);
fun Let x y = LET (x,y)
fun While x y = WHILE (x,y)
fun for w x y z  = FOR (w,x,y,z)

fun equal x y = COMP (x,EQUAL,y)
fun less x y = COMP (x,LESS,y)
fun great x y = COMP (x,GREAT,y)
fun leq x y = COMP (x,LEQ,y)
fun geq x y = COMP (x,GEQ,y)

fun plus  a b = Op (a, Plus, b)
fun minus a b = Op (a, Minus, b)
fun mul   a b = Op (a, Mul, b)
fun Miv   a b = Op (a, Div, b)


fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"
  | binOpToString Div   = "/";

fun comptostring EQUAL = "="
    | comptostring LESS ="<"
    | comptostring GREAT=">"
    | comptostring LEQ = "<="
    | comptostring GEQ =">="
    
end
