structure Beautify = 
struct


fun printspace 0 =""
|printspace space  = "   "^(printspace (space-1));
fun indent space (Ast.ASS (x,y)) = (printspace space)^(indent space x)^":="^ (indent space y)^";"
	|indent space (Ast.COMP (x,z,y)) = (indent space x)^(Ast.comptostring z)^ (indent space y)
	| indent space (Ast.Op (x,z,y)) = ((indent space x)^(Ast.binOpToString z)^ (indent space y))
	| indent space (Ast.VAR x) = x
	| indent space (Ast.IF (x,y)) = (printspace space)^("if "^(indent space x)^" then\n"^(indent (space+1) y))
	| indent space (Ast.IFELSE (x,y,z)) = (printspace space)^("if "^(indent space x)^" then "^(indent (space+1) y)^"\n"^(printspace space)^"else "^(indent (space+1) z))
	| indent space (Ast.WHILE (x,y)) = (printspace space)^("while "^(indent space x)^" do\n"^(indent (space+1) y))
	|indent space (Ast.FOR (w,x,y,z)) = (printspace space)^"for "^(indent space w)^" := "^(indent space x)^" to "^(indent space y)^" do\n"^(indent (space+1) z)
	| indent space (Ast.LET (x,y)) = (printspace space)^"let\n"^(indentdeclist (space+1) x)^(printspace space)^"in\n"^(indentlist (space+1) y)^"\n"^(printspace space)^"end\n"
	| indent space (Ast.Const x) = Int.toString x
	| indent space (Ast.ARRAY (x,y,z)) = (indent space x)^"["^(indent space y)^"] of"^(indent space z) 
	| indent space (Ast.NIL) = "nil"
	| indent space (Ast.BREAK) = "break"
	| indent space (Ast.BOX(x,y)) = (indent space x)^"["^(indent space y)^"]"
	| indent space (Ast.DOT(x,y)) = (indent space x)^"."^(indent space y)
	| indent space (Ast.STRING (x)) = x
	| indent space (Ast.LISTEXPR(x)) = "(\n" ^ (indentlist (space+1) x) ^(printspace space)^")"
	| indent space (Ast.RECORD(x,y)) = (indent space x)^"{"^(recordlist space y)^"}" 
	| indent space (Ast.NEW (x)) = ( x) 
	| indent space (Ast.FUNCTION (id,x)) = (printspace space)^(indent space id)^" ("^(recordlist space x) ^")" 
	| indent space (Ast.METHOD (x,y,z))   =  (printspace space)^(indent space x)^"."^(indent space y)^"("^(recordlist space z)^")"
	| indent space (Ast.NEGEXP (x)) = "-"^(indent space x) 
	|indent space (Ast.METHODCLASS (id,tylst,exp)) = (printspace space)^"method " ^(indent space id)^"(" ^(tylist space tylst) ^") = "^(indent space exp)
	|indent space (Ast.METHODCLASSTYPE (id1,tylst,id2,exp)) = (printspace space)^"method " ^(indent space id1)^"(" ^(tylist space tylst) ^") :"^(indent space id2) ^"="^ (indent space exp) 
 and indentlist space [] = ""
	| indentlist space (x::xs) =  (indent space x^";\n"^ indentlist space xs)
 and recordlist space [] = ""
	| recordlist space [x] = indent space x
	|recordlist  space (x::xs) =  (indent space x^", "^ recordlist space xs)
 and indentdeclist space [] = ""
	|indentdeclist space (x::xs) = (indentdec space x^"\n"^indentdeclist space xs)
 and indentdec space (Ast.IMPORT (x)) =(printspace space)^"import"^ x 
	|indentdec space (Ast.VARDEC (id,exp)) = (printspace space)^"var "^(indent space id)^":="^(indent space exp)
	|indentdec space (Ast.VARDECTYPE (id1,id2,exp)) = (printspace space)^"var "^(indent space id1)^": "^(indent space id2)^":="^(indent space exp)
	|indentdec space (Ast.FUNDEC (id,exp1,exp2)) = (printspace space)^"function "^(indent space id) ^"("^ (tylist space exp1) ^")" ^ "=" ^(indent (space) exp2) 
	|indentdec space (Ast.FUNDECTYPE (id,exp1,k,exp2)) = (printspace space)^"function "^(indent space id) ^"("^ (tylist space exp1) ^") " ^":"^(indent space k) ^" =" ^(indent (space) exp2) 
	|indentdec space (Ast.PRIMITIVEDEC (id,exp2)) = (printspace space)^"primitive "^(indent space id) ^"("^ (tylist space exp2) ^")"
	|indentdec space (Ast.PRIMITIVEDECTYPE (id,exp2,k)) = (printspace space)^"primitive "^(indent space id) ^"("^ (tylist space exp2) ^") "^":"^(indent space k)  
	|indentdec space (Ast.TYPEDEC (id1,id2))  = (printspace space)^"type " ^(indent space id1) ^"=" ^ (indent space id2)
	|indentdec space (Ast.TYPEDECFIELDS (id,typefields)) = (printspace space)^"type "^(indent space id) ^"="^"{"^ (tylist space typefields) ^"}"
	|indentdec space (Ast.TYPEARRAYDEC (id1,id2))  =  (printspace space)^"type "^(indent space id1) ^"= array of "^(indent space id2) 
	|indentdec space (Ast.CLASSTYPE (id,tyfields)) = (printspace space)^"type "^(indent space id) ^"= "^"class "^"{"^ (tylist space tyfields) ^"}"
	|indentdec space (Ast.CLASSDEC (id,classfields)) = (printspace space)^"class "^(indent space id) ^"{"^ (classlist space classfields) ^"}"
	|indentdec space (Ast.CLASSEXTENDSDEC (id1,id2,classfields)) = (printspace space)^"class "^(indent space id1)^"extends"^(indent space id2) ^"{"^ (classlist space classfields) ^"}"
	
	|indentdec space (Ast.CLASSEXTENDSTYPE (id1,id2,tyfields)) = (printspace space)^"type "^(indent space id1) ^"= "^"class "^"extends "^(indent space id2)^"{"^ (tylist space tyfields) ^"}"	
 and classlist space [] = ""
	|classlist space (x::xs) = (indent space x)^" "^ (classlist space xs)
 and tylist space [] = ""
	| tylist space [(x,y)] = (indent space x)^":"^(indent space y)
	| tylist space ((x,y)::xs) = (indent space x)^":"^(indent space y)^ "," ^(tylist space xs)
fun indentprogram space (Ast.foo x) = indent space x
|indentprogram space (Ast.bar x) = indentdeclist space x;  
end
