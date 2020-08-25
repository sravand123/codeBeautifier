
type lineNo            = int
type pos               = lineNo 
val  lineRef : pos ref = ref 0   
				    

fun updateLine n      = lineRef := !(lineRef) + n



type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


fun lineRange l r = "line " ^ l
				  
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")


fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)


fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode


%%
%header (functor ExprLexFun(structure Tokens : Expr_TOKENS));
ws    = [\ \t];
digit = [0-9]+;
str   = [a-z_A-Z]+[0-9]*[a-z_A-Z]*;
%%
\n({ws}*\n)*       => (lex());
{ws}+         => (lex());
{digit}+      => (Tokens.CONST(toInt yytext,!lineRef,!lineRef));
"/*"([^*]|\*+[^*/])*\*+"/"       => ( lex());

"\"".*"\""         => (Tokens.STRING(yytext,!lineRef,!lineRef));
":="      => (Tokens.ASS(!lineRef,!lineRef));
"if"           =>(Tokens.IF(!lineRef,!lineRef));
"else"           =>(Tokens.ELSE(!lineRef,!lineRef));
"then"           =>(Tokens.THEN(!lineRef,!lineRef));
"end"		=>(Tokens.END(!lineRef,!lineRef));
"in"		=>(Tokens.IN(!lineRef,!lineRef));
"do"		=>(Tokens.DO(!lineRef,!lineRef));
"to"		=>(Tokens.TO(!lineRef,!lineRef));
"for"		=>(Tokens.FOR(!lineRef,!lineRef));
"import"	=>(Tokens.IMPORT(!lineRef,!lineRef));
"while"		=>(Tokens.WHILE(!lineRef,!lineRef));
"array"		=>(Tokens.ARRAY(!lineRef,!lineRef));
"function"		=>(Tokens.FUNCTION(!lineRef,!lineRef));
"primitive"		=>(Tokens.PRIMITIVE(!lineRef,!lineRef));
"class"		=>(Tokens.CLASS(!lineRef,!lineRef));
"method"		=>(Tokens.METHOD(!lineRef,!lineRef));
"extends"		=>(Tokens.EXTENDS(!lineRef,!lineRef));
"let"		=>(Tokens.LET(!lineRef,!lineRef));
"+"           => ( Tokens.PLUS  (!lineRef,!lineRef) );
"="           => ( Tokens.EQUAL  (!lineRef,!lineRef) );
"<"           => ( Tokens.LESS  (!lineRef,!lineRef) );
">"           => ( Tokens.GREAT  (!lineRef,!lineRef) );
"<="           => ( Tokens.LEQ  (!lineRef,!lineRef) );
">="           => ( Tokens.GEQ  (!lineRef,!lineRef) );
"-"           => ( Tokens.MINUS  (!lineRef,!lineRef) );
"of"           => ( Tokens.OF  (!lineRef,!lineRef) );
"nil"	 => (Tokens.NIL (!lineRef,!lineRef) );
"type"	 => (Tokens.TYPE (!lineRef,!lineRef) );
":"	 => (Tokens.SEMICOLON (!lineRef,!lineRef) );
"var"	=>  (Tokens.VAR (!lineRef,!lineRef) );
"new"	 => (Tokens.NEW (!lineRef,!lineRef) );
"."		 => (Tokens.DOT (!lineRef,!lineRef) );
","		 => (Tokens.COMMA (!lineRef,!lineRef) );
"break"		=> (Tokens.BREAK (!lineRef,!lineRef) );
"/" 	      => (Tokens.DIV(!lineRef,!lineRef));
"*"           => ( Tokens.MUL (!lineRef,!lineRef) );
"("           => ( Tokens.LPAREN (!lineRef,!lineRef) );
")"           => (Tokens.RPAREN (!lineRef,!lineRef) );
"["	 => (Tokens.SQBRL (!lineRef,!lineRef) );
"]"		 => (Tokens.SQBRR (!lineRef,!lineRef) );
"{"		 => (Tokens.CURBL (!lineRef,!lineRef) );
"}"	 	=> (Tokens.CURBR (!lineRef,!lineRef) );
";"           => (Tokens.COLON (!lineRef,!lineRef) );
{str}          => (Tokens.ID(yytext,!lineRef,!lineRef));


