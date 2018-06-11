> evalProg factorial;
val it = [Entera 5, Entera 120] : Literal list
> evalDec [] varx;
val it = [("x", Celda(ref(Entera 1)))] : (string * Val) list
> evalProgAmb it (Print (IdExp "x"));
val it = [Entera 1] : Literal list
> intercambio;
val it = LetCmd(SecDecl(VarDecl("x", ConstExp(Entera 1)), VarDecl("y", ConstExp(
Entera 2))), SecCmd(SecCmd(SecCmd(Print(IdExp "x"), Print(IdExp "y")), LetCmd(Co
nstDecl("temp", IdExp "x"), SecCmd(Asignacion("x", IdExp "y"), Asignacion("y", I
dExp "temp")))), SecCmd(Print(IdExp "x"), Print(IdExp "y")))) : Comando
> evalCmd [] intercambio;
val it = () : unit
> !salida;
val it = [Entera 1, Entera 1, Entera 2, Entera 2, Entera 1] : Literal list
> evalCmd [] intercambio;
val it = () : unit
> !salida;
