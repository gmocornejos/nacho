(* Concordancia de patrones *)

(* se levantar� una excepci�n cuando no se pueda hacer
   concordancia de patrones *)

exception PatronesNoConcuerdan

fun concordar (ConstPat (Entera n)) (ConstInt n')
    = if n = n' then
        ambienteVacio
        (* literales concuerdan, no se producen asociaciones *)
      else
        raise PatronesNoConcuerdan
|   concordar (ConstPat (Booleana b)) (ConstBool b')
    = if b = b' then
        ambienteVacio
        (* literales concuerdan, no se producen asociaciones *)
      else
        raise PatronesNoConcuerdan
|   concordar (IdPat ident) valor
    = ident |-> valor
      (* se asocia ident con valor, en ambiente unitario *)
|   concordar (ParPat (pati,patd)) (Par (vali,vald))
    = (concordar pati vali)
      <+>                           (* extiende ambiente *)
      (concordar patd vald)
|   concordar Comodin _
    = ambienteVacio
      (* comod�n concuerda con todo, no produce asociaciones *)
|   concordar _ _
    = raise PatronesNoConcuerdan
      (* patr�n y valor estructuralmente incompatibles *)

(* Atenci�n: la operaci�n <+> no revisa si hay repetici�n de
   variables introducidas por pati y patd *)