(* Concordancia de patrones *)

(* se levantará una excepción cuando no se pueda hacer
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
      (* comodín concuerda con todo, no produce asociaciones *)
|   concordar _ _
    = raise PatronesNoConcuerdan
      (* patrón y valor estructuralmente incompatibles *)

(* Atención: la operación <+> no revisa si hay repetición de
   variables introducidas por pati y patd *)