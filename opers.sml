(* Operaciones predefinidas (primitivas) del lenguaje.
   Las operaciones se definen en terminos de lo disponible en
   Standard ML. *)

exception ErrorDeValor of string
and       ErrorDeTipo  of string

local  (* todas las operaciones se esconden *)

  (* Operaciones unarias *)

  fun negar (ConstBool b) = (ConstBool (not b))
  |   negar _            = raise ErrorDeTipo "Se esperaba un booleano"

  fun menos (ConstInt e)   = ConstInt (~ e)
  |   menos _            = raise ErrorDeTipo "Se esperaba un entero"

  (* Operaciones binarias *)

  fun conjuncion (Par (ConstBool b1, ConstBool b2))
      = ConstBool(b1 andalso b2)
  |   conjuncion _
      = raise ErrorDeTipo "Algun operando no es booleano"

  fun disyuncion (Par (ConstBool b1,ConstBool b2))
      = ConstBool(b1 orelse b2)
  |   disyuncion _
      = raise ErrorDeTipo "Algun operando no es booleano"

  fun adicion (Par (ConstInt e1,ConstInt e2))
      = ConstInt (e1 + e2)
  |   adicion _
      = raise ErrorDeTipo "Algun operando no es entero"

  fun sustraccion (Par (ConstInt e1,ConstInt e2))
      = ConstInt (e1 - e2)
  |   sustraccion _
      = raise ErrorDeTipo "Algun operando no es entero"

  fun multiplicacion (Par (ConstInt e1, ConstInt e2))
      = ConstInt (e1 * e2)
  |   multiplicacion _
      = raise ErrorDeTipo "Algun operando no es entero"

  fun division (Par (ConstInt e1,ConstInt 0))
      = raise ErrorDeValor "Division entre 0 (cero)"
  |   division (Par (ConstInt e1,ConstInt e2))
      = ConstInt (e1 div e2)
  |   division _
      = raise ErrorDeTipo "Algun operando no es entero"

  (* Operaciones relacionales, i.e. comparaciones *)

  fun menor (Par (ConstInt e1, ConstInt e2))
      = ConstBool (e1 < e2)
  |   menor _
      = raise ErrorDeTipo "Algun operando no es entero"

  fun mayor (Par (ConstInt e1, ConstInt e2))
      = ConstBool (e1 > e2)
  |   mayor _
      = raise ErrorDeTipo "Algun operando no es entero"

  fun mayoroigual (Par (ConstInt e1, ConstInt e2))
      = ConstBool (e1 >= e2)
  |   mayoroigual _
      = raise ErrorDeTipo "Algun operando no es entero"
  
  fun menoroigual (Par (ConstInt e1, ConstInt e2))
      = ConstBool (e1 <= e2)
  |   menoroigual _
      = raise ErrorDeTipo "Algun operando no es entero"

  fun igual (Par (ConstInt e1, ConstInt e2))
      = ConstBool (e1 = e2)
  |   igual (Par (ConstBool b1, ConstBool b2))
      = ConstBool (b1 = b2)
  |   igual _
      = raise ErrorDeTipo "Operandos de tipos diferentes"

  fun diferente (Par (ConstInt e1, ConstInt e2))
      = ConstBool (e1 <> e2)
  |   diferente (Par (ConstBool b1, ConstBool b2))
      = ConstBool (b1 <> b2)
  |   diferente _
      = raise ErrorDeTipo "Operandos de tipos diferentes"

in (* lo que sigue se exporta *)

  (* Formacion del ambiente que contiene a las funciones
     primitivas.
     Se asocia un identificador (que puede ser alfanumerico o
     simbolico) con la abstraccion de funcion que implementa
     - en Standard ML - la operacion. *)

  val ambientePrimitivas =
       "not" |-> Primitiva negar            <+>
       "~"   |-> Primitiva menos            <+>
       "and" |-> Primitiva conjuncion       <+>
       "or"  |-> Primitiva disyuncion       <+>
       "+"   |-> Primitiva adicion          <+>
       "-"   |-> Primitiva sustraccion      <+>
       "*"   |-> Primitiva multiplicacion   <+>
       "/"   |-> Primitiva division         <+>
       "div" |-> Primitiva division         <+>
       "<"   |-> Primitiva menor            <+>
       ">"   |-> Primitiva mayor            <+>
       "<="  |-> Primitiva menoroigual      <+>
       ">="  |-> Primitiva mayoroigual      <+>
       "="   |-> Primitiva igual            <+>
       "<>"  |-> Primitiva diferente

end (* local *)

