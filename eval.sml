(* Evaluador.

   Este es realmente el interprete.
   Hay un caso para cada variante dentro de las categorias sintacticas.
*)

(* Cuando se trata de invocar un identificador que no corresponde a 
   una función, se activa esta excepción *)

exception NoEsUnaFuncion of string
and       NoSeAplicanReglas
and       NoHayClausulaElse


(* evalExp evalua una expresion en un ambiente; se
   produce un valor semantico. *)

fun evalExp ambiente exp =
  case exp of
    ConstExp (Entera n)
       => ConstInt n
  | ConstExp (Booleana b)
       => ConstBool b
  | IdExp ident
       => busca ident ambiente
  | IfExp (cond,expV,expF)
       => let val condicion = evalExp ambiente cond
          in  case condicion of
               (ConstBool false) => evalExp ambiente expF
             | (ConstBool true)  => evalExp ambiente expV
             | _                 => raise ErrorDeTipo "se esperaba valor booleano"
          end
  | ParExp (expI,expD)
      => let val valI = evalExp ambiente expI
             and valD = evalExp ambiente expD
         in Par (valI, valD)
         end
(*  | LetExp ((NoRecursiva,(pat,expLocal)), exp)
      => let val valor    = evalExp ambiente expLocal
           ; val ambLocal = concordar pat valor
         in evalExp (ambiente <+> ambLocal) exp
         end *)
    (* cuando una declaración local es recursiva, se prepara el
       ambiente "desenrollándolo" *)
(*  | LetExp ((Recursiva,(pat,expLocal)), exp)
      => let val valor    = evalExp ambiente expLocal
           ; val ambLocal = concordar pat valor
         in evalExp (ambiente <+> (desenrollar ambLocal)) exp
         end *)
  | ApExp (operador,argumento)
      => let val operacion = evalExp ambiente operador
             and operando  = evalExp ambiente argumento
         in case operacion of
              Primitiva funcion
              => (funcion operando)
            | Clausura (reglas,ambDef,ambRec) 
              => aplicarReglas (ambDef <+> (desenrollar ambRec)) reglas operando
            | _  (* cualquier otra cosa no es una función *)
              => raise ErrorDeTipo "operador no es una funcion"
         end
  | AbsExp reglas
      => Clausura (reglas, ambiente, ambienteVacio)
(*  | RegExp (operador,argumento)
      => let val operacion = evalExp ambiente operador
             and operando  = evalExp ambiente argumento
         in case operacion of
              Primitiva funcion
              => (funcion operando)
            | Clausura (reglas,ambDef,ambRec) 
              => aplicarReglas (ambDef <+> (desenrollar ambRec)) reglas operando
            | _  (* cualquier otra cosa no es una función *)
              => raise ErrorDeTipo "operador no es una funcion"
         end *)
  | CondExp ([], else_clause)
      => ( case else_clause of
              Something else_clause => evalExp ambiente else_clause 
            | Nothing               => raise NoHayClausulaElse 
         )
  | CondExp ((cond,exp)::tail, else_clause)
       => let val condition = evalExp ambiente cond
          in case condition of
              (ConstBool false) => evalExp ambiente (CondExp(tail, else_clause))
            | (ConstBool true)  => evalExp ambiente exp
            | _                 => raise ErrorDeTipo "se esperaba valor booleano"
          end 
  | IterExp (localVars, cond, finalExp)
        => let val localAmb = IterVars ambiente localVars
           in IterInternal localVars cond finalExp localAmb ambiente
           end
  | _
    => raise ErrorDeTipo "expresion no valida"

and aplicarReglas ambiente reglas valor =
  (case reglas of
    []
    => raise NoSeAplicanReglas
  | (pat,exp)::masReglas
    => let val ambienteLoc = concordar pat valor
       in evalExp (ambiente <+> ambienteLoc) exp  (* disparar regla *)
       end
       handle PatronesNoConcuerdan   (* seguir con otras reglas *)
              => aplicarReglas ambiente masReglas valor
  )

(* Funciones para iterar *)

and IterVars amb []
    = []
  | IterVars amb ((ident, initExp, updateExp)::tail)
    = (ident |-> evalExp amb initExp) <+> IterVars amb tail

and IterUpdate amb []
    = []
  | IterUpdate amb ((ident, initExp, updateExp)::tail)
    = (ident |-> evalExp amb updateExp) <+> IterUpdate amb tail

and IterInternal localVars cond finalExp localAmb ambiente
    = let val condition = evalExp (localAmb <+> ambiente) cond
      in case condition of
           (ConstBool true)  => evalExp (localAmb <+> ambiente) finalExp
         | (ConstBool false) => let val newLocalAmb = IterUpdate (localAmb <+> ambiente) localVars
                                in IterInternal localVars cond finalExp newLocalAmb ambiente
                                end
         | _                 => raise ErrorDeTipo "se esperaba valor booleano"
      end
;


(* Los programas son expresiones en nuestro lenguaje.  Los unicos
   simbolos globales pre-definidos son los operadores unarios y
   binarios (que se buscan en los ambientes OperacionUnaria y
   Operacion Binaria).  
   Se exportan todas las funciones para que ustedes puedan hacer
   experimentos. *)
   

(* La expresión correspondiente al "programa principal" se evalúa
   en un ambiente vacío. *)

fun evalProg exp = evalExp ambientePrimitivas exp
