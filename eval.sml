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
  | LetExp (decl, exp)
      => let val amb = evalDecl ambiente decl
         in evalExp (ambiente <+> amb) exp
         end 
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
  | RegExp register
      => Registro (map_ambiente (evalExp ambiente) register)
  | CampoExp (exp, ident)
      => let val reg = evalExp ambiente exp
         in case reg of
              (Registro campos) => busca ident campos
            | _                 => raise ErrorDeTipo "La expresion no retorna un valor-registro"
         end
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
            | _                 => raise ErrorDeTipo "[condExp] se esperaba valor booleano"
          end 
            (* localVars = (Identificador, initExp, updateExp) list *)
  | IterExp (localVars, cond, finalExp)
        => let val localAmb = IterVars ambiente localVars
           in IterInternal localVars cond finalExp localAmb ambiente
           end
  | _
    => raise ErrorDeTipo "expresion no valida para IterExp"

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

(* Funcion para evaluar declaraciones *)

and evalDecl ambiente decl =
  case decl of
    ValDecl (NoRecursiva, pat, expLocal)
      => let val valor    = evalExp ambiente expLocal
           ; val ambLocal = concordar pat valor
         in ambLocal
         end
  | ValDecl (Recursiva, pat, expLocal)
      => let val valor    = evalExp ambiente expLocal
           ; val ambLocal = concordar pat valor
         in desenrollar ambLocal
         end
  | AndDecl (decl1, decl2)
      => let val amb1 = evalDecl ambiente decl1
             val amb2 = evalDecl ambiente decl2
         in amb1 <|> amb2
         end
  | SecDecl (decl1, decl2)
      => let val amb1 = evalDecl ambiente decl1
             val amb2 = evalDecl (ambiente <+> amb1) decl2
         in amb1 <+> amb2
         end
  | LocalDecl (decl1, decl2)
      => let val amb1 = evalDecl ambiente decl1
             val amb2 = evalDecl (ambiente <+> amb1) decl2
         in amb2
         end

(* Funciones para iterar *)

and IterVars amb []
    = []
  | IterVars amb ((ident, initExp, updateExp)::tail)
    = (ident |-> evalExp amb initExp) <|> IterVars amb tail

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
