(* Evaluador.

   Este es realmente el interprete.
   Hay un caso para cada variante dentro de las categorias sintacticas.
*)

(* Cuando se trata de invocar un identificador que no corresponde a 
   una funci�n, se activa esta excepci�n *)

exception NoEsUnaFuncion of string
and       NoSeAplicanReglas

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
  | LetExp ((NoRecursiva,(pat,expLocal)), exp)
      => let val valor    = evalExp ambiente expLocal
           ; val ambLocal = concordar pat valor
         in evalExp (ambiente <+> ambLocal) exp
         end
    (* cuando una declaraci�n local es recursiva, se prepara el
       ambiente "desenroll�ndolo" *)
  | LetExp ((Recursiva,(pat,expLocal)), exp)
      => let val valor    = evalExp ambiente expLocal
           ; val ambLocal = concordar pat valor
         in evalExp (ambiente <+> (desenrollar ambLocal)) exp
         end
  | ApExp (operador,argumento)
      => let val operacion = evalExp ambiente operador
             and operando  = evalExp ambiente argumento
         in case operacion of
              Primitiva funcion
              => (funcion operando)
            | Clausura (reglas,ambDef,ambRec) 
              => aplicarReglas (ambDef <+> (desenrollar ambRec)) reglas operando
            | _  (* cualquier otra cosa no es una funci�n *)
              => raise ErrorDeTipo "operador no es una funcion"
         end
  | AbsExp reglas
      => Clausura (reglas, ambiente, ambienteVacio)
(*Inicia codigo de Mike*)
  | RegExp (operador,argumento)
      => let val operacion = evalExp ambiente operador
             and operando  = evalExp ambiente argumento
         in case operacion of
              Primitiva funcion
              => (funcion operando)
            | Clausura (reglas,ambDef,ambRec) 
              => aplicarReglas (ambDef <+> (desenrollar ambRec)) reglas operando
            | _  (* cualquier otra cosa no es una funci�n *)
              => raise ErrorDeTipo "operador no es una funcion"
         end
  | _
    => raise ErrorDeTipo "expresion no valida"
(*Finaliza codigo de Mike*)

and aplicarReglas ambiente reglas valor =
  case reglas of
    []
    => raise NoSeAplicanReglas
  | (pat,exp)::masReglas
    => let val ambienteLoc = concordar pat valor
       in evalExp (ambiente <+> ambienteLoc) exp  (* disparar regla *)
       end
       handle PatronesNoConcuerdan   (* seguir con otras reglas *)
              => aplicarReglas ambiente masReglas valor
;

(* Los programas son expresiones en nuestro lenguaje.  Los unicos
   simbolos globales pre-definidos son los operadores unarios y
   binarios (que se buscan en los ambientes OperacionUnaria y
   Operacion Binaria).  
   Se exportan todas las funciones para que ustedes puedan hacer
   experimentos. *)
   

(* La expresi�n correspondiente al "programa principal" se eval�a
   en un ambiente vac�o. *)

fun evalProg exp = evalExp ambientePrimitivas exp
