(* Lenguaje funcional con pares y patrones *)

(* Identificadores, variables, etc.
   Los identificadores son representados mediante hileras. *)

type Identificador = string
type Variable      = Identificador

(* Las literales del lenguaje son enteras nada más *)

datatype Literal = Booleana of bool
                 | Entera    of int

(* Este es un lenguaje de expresiones, con las siguientes opciones:
   - Literal (entera o booleana)
   - Variable
   - Decisión
   - Par
   - Bloque
   - Aplicación de función
   - Abstracción de función

*)

(* una declaración (de función) puede ser recursiva o no *)

datatype Recurrencia =
           Recursiva
         | NoRecursiva            

datatype Expresion =
           ConstExp   of Literal
         | IdExp      of Identificador
         | IfExp      of Expresion * Expresion * Expresion
         | ParExp     of Expresion * Expresion
         | LetExp     of Declaracion * Expresion 
         | ApExp      of Expresion * Expresion
         | AbsExp     of Reglas

and      Patron =
           ConstPat   of Literal
         | IdPat      of Identificador
         | ParPat     of Patron * Patron
         | Comodin
	(* los dos tipos que siguen están subordinados a
           los datatypes anteriores *)
withtype Reglas =
           (Patron * Expresion) list

and      Declaracion =
           Recurrencia * (Patron * Expresion)
;

(* Hay varias cosas en el interprete que no estan implementadas.
   Ud. debera implementarlas. Los componentes no implementados
   levantan esta excepcion cuando se trata de evaluarlos. *)

exception NoImplementada of string
