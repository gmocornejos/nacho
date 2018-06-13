(* Lenguaje funcional con pares y patrones *)


(* Definimos un tipo para las cosas que son opcionales *)

datatype 'a option = Nothing
                   | Something of 'a

(* Lenguaje funcional con pares y patrones, extendido
   para efectos de tarea programada.
*)

(* Identificadores, variables, etc.
   Los identificadores son representados mediante hileras. *)

type Identificador = string
type Variable      = Identificador

(* Las literales del lenguaje son enteras y booleanas *)

datatype Literal = Booleana of bool
                 | Entera   of int

(* Este es un lenguaje de expresiones, con las siguientes opciones:
   - Literal (entera o booleana)
   - Variable
   - Condicional simple (if)
   - Condicional generalizado (cond)
   - Par
   - Bloque (let)
   - Aplicaci�n de funci�n
   - Abstracci�n de funci�n
   - Agregaci�n de registro
   - Acceso calificado a campos de un registro
   - Iteraci�n (sin efectos colaterales).

   Adem�s de las declaraciones de valor, el lenguaje permitir�
   las siguientes formas de declaraci�n compuesta:
   - colateral (and)
   - secuencial (;)
   - bloque (local ... in ... end)

*)

(* una declaraci�n (de funci�n) puede ser recursiva o no *)

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
         | RegExp     of (Identificador * Expresion) list
         | CampoExp   of Expresion * Identificador
         | IterExp    of (Identificador * Expresion * Expresion) list * Expresion * Expresion
         | CondExp    of (Expresion * Expresion) list * Expresion option

and	 Declaracion =
           ValDecl    of Recurrencia * Patron * Expresion
         | AndDecl    of Declaracion * Declaracion
         | SecDecl    of Declaracion * Declaracion
         | LocalDecl  of Declaracion * Declaracion
 
and      Patron =
           ConstPat   of Literal
         | IdPat      of Identificador
         | ParPat     of Patron * Patron
         | RegPat     of Identificador list
         | ComoPat    of Identificador * Patron
         | Comodin

  (* el tipo que sigue est� subordinado a
     los datatypes anteriores *)

withtype Reglas =
           (Patron * Expresion) list

;


(* un programa es una expresi�n *)

type Programa = Expresion


(* Hay varias cosas en el int�rprete que no est�n implementadas.
   Ud. deber� implementarlas. Los componentes no implementados
   levantan esta excepci�n cuando se trata de evaluarlos. *)

exception NoImplementada of string

