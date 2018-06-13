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
   - Aplicación de función
   - Abstracción de función
   - Agregación de registro
   - Acceso calificado a campos de un registro
   - Iteración (sin efectos colaterales).

   Además de las declaraciones de valor, el lenguaje permitirá
   las siguientes formas de declaración compuesta:
   - colateral (and)
   - secuencial (;)
   - bloque (local ... in ... end)

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

  (* el tipo que sigue está subordinado a
     los datatypes anteriores *)

withtype Reglas =
           (Patron * Expresion) list

;


(* un programa es una expresión *)

type Programa = Expresion


(* Hay varias cosas en el intérprete que no están implementadas.
   Ud. deberá implementarlas. Los componentes no implementados
   levantan esta excepción cuando se trata de evaluarlos. *)

exception NoImplementada of string

