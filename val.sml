(* Valores semanticos.
   Nuestro lenguaje admite literales, pares de valores,
   funciones primitivas y funciones definidas por el
   programador (posiblemente recursivas) *)

datatype Valor =
           ConstBool of bool
         | ConstInt  of int
         | Par       of Valor * Valor
         | Clausura  of
             Reglas           (* patrones y expresiones *)
           * (Valor Ambiente) (* ambiente prevaleciente
                                 al definir la función *)
           * (Valor Ambiente) (* ambiente para resolver
                                 llamadas recursivas *)
         | Primitiva of Valor -> Valor


(* "Desenrollar" clausuras.
   Esta operación prepara las clausuras presentes en un
   ambiente para que puedan hacerse llamadas recursivas. *)

fun desenrollar ambR =
  let
    fun desenrollar_item (Clausura (reglas,ambE,_))
        = Clausura (reglas, ambE, ambR)
    |   desenrollar_item item_no_clausura
        = item_no_clausura
  in map_ambiente desenrollar_item ambR
  end
  