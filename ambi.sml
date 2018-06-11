(* Ambientes. *)
(* Los ambientes son representados como listas de pares de objetos *)

type 'a Ambiente = (Identificador * 'a) list

(* el ambiente vacío es una lista vacía de pares *)
val ambienteVacio = []

(* La operacion de extension de ambientes se representa como la
   concatenacion de listas.  Note que los elementos provenientes
   del segundo operando apareceran primero en la lista resultante.
   Esta operacion corresponde a la cruz encerrada en un circulo
   que vimos en clase.  Hay una funcion de busqueda que examina el
   ambiente de izquierda a derecha, encontrando de esa manera las
   asociaciones que se hayan puesto mas recientemente.
*)

infix <+>  (* se declara un simbolo infijo, que se define abajo *)

fun amb1 <+> amb2 = amb2 @ amb1  
(* @ concatena dos listas y produce una nueva *)

(* La siguiente funcion forma un ambiente constituido por un solo par. *)

infix 1 |->

fun ident |-> valor = [ (ident,valor) ]

(* Las siguientes declaraciones implementan la busqueda en el ambiente. 
   Cuando un identificador no esta definido en el ambiente, se levanta 
   una excepcion. *)

exception NoEstaEnElDominio of Identificador

fun busca ident []
    = raise NoEstaEnElDominio ident
|   busca ident ((ident',valor)::ambiente)
    = if ident = ident'
      then valor
      else busca ident ambiente

(* La siguiente función aplica otra función sobre todos los
   pares (asociaciones) pertenecientes a un ambiente *)

fun map_ambiente f []
    = []
|   map_ambiente f ((ident,valor)::amb)
    = (ident,(f valor))::(map_ambiente f amb)
