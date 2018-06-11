(* Este archivo carga todos los componentes del interprete en el
   orden correcto.
*)

use "sintax.sml";  (* sintaxis abstracta del minilenguaje *)
use "ambi.sml";    (* manejo de ambientes *)
use "val.sml";     (* valores semanticos *)
use "concord.sml"; (* concordancia de patrones *)
use "opers.sml";   (* operaciones predefinidas del minilenguaje *)
use "eval.sml";    (* el evaluador *)
use "prufun.sml";  (* pruebitas *)
