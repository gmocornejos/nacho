(* iterExp test *)

use "interp.sml";


(* factorial de 10 *)
evalProg (IterExp([("i", ConstExp(Entera 1), ApExp(IdExp "+", ParExp(IdExp "i", ConstExp(Entera 1)))),
                   ("k", ConstExp(Entera 1), ApExp(IdExp "*", ParExp(IdExp "k", IdExp "i")))], 
                  ApExp(IdExp ">", ParExp(IdExp "i", ConstExp(Entera 10))), 
                  IdExp "k"))


