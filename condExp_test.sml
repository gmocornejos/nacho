(* condExp test *)

use "interp.sml";

(* Intente cambiar los valores false a true o el símbolo > a < *)

evalProg (CondExp([(ConstExp(Booleana false), ConstExp(Entera 1)),
                   (ConstExp(Booleana false), ConstExp(Entera 10)),
                   (ConstExp(Booleana false), ConstExp(Entera 20)),
                   (ApExp(IdExp ">", ParExp(ConstExp(Entera 30), ConstExp(Entera 20))), ConstExp(Entera 30))],
                    Nothing))
