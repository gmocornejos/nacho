(* Register test *)

use "interp.sml";

evalProg (
    CampoExp(
        RegExp([("Edad", ConstExp(Entera 21)),
                ("Donante", ConstExp(Booleana true)),
                ("Infracciones", ConstExp(Entera 0))]),
        "Edad" 
    )
)
