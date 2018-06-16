(* decl test *)

use "interp.sml";

(* Codigo de algun pobre estudiante del pasado *)

(* Declaraciones *)
val val1 = ValDecl(NoRecursiva, IdPat "a", ConstExp(Entera 1));
val val2 = ValDecl(NoRecursiva, IdPat "b", ConstExp(Entera 2));
val val3 = ValDecl(NoRecursiva, IdPat "c", ApExp(IdExp "+", ParExp(IdExp "a", ConstExp(Entera 2))));
val val4 = ValDecl(NoRecursiva, IdPat "d", ApExp(IdExp "+", ParExp(IdExp "f", ConstExp(Entera 3))));

evalProg (LetExp(AndDecl(val1, val2), 
                    ApExp(IdExp "+", ParExp(IdExp "a", IdExp "b")))
);

evalProg (LetExp(SecDecl(val1, val2), 
                  ApExp(IdExp "+", ParExp(IdExp "a", IdExp "b")))
);

evalProg (LetExp(LocalDecl(val1, val3),
                  IdExp "c")
);
