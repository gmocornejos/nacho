(* pruebas para lenguaje funcional con pares y patrones *)

(* let a = 5 !!!!!!! ojo: cambiar esto para las pruebas
   in let rec fact = fn n => if n < 2 then
                               1
                             else
                               n * fact (n - 1)
      in
        fact (a + 1)
      end
   end
*)


val aMas1 = ApExp (IdExp "+",
                   ParExp (IdExp "a",ConstExp (Entera 1)))
val fact_aMas1 = ApExp (IdExp "fact", aMas1)

val nMenor2 = ApExp (IdExp "<",
                     ParExp (IdExp "n",ConstExp (Entera 2)))

val nMenos1 = ApExp (IdExp "-",
                     ParExp (IdExp "n", ConstExp (Entera 1)))
val fact_nMenos1 = ApExp (IdExp "fact", nMenos1)
val nPorfact_nMenos1 = ApExp (IdExp "*",
                              ParExp (IdExp "n",fact_nMenos1))

val iffact = IfExp (nMenor2,ConstExp (Entera 1),nPorfact_nMenos1)

val reglaN = (IdPat "n", iffact)

(* factorial con if *)
val defFact = AbsExp [reglaN]

val letrecFact =
  LetExp ((Recursiva,(IdPat "fact", defFact))
         , fact_aMas1)

(* factorial de 6 *)
val pru6 =
  LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera 5)))
         , letrecFact)

(* factorial de 8 *)
val pru8 =
  LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera 7)))
         , letrecFact)

(* let a = 5 !!!!!!! ojo: cambiar esto para las pruebas
   in let rec fact = fn 0 => 1
                     |  1 => 1
                     |  n => n * fact (n - 1)
      in
        fact (a + 1)
      end
   end
*)

val regla0  = (ConstPat (Entera 0), ConstExp (Entera 1))
val regla1  = (ConstPat (Entera 1), ConstExp (Entera 1))
val reglaN' = (IdPat "n", nPorfact_nMenos1)

(* factorial con patrones (casos) *)
val defFact' = AbsExp [regla0, regla1, reglaN']

val letrecFact' =
  LetExp ((Recursiva,(IdPat "fact", defFact'))
         , fact_aMas1)

val pru6' =
  LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera 5)))
         , letrecFact')

val pru8' =
  LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera 7)))
         , letrecFact')

val pru0 =
  LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera (~ 1))))
         , letrecFact')

val pru1 =
  LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera 0)))
         , letrecFact')

val tresmascinco = ApExp (IdExp "+",
                   ParExp (ConstExp (Entera 3),ConstExp (Entera 5)))

fun sumar a b = ApExp (IdExp "+",
                   ParExp (ConstExp (Entera a),ConstExp (Entera b)))

fun factorial x =   LetExp ((NoRecursiva,(IdPat "a", ConstExp (Entera (x - 1))))
         , letrecFact)


val es_cuerpo = [(ParPat (IdPat "a", IdPat "b"), ParExp (IdExp "b", IdExp "a"))]

fun let_espejo a b = LetExp ((NoRecursiva,(IdPat "espejo", AbsExp es_cuerpo))
                            , ApExp (IdExp "espejo", ParExp (ConstExp (Entera a),ConstExp (Entera b)))
                            )

 