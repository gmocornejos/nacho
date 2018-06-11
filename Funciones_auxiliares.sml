(* Unas funciones potencialmente �tiles para su Proyecto #2 de TLP *)

(* para a�adir a ambi.sml *)

(* combinaci�n de ambientes disyuntos *)

exception DominiosNoDisyuntos

fun existe ident []
    = false
|   existe ident ((ident',valor)::ambiente)
    = if ident = ident' 
      then true
      else existe ident ambiente

(* recorre un ambiente, buscando en el otro.  Si encuentra, hay error *)

infix <|>

fun amb1 <|> amb2 =
  let fun comprueba []               = amb1 <+> amb2
      |   comprueba ((ident,_)::amb) = if existe ident amb2 then
                                         raise DominiosNoDisyuntos
                                       else
                                         comprueba amb
  in
    comprueba amb1
  end


(* sustracci�n de dominio *)

infix <--

fun ident <-- [] 
    = []
|   ident <-- ((par as (ident',valor))::ambiente)
    = if ident = ident' then
        ident <-- ambiente
      else
        par :: (ident <-- ambiente)

(* para a�adir a concord.sml *)

(* auxiliar: combina dos listas (de mismo tama�o), mediante la aplicaci�n de una funci�n  *)
fun zipconcat f []      []      = ambienteVacio
|   zipconcat f (x::xs) (y::ys) = (f x y) <|> (zipconcat f xs ys)
|   zipconcat f _       _       = raise PatronesNoConcuerdan



