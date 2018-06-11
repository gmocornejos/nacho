(* Unas funciones potencialmente útiles para su Proyecto #2 de TLP *)

(* para añadir a ambi.sml *)

(* combinación de ambientes disyuntos *)

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


(* sustracción de dominio *)

infix <--

fun ident <-- [] 
    = []
|   ident <-- ((par as (ident',valor))::ambiente)
    = if ident = ident' then
        ident <-- ambiente
      else
        par :: (ident <-- ambiente)

(* para añadir a concord.sml *)

(* auxiliar: combina dos listas (de mismo tamaño), mediante la aplicación de una función  *)
fun zipconcat f []      []      = ambienteVacio
|   zipconcat f (x::xs) (y::ys) = (f x y) <|> (zipconcat f xs ys)
|   zipconcat f _       _       = raise PatronesNoConcuerdan



