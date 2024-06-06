module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Sabor = String
type Temperatura = Number
type Gramos = Number

data Postre = UnPostre {
    nombre::String,
    sabores::[Sabor],
    peso::Gramos,
    temperatura::Temperatura
} deriving Show

bizcocho_borracho::Postre
bizcocho_borracho= UnPostre "bizcocho borracho" ["fruta", "crema"] 100 25

type Hechizo = Postre -> Postre

porcentajeDelPostre::Number->Postre->Number
porcentajeDelPostre porcentaje postre = peso postre * (porcentaje/100)

restarPorcentajeDePeso::Postre -> Number -> Gramos
restarPorcentajeDePeso postre porcentaje = peso postre - porcentajeDelPostre porcentaje postre

agregarSabor::Postre->Sabor->[Sabor]
agregarSabor postre sabor = (sabores postre)++[sabor]

incendio::Hechizo
incendio postre = postre {peso = restarPorcentajeDePeso postre 5, temperatura=temperatura postre + 1} 

immobulus::Hechizo
immobulus postre = postre {temperatura=0}

wingardiumLeviosa::Hechizo
wingardiumLeviosa postre = postre {peso = restarPorcentajeDePeso postre 10, sabores= agregarSabor postre "concentrado"}

diffindo::Number -> Hechizo
diffindo porcentaje postre = postre {peso = restarPorcentajeDePeso postre porcentaje} 

riddikulus::Sabor->Hechizo
riddikulus sabor postre = postre {sabores= agregarSabor postre (reverse sabor)}

avadaKedavra::Hechizo
avadaKedavra postre = immobulus postre {sabores=[]}

estaListo::Hechizo->Postre->Bool
estaListo hechizo postre = (noEstaCongelado hechizo postre) && (tienePeso hechizo postre) && (tieneSabores hechizo postre)

tienePeso:: Hechizo->Postre->Bool
tienePeso hechizo = (>0).peso.hechizo

tieneSabores:: Hechizo->Postre->Bool
tieneSabores hechizo = (>0).length.sabores.hechizo

noEstaCongelado::Hechizo->Postre->Bool
noEstaCongelado hechizo = (>0).temperatura.hechizo

estaranListos::Hechizo->[Postre]->Bool
estaranListos hechizo postres = all (estaListo hechizo) postres 



