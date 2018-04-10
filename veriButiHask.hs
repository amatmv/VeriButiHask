--Implementació d'un validador de partides de Butifarra.

--Definició dels nous tipus de dades
--Trumfu: Pal de la Ma
data Trumfu = Or | Ba | Co | Es | Bu deriving (Show, Eq) --Fer Trumfu mostrable i comparable

--Pal: Pal de la basa
data Pal = Orus | Bastos | Copes | Espases deriving (Show, Eq) --Fer Pal mosatrable i comparable

--TipusCarta: Numero de la carta
data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Show, Eq) --Fer TipusCarta mostrable i comparable

instance Ord TipusCarta where --Fer TipusCarta ordenable
  compare tipuscarta1 tipuscarta2
    | (getCardValue(tipuscarta1)) == (getCardValue(tipuscarta2)) = EQ
    | (getCardValue(tipuscarta1)) <= (getCardValue(tipuscarta2)) = LT
    | otherwise = GT

--Carta: Representació en Haskell d'una carta de la baralla espanyola.
data Carta = NewC (Pal,TipusCarta)

instance Eq Carta where --Fer Carta comparable
  (NewC (p1,t1)) == (NewC (p2,t2)) = (t1 == t2)

instance Show Carta where --Fer Carta mostrable
  show (NewC (p,t)) =  "["++ ((show t) ++ " " ++ (show p)) ++ "]"

instance Ord Carta where --Fer carta ordenable
  compare (NewC (p1,t1)) (NewC (p2,t2))
    | t1 == t2 = EQ
    | t1 <= t2 = LT
    | otherwise = GT

--Ma: Representació de la ma d'un jugador.
data Ma = NewM [Carta] deriving Eq --Fer Ma comparable

instance Show Ma where --Fer Ma mostrable
  show (NewM []) = "{}"
  show (NewM (x:xs)) = ("{" ++ (show x) ++ "-" ++ (show2 (NewM xs)) ++ "}")
--Funcio auxiliar per a fer el Show de la ma.
show2 :: Ma->String
show2 (NewM []) = ""
show2 (NewM (x:[])) = (show x)
show2 (NewM (x:xs)) = ((show x) ++ "-" ++ (show2 (NewM xs)))

--Basa: Representació d'una ronda de tirades dels 4 jugadors
data Basa = NewB (Integer, Carta, Carta, Carta, Carta) deriving Eq

instance Show Basa where --Fer Basa mostrable.
  show (NewB (j,w,x,y,z))= ("Tirada iniciada per el jugador: " ++ (show j) ++ "\n" ++ "  Tirada 1: " ++ (show w) ++ "\n" ++ "  Tirada 2: " ++ (show x) ++ "\n" ++ "  Tirada 3: " ++ (show y) ++ "\n" ++ "  Tirada 4: " ++ (show z))

--Funcions Auxiliars
--getCardValue: Retorna el valor numèric del Tipuscarta.
getCardValue :: TipusCarta ->  Integer
getCardValue tipus = case tipus of
      Manilla -> 12
      As -> 11
      Rei -> 10
      Cavall -> 9
      Sota -> 8
      Vuit -> 7
      Set -> 6
      Sis -> 5
      Cinc -> 4
      Quatre -> 3
      Tres -> 2
      Dos -> 1

--getCardPunctuation: Retorna el valor numèric de la puntuació del TipusCarta
getCardPunctuation :: TipusCarta ->  Integer
getCardPunctuation tipus = case tipus of
      Sota -> 1
      Cavall -> 2
      Rei -> 3
      As -> 4
      Manilla -> 5
      otherwise -> 0

--punts: Retorna la suma del valor numèric de les cartes
punts :: [Carta] -> Integer
punts [] = 0
punts ((NewC (p,t)):cv) = (getCardPunctuation t) + (punts cv)

--getPal: Retorna el pal d'una carta
getPal :: Carta -> Pal
getPal (NewC (p,t)) = p

--getTipus: Retorna el tipus d'una Carta
getTipus :: Carta -> TipusCarta
getTipus (NewC (p,t)) = t

--cartesPalBasa: Retorna les cartes que son del Pal especificat en la Basa
cartesPalBasa :: Basa->Pal->[Carta]
cartesPalBasa (NewB (j,w,x,y,z)) pal = [x | x<-[w,x,y,z], (pal == (getPal x))]

--cartesPalMa: Retorna les cartes que son del Pal especificat en la ma
cartesPalMa :: Ma->Pal->[Carta]
cartesPalMa (NewM llista) pal= [x | x<-llista, (pal == (getPal x))]

--cartesPal: Retorna les cartes que son del Pal especificat en la llista
cartesPal :: [Carta]->Pal->[Carta]
cartesPal llista pal = [x | x<-llista, (pal == (getPal x))]

--basa1: Retorna la tirada numero 1 de la Basa
basa1 :: Basa -> Carta
basa1 (NewB (_,w,_,_,_)) = w

--basa2: Retorna la tirada numero 2 de la Basa
basa2 :: Basa -> Carta
basa2 (NewB (_,_,x,_,_)) = x

--basa3: Retorna la tirada numero 3 de la Basa
basa3 :: Basa -> Carta
basa3 (NewB (_,_,_,y,_)) = y

--basa4: Retorna la tirada numero 4 de la Basa
basa4 :: Basa -> Carta
basa4 (NewB (_,_,_,_,z)) = z

--iniciadorBasa: Retorna el numero del jugador que tira la primera carta de la basa.
iniciadorBasa :: Basa -> Integer
iniciadorBasa (NewB (j,_,_,_,_)) = j

--cartesBasa
cartesBasa :: Basa -> [Carta]
cartesBasa (NewB (j,w,x,y,z)) = [w,x,y,z]

--palGuanyadorBasa
palGuanyadorBasa :: Basa -> Trumfu -> Pal
palGuanyadorBasa b t
  | ((cartesPalBasa b (trumfu2Pal t)) /= []) = (trumfu2Pal t)
  | otherwise = (getPal (basa1 b))

--trumfu2Pal
trumfu2Pal :: Trumfu -> Pal
trumfu2Pal trumfu = case trumfu of
  Or -> Orus
  Ba -> Bastos
  Co -> Copes
  Es -> Espases
  Bu -> error "No és un Pal"

pal2Trumfu :: Pal -> Trumfu
pal2Trumfu pal = case pal of
  Orus  -> Or
  Bastos -> Ba
  Copes -> Co
  Espases -> Es

jugadorGuanyaBasa :: Basa -> Trumfu -> Integer
jugadorGuanyaBasa b t = tiradorCarta (cartaGuanyadora (basa1 b) (cartesBasa b) (palGuanyadorBasa b t)) b

cartaGuanyadora :: Carta -> [Carta] -> Pal -> Carta
cartaGuanyadora c [] p = c
cartaGuanyadora c (x:xs) p = if (mata c x p) == c then cartaGuanyadora c xs p else cartaGuanyadora x xs p

mata :: Carta -> Carta -> Pal -> Carta
mata c1 c2 p
  | (getPal c1 == p) && (getPal c2 /= p) = c1
  | (getPal c1 /= p) && (getPal c2 == p) = c2
  | c1 >= c2 = c1
  | c2 > c1 = c2

tiradorCarta :: Carta -> Basa -> Integer
tiradorCarta c b
  | (c == (basa1 b)) = (iniciadorBasa b)
  | (c == (basa2 b)) = mod ((iniciadorBasa b)+1) 4
  | (c == (basa3 b)) = mod ((iniciadorBasa b)+2) 4
  | (c == (basa4 b)) = mod ((iniciadorBasa b)+3) 4
