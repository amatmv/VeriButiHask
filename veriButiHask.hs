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
  show (NewB (j,w,x,y,z))= ("Tirada iniciada per el jugador: " ++ (show j) ++ "\n" ++ "  Tirada 1: " ++ (show w) ++ "\n" ++ "  Tirada 2: " ++ (show x) ++ "\n" ++ "  Tirada 3: " ++ (show y) ++ "\n" ++ "  Tirada 4: " ++ (show w))

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
