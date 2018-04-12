{-------------------------------------------------------------}
{--                                                         --}
{--  IMPLEMENTACIÓ D'UN VALIDADOR DE PARTIDES DE BUTIFARRA  --}
{--                                                         --}
{-------------------------------------------------------------}

--------------------------
-- Definicions de tipus --
--------------------------

{-------- PAL: Pal de la basa ------
  És mostrable i comparable.
-}
data Pal = Orus | Bastos | Copes | Espases
           deriving (Show, Eq)

{-------- TRUMFU: Pal de la Ma ------
  El Trumfu pot ser un Pal o "Butifarra".
  És mostrable i comparable.
-}
data Trumfu = Or | Ba | Co | Es | Bu
              deriving (Show, Eq)

{--------- TIPUSCARTA: Numero de la carta ------
  Cada Pal té 12 numeros, que van des del 1 fins al 12.
  És mostrable, comparable i ordenable.
-}
data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla |
                  Sota | Cavall | Rei
                  deriving (Show, Eq)
instance Ord TipusCarta where
  compare tipuscarta1 tipuscarta2
    | (getCardValue(tipuscarta1)) == (getCardValue(tipuscarta2)) = EQ
    | (getCardValue(tipuscarta1)) <= (getCardValue(tipuscarta2)) = LT
    | otherwise = GT

{------ CARTA: Representació en Haskell d'una carta de la baralla espanyola. ------
  Una Carta Pertany a un Pal i és un tipus de carta (que representa el nombre de  l'1 al 12)
  És comparable, mostrable i ordenable.
-}
data Carta = NewC (Pal, TipusCarta)

instance Eq Carta where
  (NewC (p1,t1)) == (NewC (p2,t2)) = (t1 == t2)

instance Show Carta where
  show (NewC (pal, tipus)) =  "["++ ((show tipus) ++ " " ++ (show pal)) ++ "]"

instance Ord Carta where
  compare (NewC (p1,t1)) (NewC (p2,t2))
    | t1 == t2 = EQ
    | t1 <= t2 = LT
    | otherwise = GT

{---- MA: Representació de la ma d'un jugador. ----
  S'entén per "Ma" el conjunt de Cartes que té el jugador a la seva mà.
  És comparable i mostrable.
-}
data Ma = NewM [Carta] deriving Eq

instance Show Ma where
  show (NewM []) = "{}"
  show (NewM (x:xs)) = ("{" ++ (show x) ++ "-" ++ (show2 (NewM xs)) ++ "}")

-- Funció auxiliar per a fer el Show de la ma.
show2 :: Ma -> String
show2 (NewM []) = ""
show2 (NewM (x:[])) = (show x)
show2 (NewM (x:xs)) = ((show x) ++ "-" ++ (show2 (NewM xs)))

{--- BASA: Representació d'una ronda de tirades dels 4 jugadors ----
  La Basa sempre conté quatre cartes i és iniciada per un jugador.
  És comparable.
-}
data Basa = NewB (Integer, Carta, Carta, Carta, Carta) deriving Eq

instance Show Basa where --Fer Basa mostrable.
  show (NewB (j,w,x,y,z))= ("Tirada iniciada per el jugador: " ++ (show j) ++ "\n" ++ "  Tirada 1: " ++ (show w) ++ "\n" ++ "  Tirada 2: " ++ (show x) ++ "\n" ++ "  Tirada 3: " ++ (show y) ++ "\n" ++ "  Tirada 4: " ++ (show z))

--------------------------
--- Funcions Auxiliars ---
--------------------------

{- getCardValue
  Input: Un TipusCarta
  Output: Valor numèric corresponent al tipus de la carta
-}
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

{- getCardPunctuation
  Input: El TipusCarta
  Output: El valor numèric corresponent a la puntuació d'aquest TipusCarta
-}
getCardPunctuation :: TipusCarta ->  Integer
getCardPunctuation tipus = case tipus of
      Sota -> 1
      Cavall -> 2
      Rei -> 3
      As -> 4
      Manilla -> 5
      otherwise -> 0

{- getPuntsCarta
  Input: Conjunt de cartes
  Output: Puntuació del conjunt de cartes
-}
getPuntsCarta :: [Carta] -> Integer
getPuntsCarta [] = 0
getPuntsCarta ((NewC (pal, tipus)):cv) = (getCardPunctuation tipus) + (getPuntsCarta cv)

{- getPal
  Input: Una Carta.
  Output: Pal de la Carta.
-}
getPal :: Carta -> Pal
getPal (NewC (pal, tipus)) = pal

{- getTipus
  Input: Una Carta.
  Output: Tipus de la carta.
-}
getTipus :: Carta -> TipusCarta
getTipus (NewC (pal, tipus)) = tipus

{- cartesPalBasa
  Input: Donada una Basa i el Pal
  Output: Les cartes d'aquest Pal en la Basa
-}
cartesPalBasa :: Basa -> Pal -> [Carta]
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
