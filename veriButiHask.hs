{-------------------------------------------------------------}
{--                                                         --}
{--  IMPLEMENTACIÓ D'UN VALIDADOR DE PARTIDES DE BUTIFARRA  --}
{--                                                         --}
{-------------------------------------------------------------}
import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

--------------------------
-- Definicions de tipus --
--------------------------

{-------- PAL: Pal de la basa ------
  És mostrable i comparable.
-}
data Pal = Oros | Bastos | Copes | Espases
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
  show (NewC (pal, tipus)) =  ((show tipus) ++ (if (show pal) !! 0 `elem` ['A', 'E', 'I', 'O', 'U'] then " d\'" else " de ") ++ (show pal))

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

-------------------------------
--- Definicio de la baralla ---
-------------------------------

baralla = [NewC(Oros, As), NewC(Oros, Dos), NewC(Oros, Tres), NewC(Oros, Quatre), NewC(Oros, Cinc), NewC(Oros, Sis), NewC(Oros, Set), NewC(Oros, Vuit), NewC(Oros, Manilla), NewC(Oros, Sota), NewC(Oros, Cavall), NewC(Oros, Rei), NewC(Espases, As), NewC(Espases, Dos), NewC(Espases, Tres), NewC(Espases, Quatre), NewC(Espases, Cinc), NewC(Espases, Sis), NewC(Espases, Set), NewC(Espases, Vuit), NewC(Espases, Manilla), NewC(Espases, Sota), NewC(Espases, Cavall), NewC(Espases, Rei), NewC(Bastos, As), NewC(Bastos, Dos), NewC(Bastos, Tres), NewC(Bastos, Quatre), NewC(Bastos, Cinc), NewC(Bastos, Sis), NewC(Bastos, Set), NewC(Bastos, Vuit), NewC(Bastos, Manilla), NewC(Bastos, Sota), NewC(Bastos, Cavall), NewC(Bastos, Rei), NewC(Copes, As), NewC(Copes, Dos), NewC(Copes, Tres), NewC(Copes, Quatre), NewC(Copes, Cinc), NewC(Copes, Sis), NewC(Copes, Set), NewC(Copes, Vuit), NewC(Copes, Manilla), NewC(Copes, Sota), NewC(Copes, Cavall), NewC(Copes, Rei)]

----------------------
--- Loop principal ---
----------------------
main = do
  putStrLn "Entra la llavor: "
  llavor <- getLine
  print (fst (shuffle' baralla (mkStdGen (read llavor::Int))))



--------------------------
--- Funcions Auxiliars ---
--------------------------

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

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

{- punts
  Input: Conjunt de cartes
  Output: Puntuació del conjunt de cartes
-}
punts :: [Carta] -> Integer
punts [] = 0
punts ((NewC (pal, tipus)):cv) = (getCardPunctuation tipus) + (punts cv)

{- cartesGuanyades
  Input: Donat un trumfu, un conjunt de cartes i el numero de tirador inicial.
  Output: Retorna una tupla que conté les cartes que ha guanyat cada grup
-}
cartesGuanyades :: Trumfu -> [Carta] -> Integer -> ([Carta],[Carta])
cartesGuanyades trumfu [] _ = ([],[])
cartesGuanyades trumfu (carta:xs) tirador
  | (length xs) - 3 == 0 = if mod _guanyador 2 /= 0 then (fst _cas_base ++ _cartes_guanyades, snd _cas_base) else (fst _cas_base, snd _cas_base ++ _cartes_guanyades)
  | mod ((length xs)+1) 3 == 0 = cartesGuanyades trumfu (drop 3 xs) _guanyador
  | otherwise = error "Les cartes tirades han de ser múltiples de 4"
  where
    _cas_base = (cartesGuanyades trumfu [] 0)
    _guanyador = jugadorGuanyaBasa _baseActual trumfu
    _baseActual = (NewB (tirador, carta, xs !! 0, xs !! 1, xs !! 2))
    _cartes_guanyades = cartesBasa (NewB (_guanyador, carta, xs !! 0, xs !! 1, xs !! 2))

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

{- cartesPalMa
   Input: Una Ma i un Pal
   Output: Retorna les cartes de la Mà que són del Pal
-}
cartesPalMa :: Ma -> Pal -> [Carta]
cartesPalMa (NewM llista) pal= [x | x<-llista, (pal == (getPal x))]

{- puntsParelles
  Input: Un conjunt de Cartes i un Pal
  Output: Conjunt de Cartes que són del Pal especificat
-}
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal llista pal = [x | x<-llista, (pal == (getPal x))]

{- basa1
  Input: Una basa
  Output: Retorna la primera carta de la basa
-}
basa1 :: Basa -> Carta
basa1 (NewB (_,w,_,_,_)) = w

{- basa2
  Input: Una basa
  Output: Retorna la segona carta de la basa
-}
basa2 :: Basa -> Carta
basa2 (NewB (_,_,x,_,_)) = x

{- basa3
  Input: Una basa
  Output: Retorna la tercera carta de la basa
-}
basa3 :: Basa -> Carta
basa3 (NewB (_,_,_,y,_)) = y

{- basa4
  Input: Una basa
  Output: Retorna la quarta carta de la basa
-}
basa4 :: Basa -> Carta
basa4 (NewB (_,_,_,_,z)) = z

{- iniciadorBasa
  Input: Una basa
  Output: Retorna el jugador que tira la primera carta de la basa
-}
iniciadorBasa :: Basa -> Integer
iniciadorBasa (NewB (j,_,_,_,_)) = j

{- cartesBasa
  Input: Una basa
  Output: Retorna una llista que conté les cartes de la basa
-}
cartesBasa :: Basa -> [Carta]
cartesBasa (NewB (j,w,x,y,z)) = [w,x,y,z]

{- palGuanyadorBasa
  Input:
  Output: Retorna una llista que conté les cartes de la basa
-}
palGuanyadorBasa :: Basa -> Trumfu -> Pal
palGuanyadorBasa b t
  | ((cartesPalBasa b (trumfu2Pal t)) /= []) = (trumfu2Pal t)
  | otherwise = (getPal (basa1 b))

--trumfu2Pal
trumfu2Pal :: Trumfu -> Pal
trumfu2Pal trumfu = case trumfu of
  Or -> Oros
  Ba -> Bastos
  Co -> Copes
  Es -> Espases
  Bu -> error "No és un Pal"

pal2Trumfu :: Pal -> Trumfu
pal2Trumfu pal = case pal of
  Oros  -> Or
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


--trampa:: [Ma] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)

--jugadesPossibles:: Ma->Trumfu->Basa->[Carta]
--jugadesPossibles (NewM llista) t b

FiltrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal :: [Carta] -> Carta -> [Carta]
FiltrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal l c
  | (filter (>c) l == []) = l
  | otherwise = (filter (>c) l


filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes :: [Carta] -> Carta -> Trunfu -> [Carta]
filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes l c t
  | ([x | x<-l, (trumfu2Pal t == (getPal x))] == []) = l
  | otherwise [x | x<-l, (trumfu2Pal t == (getPal x))]

-- Donat un enter (mida llista 2 o 3), una llista de cartes i el trunfo retorna el jugador segons ordre de tirada que esta guanyant
quiEstaGuanyant :: Integer -> [Carta] -> Trunfu -> Integer
quiEstaGuanyant :: m (x:xs) t
  | (m == 2) && (getPal x == getPal head xs) = if x > head xs then 1 else 2 
  | (m == 2) && (getPal x /= getPal head xs) = if getPal x == trumfu2Pal t then 1
												else if getPal head xs == trumfu2Pal t then 2
												else 1
  | (m == 3) && (quiEstaGuanyant 2 (take 2 (x:xs)) t == 1) = if quiEstaGuanyant 2 (x:(tail (x:xs))) t == 1 then 1 else 3 
  | (m == 3) && (quiEstaGuanyant 2 (take 2 (x:xs)) t == 2) = if quiEstaGuanyant 2 xs t == 1 then 2 else 3 

  
-- Ma: ma del jugador  -- Trunfu: pal dominant de la partida -- [Carta] cartes de la base pot ser buida -- Return [Carta] possibles cartes
-- Si cartes basa buida podem tirar qualsevol
-- Si cartes basa size 1 (obligats a matar si podem):
		-- Si tenim del mateix pal i podem matar qualsevol que pugui matar
		-- si fallem tirem qualsevol trunfo si tenim
		-- si tampoc tenim trunfo tirem qualsevol
-- Si cartes basa size 2 hem de mirar si el company [0] esta guanyant o no:
		-- Si company esta guanyant podem tirar qualsevol carta del mateix pal que a tirat i si fallem qualsevol altre
		-- altrament
			-- si [1] no ha fallat hem de matar si podem amb cartes del mateix pal sino hem de tirar trunfo si tenim sino qualsevol
			-- si [1] ha fallat (amb trunfo) i tenim encara cartes del pal iniciador podem tirar qualsevol del pal
			-- Si [1] ha fallat (amb trunfo) i fallem tambe hem de matar amb trunfu si podem altrament qualsevol

-- Si cartes base size 3 (el nostre company es [1]):
		-- si el company [1] esta guanyant podem tirar qualsevol carta del mateix pal que a tirat i si fallem qualsevol altre
		-- Altrament:
			-- mirem quina carta esta guanyant(Hem de matar si podem):
				-- Si esta guanyant una carta del mateix pal que ha tirat el company:
					-- si tenim d'aquell pal i podem matar nomes podem tirar aquestes altrament si fallem trunfu si tenim altrament qualsevol
				-- si esta guanyant un fallo (de trunfo):
					-- si tenim del pal de la base qualsevol del pal
					-- Si fallem i tenim un trunfo que pugui matar qualsevol trunfo que pugui matar
					-- altrament qualsevol carta
realJugadesPossibles :: Ma -> Trunfu -> [Carta] -> [Carta]
realJugadesPossibles (NewM llista) _ [] = [x | x<-llista] -- Si no s'ha tirat cap carta començem nosaltres i podem tirar qualsevol carta
realJugadesPossibles (NewM llista) t (x:xs)
  | (length xs + 1 == 1) = if cartesPalMa getPal x == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] x t else FiltrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa llista (getPal x)) x
  | (length xs + 1 == 2) && (quiEstaGuanyant 2 (x:xs) t == 2) = if cartesPalMa getPal x == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] x t else FiltrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa llista (getPal x)) x
  | (length xs + 1 == 2) && (quiEstaGuanyant 2 (x:xs) t == 1) = if cartesPalMa getPal x == [] then [c | c<-llista] else cartesPalMa getPal x
  | (length xs + 1 == 3) =
