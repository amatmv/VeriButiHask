{-------------------------------------------------------------}
{--                                                         --}
{--  IMPLEMENTACIÓ D'UN VALIDADOR DE PARTIDES DE BUTIFARRA  --}
{--                                                         --}
{-------------------------------------------------------------}
import System.Random
import System.IO.Unsafe
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

--------------------------
-- Definicions de tipus --
--------------------------


{-------- PAL: Pal de les cartes ------
  És mostrable i comparable.
-}
data Pal = Oros | Bastos | Copes | Espases
           deriving (Show, Eq)


{-------- TRUMFU: Pal de la partia ------
  El Trumfu pot ser un Pal o "Butifarra".
  És mostrable i comparable.
-}
data Trumfu = Or | Ba | Co | Es | Bu
              deriving (Show, Eq)

{-------- MULTIPLICADOR: Multiplicadors de la partida ------
  El Multiplicador es el valor per el qual es multiplica la puntuació.
  És mostrable i comparable.
-}
data Multiplicador = Contro | Recontro | SantVicens | Barraca
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


-----------------------------------
--- Implementació de la partida ---
-----------------------------------

jugar = do
  putStrLn "---------------------------------------------------------"
  putStrLn "--------------------- VeriButiHask ----------------------"
  putStrLn "---------------------------------------------------------"
  putStrLn "                              _____"
  putStrLn "  _____                _____ |6    |"
  putStrLn " |2    | _____        |5    || & & |"
  putStrLn " |  &  ||3    | _____ | & & || & & | _____"
  putStrLn " |     || & & ||4    ||  &  || & & ||7    |"
  putStrLn " |  &  ||     || & & || & & ||____9|| & & | _____"
  putStrLn " |____Z||  &  ||     ||____S|       |& & &||8    | _____"
  putStrLn "        |____E|| & & |              | & & ||& & &||9    |"
  putStrLn "               |____h|              |____L|| & & ||& & &|"
  putStrLn "                           _____           |& & &||& & &|"
  putStrLn "                   _____  |K  WW|          |____8||& & &|"
  putStrLn "           _____  |Q  ww| | o {)|                 |____6|"
  putStrLn "    _____ |J  ww| | o {(| |o o%%| _____"
  putStrLn "   |10 & || o {)| |o o%%| | |%%%||A _  |"
  putStrLn "   |& & &||o o% | | |%%%| |_%%%>|| ( ) |"
  putStrLn "   |& & &|| | % | |_%%%O|        |(_'_)|"
  putStrLn "   |& & &||__%%[|                |  |  |"
  putStrLn "   |___0I|                       |____V|"
  putStrLn " "
  putStrLn "---------------------------------------------------------"
  putStrLn "---------------------------------------------------------"
  putStrLn " "
  putStrLn "Entra una llavor per a generar la baralla: "
  llavor <- getLine
  putStrLn "S'ha generat la baralla. "
  putStrLn " "
  putStrLn "Procedint a inicialitzar la partida..."
  putStrLn "Repartint les cartes..."
  -- print(crearMans(repartirCartes (fst (shuffle' baralla (mkStdGen (read llavor::Int))))))
  let mans = crearMans(repartirCartes (fst (shuffle' baralla (mkStdGen (read llavor::Int)))))
  let ma1 = (mans !! 0)
  let ma2 = (mans !! 1)
  let ma3 = (mans !! 2)
  let ma4 = (mans !! 3)
  -- putStrLn "Ma 1: "
  -- print(ma1)
  -- putStrLn "Ma 2: "
  -- print(ma2)
  -- putStrLn "Ma 3: "
  -- print(ma3)
  -- putStrLn "Ma 4: "
  -- print(ma4)
  putStrLn "Cartes repartides als jugadors!"
  putStrLn " "
  putStrLn "La teva ma és la següent: "
  putStrLn " "
  print(ma1)
  putStrLn " "
  putStrLn "El primer jugador a tirar ets tu!"
  putStrLn " "
  putStrLn "Escull un dels seguents Trumfus:"
  escollirTrumfu ma1 ma2 ma3 ma4 1

escollirTrumfu ma1 ma2 ma3 ma4 escollidor = do
  putStrLn "->> Oros, Bastos, Copes, Espases, Butifarra <<-"
  if escollidor == 1 then do --si escollim nosaltres
    trumfu <- getLine
    putStrLn " "
    if trumfu == "Oros" then do
      putStrLn "En aquesta partida el Trumfu escollit es Oros!"
      contrarTime Or ma1 ma2 ma3 ma4 escollidor
    else if trumfu == "Bastos" then do
      putStrLn "En aquesta partida el Trumfu escollit es Bastos!"
      contrarTime Ba ma1 ma2 ma3 ma4 escollidor
    else if trumfu == "Copes" then do
      putStrLn "En aquesta partida el Trumfu escollit es Copes!"
      contrarTime Co ma1 ma2 ma3 ma4 escollidor
    else if trumfu == "Espases" then do
      putStrLn "En aquesta partida el Trumfu escollit es Espases!"
      contrarTime Es ma1 ma2 ma3 ma4 escollidor
    else if trumfu == "Butifarra" then do
      putStrLn "En aquesta partida el Trumfu escollit es Butifarra!!!"
      contrarTime Bu ma1 ma2 ma3 ma4 escollidor
    else do
      putStrLn "No es una opcio correcte, torna a escollir un  dels seguents Trumfus:"
      escollirTrumfu ma1 ma2 ma3 ma4 escollidor
  else do -- la maquina tria
    putStrLn "WIP"
    putStrLn "En aquesta partida el Trumfu escollit es Oros!"
    contrarTime Or ma1 ma2 ma3 ma4 escollidor

contrarTime trumfu ma1 ma2 ma3 ma4 escollidor = do
  if (escollidor == 1) || (escollidor == 3) then do
    if esContra2 trumfu ma2 ma4 then do --la maquina contra
      if esRecontra1 trumfu ma3 then do --evalua si la teva parella recontraria
        putStrLn "#La teva parella s'ha rascat la orella esquerra molt sutilment... estara volguent dir alguna cosa..."
      else do
        putStrLn " "
      putStrLn "Jugador 1, vols recontrar?"
      putStrLn "(S/N)"
      eleccio <- getLine
      if eleccio == "S" || eleccio == "s" then do
        putStrLn "La parella formada per el jugador 1 i 3 ho tenen molt clar i recontren!"
        putStrLn "WIP1"
      else if eleccio == "N" || eleccio == "n" then do
        putStrLn "La parella formada per el jugador 1 i 3  no ho han vist clar i han deixat passar la oportunitat de recontrar..."
        putStrLn "Procedim doncs a comensar la partida sense multiplicadors de puntuacio."
        mainLoop trumfu ma1 ma2 ma3 ma4 escollidor 2
      else do
        putStrLn "No es una opcio valida, torna a respondre:"
        contrarTime trumfu ma1 ma2 ma3 ma4 escollidor
    else do --la maquina no recontra
      putStrLn "La parella formada per el jugador 2 i 4 no ho han vist clar i han deixat passar la oportunitat de contrar..."
      putStrLn "Procedim doncs a comensar la partida sense multiplicadors de puntuacio."
      mainLoop trumfu ma1 ma2 ma3 ma4 escollidor 1
  else do
    putStrLn "WIP2"

mainLoop trumfu ma1 ma2 ma3 ma4 oldEscollidor multiplicador = do
  putStrLn " "
  putStrLn "------   COMENÇA LA PARTIDA   ------"
  putStrLn " "
  let newEscollidor = mod (oldEscollidor + 1) 4
  ent <- getLine
  if ent == "GG" then do
    putStrLn "GG WP"
    mainLoop trumfu ma1 ma2 ma3 ma4 oldEscollidor multiplicador
  else do
    putStrLn "Surrender"


--------------------------
--- Funcions Auxiliars ---
--------------------------

-- Aixo ha de dir si la IA hi aniria a l'hora de Contro Recontro SantVicens i Barraca polete
esRecontra1 :: Trumfu -> Ma -> Bool --ToDo: Aqui necessitem la teva maestria polete!
esRecontra1 trumfu maX = True
esContra2 :: Trumfu -> Ma -> Ma -> Bool --ToDo: Aqui necessitem la teva maestria polete!
esContra2 trumfu maX maY = True
--


crearMans :: [[Carta]] -> [Ma]
crearMans llista = [(NewM (llista !! 0)),(NewM (llista !! 1)),(NewM (llista !! 2)),(NewM (llista !! 3))]

repartirCartes :: [Carta] -> [[Carta]]
repartirCartes (w:x:y:z:xs) = repartirCartes' [[w],[x],[y],[z]] xs

repartirCartes' :: [[Carta]] -> [Carta] -> [[Carta]]
repartirCartes' llista [] = llista
repartirCartes' llista (w:x:y:z:xs) = repartirCartes' [((llista !! 0) ++ [w]), ((llista !! 1) ++ [x]), ((llista !! 2) ++ [y]), ((llista !! 3) ++ [z])] xs

shuffle :: [Carta] -> Int -> [Carta]
shuffle (carta:[]) llavor = [carta]
shuffle (carta:cs) llavor = (cartes !! rand) : shuffle (delete rand cartes) llavor
    where
      cartes = (carta:cs)
      rand = mod unsafeIORandom llavor
      unsafeIORandom = (unsafePerformIO (getStdRandom (randomR (0, (length cartes)-1))))
      delete :: Int -> [Carta] -> [Carta]
      delete n c = (fst $ splitAt n c) ++ (tail $ snd $ splitAt n c)


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
  | not (nombreJugadorsCorrecte tirador) = error "Només hi ha 4 jugadors (1-4)"
  | not (nombreCorrecteDeCartes (carta:xs)) = error "El nombre de cartes tirades ha de ser múltiples de 4"
  | (length (carta:xs)) == 4  = if mod _guanyador 2 /= 0 then (fst _cas_base ++ _cartes_guanyades, snd _cas_base) else (fst _cas_base, snd _cas_base ++ _cartes_guanyades)
  | mod (length (carta:xs)) 4 == 0 = cartesGuanyades trumfu (drop 4 (carta:xs)) _guanyador
  | otherwise = error "Error desconegut"
  where
    _cas_base = (cartesGuanyades trumfu [] 0)
    _guanyador = jugadorGuanyaBasa _baseActual trumfu
    _baseActual = (NewB (tirador, carta, xs !! 0, xs !! 1, xs !! 2))
    _cartes_guanyades = cartesBasa (NewB (_guanyador, carta, xs !! 0, xs !! 1, xs !! 2))

nombreCorrecteDeCartes :: [Carta] -> Bool
nombreCorrecteDeCartes (carta:xs) = (mod (length (carta:xs)) 4) == 0

nombreJugadorsCorrecte :: Integer -> Bool
nombreJugadorsCorrecte jugadors = jugadors >= 1 && jugadors <= 4

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

--jugadesPossibles:: Ma->Trumfu->Basa->[Carta]
--jugadesPossibles (NewM llista) t b

filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal :: [Carta] -> Carta -> [Carta]
filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal l c
  | (filter (>c) l == []) = l
  | otherwise = (filter (>c) l)

filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes :: [Carta] -> Carta -> Trumfu -> [Carta]
filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes l c t
  | ([x | x<-l, (trumfu2Pal t == (getPal x))] == []) = l
  | otherwise = [x | x<-l, (trumfu2Pal t == (getPal x))]

-- Donat un enter (mida llista 2 o 3), una llista de cartes i el trumfu retorna el jugador segons ordre de tirada que esta guanyant
quiEstaGuanyant :: Integer -> [Carta] -> Trumfu -> Integer
quiEstaGuanyant m (x:xs) t
  | (m == 2) && (getPal x == getPal (head xs)) = if x > head xs then 1 else 2
  | (m == 2) && (getPal x /= getPal (head xs)) = if getPal x == trumfu2Pal t then 1
                                                 else if getPal (head xs) == trumfu2Pal t then 2
                                                 else 1
  | (m == 3) && (quiEstaGuanyant 2 (take 2 (x:xs)) t == 1) = if quiEstaGuanyant 2 (x:(tail (xs))) t == 1 then 1 else 3
  | (m == 3) && (quiEstaGuanyant 2 (take 2 (x:xs)) t == 2) = if quiEstaGuanyant 2 xs t == 1 then 2 else 3


-- Ma: ma del jugador  -- Trumfu: pal dominant de la partida -- [Carta] cartes de la base pot ser buida -- Return [Carta] possibles cartes
-- Si cartes basa buida podem tirar qualsevol
-- Si cartes basa size 1 (obligats a matar si podem):
        -- Si tenim del mateix pal i podem matar qualsevol que pugui matar
        -- si fallem tirem qualsevol trumfu si tenim
        -- si tampoc tenim trumfu tirem qualsevol
-- Si cartes basa size 2 hem de mirar si el company [0] esta guanyant o no:
        -- Si company esta guanyant podem tirar qualsevol carta del mateix pal que a tirat i si fallem qualsevol altre
        -- altrament
            -- si [1] no ha fallat hem de matar si podem amb cartes del mateix pal sino hem de tirar trumfu si tenim sino qualsevol
            -- si [1] ha fallat (amb trumfu) i tenim encara cartes del pal iniciador podem tirar qualsevol del pal
            -- Si [1] ha fallat (amb trumfu) i fallem tambe hem de matar amb trumfu si podem altrament qualsevol

-- Si cartes base size 3 (el nostre company es [1]):
        -- si el company [1] esta guanyant podem tirar qualsevol carta del mateix pal que a tirat i si fallem qualsevol altre
        -- Altrament:
        -- mirem quina carta esta guanyant(Hem de matar si podem):
                -- Si esta guanyant una carta del mateix pal que ha tirat el company:
                    -- si tenim d'aquell pal i podem matar nomes podem tirar aquestes altrament si fallem trumfu si tenim altrament qualsevol
                    -- si esta guanyant un fallo (de trumfu):
                    -- si tenim del pal de la base qualsevol del pal
                    -- Si fallem i tenim un trumfu que pugui matar qualsevol trumfu que pugui matar
                    -- altrament qualsevol carta
realJugadesPossibles :: Ma -> Trumfu -> [Carta] -> [Carta]
realJugadesPossibles (NewM llista) _ [] = [x | x<-llista] -- Si no s'ha tirat cap carta començem nosaltres i podem tirar qualsevol carta
realJugadesPossibles (NewM llista) t (x:xs)
  | (length xs + 1 == 1) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] x t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) x
  | (length xs + 1 == 2) && (quiEstaGuanyant 2 (x:xs) t == 2) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] (head xs) t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) (head xs)
  | (length xs + 1 == 2) && (quiEstaGuanyant 2 (x:xs) t == 1) = if cartesPalMa (NewM llista) (getPal x) == [] then [c | c<-llista] else cartesPalMa (NewM llista) (getPal x)
  | (length xs + 1 == 3) && (quiEstaGuanyant 3 (x:xs) t == 1) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] x t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) x
  | (length xs + 1 == 3) && (quiEstaGuanyant 3 (x:xs) t == 3) = if cartesPalMa (NewM llista) (getPal x) == [] then filtrarGuanyadoresFallantMirantSiTenimTrunfosSinoRetornaTotes [c | c<-llista] (last xs) t else filtrarGuanyadorasNoFallantSiNoPodemTotesLesDelPal (cartesPalMa (NewM llista) (getPal x)) (last xs)
  | otherwise = if cartesPalMa (NewM llista) (getPal x) == [] then [c | c<-llista] else cartesPalMa (NewM llista) (getPal x)

-- Donada les mans dels jugadors, el trumfu de la partida, les cartes jugades a la partida i el jugador que ha començat
-- retorna una tupla amb la base, numero de base i jugador que ha fet trampa

--baseEsTrampa ::

--trampa :: [Ma] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
